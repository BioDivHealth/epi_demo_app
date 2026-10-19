library(testthat)

# Grid of 0.1-degree squares near the equator, with populations
make_grid <- function(n_x = 5, n_y = 5, x0 = 0, y0 = 0, size = 0.1) {
  cells <- list()
  for (j in seq_len(n_y)) {
    for (i in seq_len(n_x)) {
      x <- x0 + (i - 1) * size
      y <- y0 + (j - 1) * size
      corners <- rbind(c(x, y), c(x + size, y), c(x + size, y + size), c(x, y + size), c(x, y))
      cells[[length(cells) + 1]] <- sf::st_polygon(list(corners))
    }
  }
  sf::st_sf(
    shapeName = paste0("cell_", seq_along(cells)),
    pop = 1000,
    geometry = sf::st_sfc(cells, crs = 4326)
  )
}

test_that("name_key ignores case, accents and punctuation", {
  expect_equal(name_key("São Paulo"), "sao paulo")
  expect_equal(name_key("  NEW-YORK "), "new york")
})

test_that("find_place ranks exact matches, then countries, then bigger places", {
  index <- data.frame(
    id = c("country_GEO", "city_tbilisi", "city_georgetown", "city_hyd_in", "city_hyd_pk"),
    label = c("Georgia (country)", "Tbilisi (city, Georgia)", "Georgetown (city, Guyana)",
              "Hyderabad (city, India)", "Hyderabad (city, Pakistan)"),
    type = c("country", "city", "city", "city", "city"),
    search_names = c("Georgia|GEO", "Tbilisi", "Georgetown", "Hyderabad", "Hyderabad"),
    pop_total = c(3.7e6, 1.1e6, 0.2e6, 9e6, 2e6)
  )

  # Exact name match
  georgia <- find_place("georgia", index)
  expect_equal(georgia$id, "country_GEO")

  # Partial text: both start with it, country listed first
  georg <- find_place("georg", index)
  expect_equal(georg$id, c("country_GEO", "city_georgetown"))

  # Same name in two countries: both offered, bigger first
  hyderabad <- find_place("HYDERABAD", index)
  expect_equal(hyderabad$id, c("city_hyd_in", "city_hyd_pk"))

  # No match and empty text give zero rows
  expect_equal(nrow(find_place("atlantis", index)), 0)
  expect_equal(nrow(find_place("   ", index)), 0)
})

test_that("pick_city_units takes the units near the city point", {
  grid <- make_grid(5, 5)

  # City in the middle cell (0.25, 0.25). Cells are ~11 km: a 23 km radius covers
  # the middle, 4 sides, 4 diagonals and the 4 cells two steps away = 13 areas
  in_play <- pick_city_units(grid, city_lon = 0.25, city_lat = 0.25, radius_km = 23)

  expect_equal(grid$shapeName[in_play[1]], "cell_13")   # containing cell first
  expect_length(in_play, 13)
  expect_false(attr(in_play, "widened"))
})

test_that("pick_city_units widens to at least 10 areas and caps at a maximum", {
  grid <- make_grid(5, 5)

  # 12 km radius only reaches 5 cells, so widen to the 10 nearest and flag it
  few <- pick_city_units(grid, 0.25, 0.25, radius_km = 12)
  expect_length(few, 10)
  expect_true(attr(few, "widened"))

  # Huge radius: capped at n_max nearest
  many <- pick_city_units(grid, 0.25, 0.25, radius_km = 1000, n_max = 15)
  expect_length(many, 15)
})

test_that("keep_mainland drops a far island group but keeps the main cluster", {
  mainland <- make_grid(3, 3)
  islands <- make_grid(2, 1, x0 = 20, y0 = 0)   # ~2200 km east, two touching islands
  islands$shapeName <- c("island_1", "island_2")
  units <- rbind(mainland, islands)

  keep <- keep_mainland(units)

  expect_true(all(keep[1:9]))
  expect_false(any(keep[10:11]))
})

test_that("a prebuilt place loads as a valid game bundle (if the library exists)", {
  library_dir <- testthat::test_path("..", "..", "locations", "library")
  skip_if_not(file.exists(file.path(library_dir, "index.rds")), "place library not built")

  index <- read_place_index(library_dir)
  place <- load_place(index$id[1], index, library_dir = library_dir)
  K <- place$inputs$K

  expect_equal(nrow(K), ncol(K))
  expect_equal(nrow(K), nrow(place$inputs$pop_tbl))
  expect_equal(unname(rowSums(K)), rep(1, nrow(K)), tolerance = 1e-6)
  expect_equal(place$inputs$pop_tbl$borough_name, place$inputs$boros$borough_name)
})

test_that("restricted boundary licences are dropped and others recorded", {
  index <- data.frame(
    label = c("A (country)", "B (city)", "C (country)"),
    iso3 = c("AAA", "BBB", "CCC"),
    level = c("ADM1", "ADM2", "ADM1")
  )
  meta_csv <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    boundaryISO = c("AAA", "BBB"),
    boundaryType = c("ADM1", "ADM2"),
    boundaryLicense = c("Open Data Commons Open Database License 1.0", "Other - Direct Permission"),
    licenseSource = c("x", "y")
  ), meta_csv, row.names = FALSE)

  out <- suppressMessages(add_boundary_licenses(index, meta_csv))

  expect_equal(out$label, c("A (country)", "C (country)"))
  expect_match(out$boundary_license[1], "Open Database")
  expect_match(out$boundary_license[2], "Unknown")
})

test_that("load_place rebuilds K from stored centroids (alpha/beta adjustable)", {
  grid <- make_grid(3, 1)
  inputs <- build_location_inputs(grid, pop = c(100, 1000, 100), name_col = "shapeName")
  K_built <- inputs$K

  # Store the place the way the library does: no K, no planar copy
  inputs$K <- NULL
  inputs$boros_planar <- NULL
  library_dir <- withr::local_tempdir()
  saveRDS(list(meta = data.frame(id = "test"), inputs = inputs, context = grid[0, ]),
          file.path(library_dir, "test.rds"))
  index <- data.frame(id = "test", file = "test.rds", use_builtin = FALSE)

  place <- load_place("test", index, library_dir = library_dir)
  expect_equal(place$inputs$K, K_built, tolerance = 1e-9)
  expect_s3_class(place$inputs$boros_planar, "sf")

  # Steeper distance decay keeps more infection at home
  steep <- load_place("test", index, library_dir = library_dir, gravity = list(alpha = 1, beta = 4))
  expect_gt(steep$inputs$K[1, 1], place$inputs$K[1, 1])
})
