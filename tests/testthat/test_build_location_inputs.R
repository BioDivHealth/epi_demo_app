library(testthat)

# Small synthetic map: four 10 km squares in a row (west to east),
# built in a planar CRS so distances are easy to reason about
make_row_of_squares <- function(n = 4, size_m = 10000) {
  squares <- lapply(seq_len(n), function(i) {
    x0 <- (i - 1) * size_m
    corners <- rbind(c(x0, 0), c(x0 + size_m, 0), c(x0 + size_m, size_m), c(x0, size_m), c(x0, 0))
    sf::st_polygon(list(corners))
  })
  sf::st_sf(
    name = paste0("unit_", seq_len(n)),
    geometry = sf::st_sfc(squares, crs = 3857)
  )
}

test_that("bundle has the load_inputs() shape for any number of units", {
  units <- make_row_of_squares(4)
  inputs <- build_location_inputs(units, pop = rep(1000, 4), name_col = "name", planar_crs = 3857)

  expect_named(inputs, c("boros", "boros_planar", "nb", "A", "K", "K_measles", "pop_tbl"))
  expect_s3_class(inputs$boros, "sf")
  expect_equal(sf::st_crs(inputs$boros)$epsg, 4326)
  expect_true(all(c("gss_code", "borough_name", "pop") %in% names(inputs$pop_tbl)))
  expect_length(inputs$nb, 4)
})

test_that("K is square and rows sum to 1", {
  units <- make_row_of_squares(4)
  inputs <- build_location_inputs(units, pop = c(100, 200, 300, 400), name_col = "name", planar_crs = 3857)

  expect_true(is.matrix(inputs$K))
  expect_equal(dim(inputs$K), c(4L, 4L))
  expect_equal(dim(inputs$A), dim(inputs$K))
  expect_equal(unname(rowSums(inputs$K)), rep(1, 4), tolerance = 1e-9)
  expect_true(all(inputs$K > 0))
})

test_that("population is aligned to units even if the table is shuffled", {
  units <- make_row_of_squares(4)
  pop_shuffled <- data.frame(name = c("unit_3", "unit_1", "unit_4", "unit_2"),
                             pop = c(300, 100, 400, 200))
  inputs <- build_location_inputs(units, pop = pop_shuffled, name_col = "name", planar_crs = 3857)

  expect_equal(inputs$pop_tbl$borough_name, units$name)
  expect_equal(inputs$pop_tbl$borough_name, inputs$boros$borough_name)
  expect_equal(inputs$pop_tbl$pop, c(100, 200, 300, 400))
  expect_equal(rownames(inputs$K), units$name)
})

test_that("missing or bad population is an error", {
  units <- make_row_of_squares(4)
  pop_short <- data.frame(name = c("unit_1", "unit_2"), pop = c(1, 2))

  expect_error(build_location_inputs(units, pop = pop_short, name_col = "name", planar_crs = 3857),
               "Population missing")
  expect_error(build_location_inputs(units, pop = c(1, 2, 0, 4), name_col = "name", planar_crs = 3857),
               "positive")
})

test_that("closer units get more weight (equal populations)", {
  units <- make_row_of_squares(4)
  inputs <- build_location_inputs(units, pop = rep(1000, 4), name_col = "name", planar_crs = 3857)
  K <- inputs$K

  # From unit 1: neighbour > two away > three away
  expect_gt(K[1, 2], K[1, 3])
  expect_gt(K[1, 3], K[1, 4])
})

test_that("bigger units get more weight (equal distances)", {
  units <- make_row_of_squares(3)
  # unit_2 sits in the middle; units 1 and 3 are equally far from it
  inputs <- build_location_inputs(units, pop = c(1000, 1000, 5000), name_col = "name", planar_crs = 3857)
  K <- inputs$K

  expect_gt(K[2, 3], K[2, 1])
})

test_that("alpha and beta change the kernel in the expected direction", {
  units <- make_row_of_squares(4)
  steep <- build_location_inputs(units, pop = rep(1000, 4), name_col = "name",
                                 gravity = list(alpha = 1, beta = 3), planar_crs = 3857)
  flat <- build_location_inputs(units, pop = rep(1000, 4), name_col = "name",
                                gravity = list(alpha = 1, beta = 1), planar_crs = 3857)

  # Steeper distance decay keeps less weight on the far unit
  expect_lt(steep$K[1, 4], flat$K[1, 4])
})

test_that("London boroughs rebuild to the same adjacency as data/", {
  data_dir <- testthat::test_path("..", "..", "data")
  london <- readRDS(file.path(data_dir, "boroughs.rds"))
  A_saved <- readRDS(file.path(data_dir, "adjacency_matrix.rds"))

  inputs <- build_location_inputs(london, pop = london$pop, name_col = "borough_name",
                                  code_col = "gss_code", planar_crs = 27700)

  expect_equal(nrow(inputs$K), 33)
  expect_equal(unname(inputs$A), unname(A_saved))
  expect_equal(unname(rowSums(inputs$K)), rep(1, 33), tolerance = 1e-9)
})

test_that("save_location_inputs refuses to write into data/", {
  units <- make_row_of_squares(2)
  inputs <- build_location_inputs(units, pop = c(1, 2), name_col = "name", planar_crs = 3857)

  withr::with_dir(testthat::test_path("..", ".."), {
    expect_error(save_location_inputs(inputs, "data"), "Refusing")
  })

  out_dir <- withr::local_tempdir()
  save_location_inputs(inputs, out_dir)
  expect_true(file.exists(file.path(out_dir, "kernel_K.rds")))
  expect_true(file.exists(file.path(out_dir, "pop_boroughs.rds")))
})
