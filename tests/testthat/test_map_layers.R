library(testthat)

# Map layers: test positivity, admissions and deaths, on fixed colour steps

pop_tbl <- data.frame(borough_name = c("A", "B", "C"), pop = c(100000, 200000, 100000))

test_that("each layer gives one value per map area, in the map's order", {
  results <- data.frame(borough_name = c("B", "A", "B"), n = c(100, 50, 100), positives = c(4, 0, 6))
  reports <- data.frame(
    turn = c(1L, 1L, 1L, 2L, 2L, 2L),
    borough_index = c(1L, 2L, 3L, 1L, 2L, 3L),
    admissions = c(9L, 9L, 9L, 0L, 4L, 1L),
    deaths = c(0L, 0L, 0L, 0L, 0L, 2L)
  )
  # map areas listed in a different order from the population table
  layers <- map_layers(results, reports, pop_tbl, area_names = c("C", "B", "A"))

  expect_equal(layers$positivity$values, c(NA, 0.05, 0))
  # only the latest report counts, per 100,000 people
  expect_equal(layers$admissions$counts, c(1, 4, 0))
  expect_equal(layers$admissions$values, c(1, 2, NA))
  expect_equal(layers$deaths$values, c(2, NA, NA))
})

test_that("colours follow fixed steps: worse is always deeper, none is slate", {
  breaks <- map_layers(NULL, NULL, pop_tbl, c("A", "B", "C"))$positivity$breaks
  cols <- map_colours(c(NA, 0, 0.004, 0.03, 0.5), breaks)
  expect_equal(cols[1], "#3d4f61")
  expect_equal(cols[2], map_palette[1])
  expect_equal(match(cols[-1], map_palette), sort(match(cols[-1], map_palette)))
  expect_equal(cols[5], map_palette[length(map_palette)])
})
