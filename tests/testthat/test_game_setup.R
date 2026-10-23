library(testthat)

# Game codes ------------------------------------------------------------------

index_stub <- tibble::tibble(id = c("london", "kenya", "lagos", "lima"))

test_that("the same game code always gives the same disease, place and date", {
  a <- draw_game_setup(482731L, index_stub)
  b <- draw_game_setup(482731L, index_stub)
  expect_identical(a, b)
  expect_true(a$scenario_id %in% setdiff(scenario_catalog()$id, "custom"))
  expect_true(a$place_id %in% index_stub$id)
  expect_equal(format(a$start_date, "%Y"), "2027")
})

test_that("different codes give a spread of diseases, places and dates", {
  setups <- lapply(100001:100200, draw_game_setup, place_index = index_stub)
  expect_gt(length(unique(vapply(setups, `[[`, "", "scenario_id"))), 3)
  expect_gt(length(unique(vapply(setups, `[[`, "", "place_id"))), 2)
  expect_gt(length(unique(vapply(setups, function(s) as.character(s$start_date), ""))), 50)
})

test_that("a chosen place overrides the code's place, and codes are validated", {
  expect_equal(draw_game_setup(482731L, index_stub, place_override = "lima")$place_id, "lima")
  expect_equal(parse_game_code("482731"), 482731L)
  expect_true(is.na(parse_game_code("12")))
  expect_true(is.na(parse_game_code("abc")))
})

test_that("drawing a game setup does not disturb the outbreak's random numbers", {
  set.seed(1)
  before <- runif(3)
  set.seed(1)
  draw_game_setup(555555L, index_stub)
  after <- runif(3)
  expect_equal(before, after)
})
