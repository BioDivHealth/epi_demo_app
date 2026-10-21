library(testthat)

# Replay: what you saw against what really happened, on one clock -----------

data_dir <- testthat::test_path("..", "..", "data")
inputs <- load_inputs(data_dir = data_dir)

test_that("the replay shows nothing until the first report, then the player's reports", {
  config <- scenario_config("covid_early", inputs)
  state <- new_game(config, seed = 5)
  lines <- list("first report")
  for (i in 1:3) {
    state <- play_turn(state, list(), config)$state
    lines[[i + 1]] <- paste("report", i)
  }
  offset <- state$truth_offset

  # the unseen start: true infections, nothing seen
  first_week <- reveal_frame(state, 0, config, lines)
  expect_false(first_week$seen)
  expect_equal(first_week$infections_this_week, 1)      # one infected person
  expect_true(first_week$date < turn_date(0, config))

  # the first report
  report_week <- reveal_frame(state, offset, config, lines)
  expect_true(report_week$seen)
  expect_equal(report_week$turn, 0)
  expect_equal(report_week$report_lines, "first report")

  # the last week: truth so far is everyone ever infected
  last <- reveal_frame(state, offset + 3, config, lines)
  expect_equal(last$report_lines, "report 3")
  expect_equal(last$infections_so_far, sum(state$truth$cumulative))
  expect_true(all(last$reports$turn == 3))
})
