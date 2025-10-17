library(testthat)

pop_stub <- tibble::tibble(
  borough_name = c("A", "B", "C"),
  pop = c(1000, 1000, 1000)
)

truth_history <- list(
  c(5L, 0L, 0L),
  c(6L, 1L, 0L),
  c(4L, 2L, 0L)
)

observed_history <- tibble::tibble(
  borough_index = c(1, 1),
  borough_name = c("A", "A"),
  type = c("pcr", "ag"),
  n = c(10L, 8L),
  positives = c(3L, 2L),
  turn_sampled = c(0L, 1L),
  turn_available = c(1L, 2L)
)

run_log <- tibble::tibble(
  turn = c(0L, 1L, 2L),
  tests_ag = c(10L, 5L, 5L),
  tests_pcr = c(0L, 20L, 10L),
  allocation_spend = c(0, 100, 80),
  intervention_cost = c(0, 30, 0),
  total_turn_spend = c(0, 130, 80),
  cumulative_spend = c(0, 130, 210),
  observed_tests = c(0L, 10L, 8L),
  observed_positives = c(0L, 3L, 2L),
  active_interventions = c("None", "Social distancing", "None"),
  notes = c("Starter intel: targeted alert", "", ""),
  queue_pending = c(0L, 0L, 2L)
)

report <- build_run_report(
  truth_history = truth_history,
  observed_history = observed_history,
  pop_tbl = pop_stub,
  total_spend = 210,
  run_log = run_log
)

test_that("run report carries metrics and summaries", {
  expect_equal(report$metrics$total_spend, 210)
  expect_equal(length(report$metrics), 6)
  expect_equal(nrow(report$turn_summary), length(truth_history))
  expect_equal(report$turn_summary$truth_total, c(5, 7, 6))
  expect_equal(report$turn_summary$observed_tests[report$turn_summary$turn == 2], 8)
  expect_equal(report$turn_summary$active_interventions[2], "Social distancing")
})
