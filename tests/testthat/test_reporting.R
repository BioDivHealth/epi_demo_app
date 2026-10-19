library(testthat)

pop_stub <- tibble::tibble(
  borough_name = c("A", "B", "C"),
  gss_code = c("G1", "G2", "G3"),
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
  reported_deaths = c(0L, 1L, 0L),
  active_interventions = c("None", "Social distancing", "None"),
  notes = c("Starter intel: targeted alert", "", ""),
  queue_pending = c(0L, 0L, 2L)
)

truth_log <- tibble::tibble(
  turn = rep(0:2, each = 3),
  borough_index = rep(1:3, times = 3),
  gss_code = rep(pop_stub$gss_code, times = 3),
  borough_name = rep(pop_stub$borough_name, times = 3),
  incidence = c(5L, 0L, 0L, 6L, 1L, 0L, 4L, 2L, 0L),
  cumulative = c(5L, 0L, 0L, 11L, 1L, 0L, 15L, 3L, 0L),
  deaths = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L),
  cumulative_deaths = c(0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 0L),
  imports = c(0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L),
  Rt = rep(1.2, 9)
)

report <- build_run_report(
  truth_history = truth_history,
  observed_history = observed_history,
  pop_tbl = pop_stub,
  total_spend = 210,
  run_log = run_log,
  truth_log = truth_log
)

test_that("run report carries metrics and summaries", {
  expect_equal(report$metrics$total_spend, 210)
  expect_true(all(c("time_to_detection", "real_cases_found", "false_positives",
                    "peak_true_incidence", "rt_observed_mean", "rt_truth_mean") %in% names(report$metrics)))
  expect_equal(nrow(report$turn_summary), length(truth_history))
  expect_equal(report$turn_summary$truth_total, c(5, 7, 6))
  expect_equal(report$turn_summary$imports_total, c(0, 1, 1))
  expect_equal(report$turn_summary$deaths_total, c(0, 1, 1))
  expect_equal(report$turn_summary$observed_tests[report$turn_summary$turn == 2], 8)
  expect_equal(report$turn_summary$active_interventions[2], "Social distancing")
})
