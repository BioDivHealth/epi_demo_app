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
  expect_equal(length(report$metrics), 14)
  expect_equal(nrow(report$turn_summary), length(truth_history))
  expect_equal(report$turn_summary$truth_total, c(5, 7, 6))
  expect_equal(report$turn_summary$imports_total, c(0, 1, 1))
  expect_equal(report$turn_summary$deaths_total, c(0, 1, 1))
  expect_equal(report$turn_summary$observed_tests[report$turn_summary$turn == 2], 8)
  expect_equal(report$turn_summary$active_interventions[2], "Social distancing")
})

make_truth_log_fixture <- function(history, deaths_by_turn, pop_tbl, branch_name) {
  logs <- init_turn_logs(pop_tbl)
  truth_log <- logs$truth

  cumulative <- integer(length(history[[1]]))
  cumulative_deaths <- integer(length(history[[1]]))

  for (idx in seq_along(history)) {
    turn <- as.integer(idx - 1L)
    incidence <- as.integer(history[[idx]])
    deaths <- as.integer(deaths_by_turn[[idx]])
    cumulative <- cumulative + incidence
    cumulative_deaths <- cumulative_deaths + deaths

    truth_log <- append_truth_log(
      truth_log = truth_log,
      turn = turn,
      incidence = incidence,
      cumulative = cumulative,
      deaths = deaths,
      cumulative_deaths = cumulative_deaths,
      imports = integer(length(incidence)),
      Rt = 1.2,
      pop_tbl = pop_tbl,
      branch = branch_name
    )
  }

  truth_log
}

make_branch_fixture <- function(name, history, deaths_by_turn, observed_history, total_spend, pop_tbl) {
  logs <- init_turn_logs(pop_tbl)
  truth_log <- make_truth_log_fixture(history, deaths_by_turn, pop_tbl, name)
  final_turn <- as.integer(length(history) - 1L)

  run_log <- tibble::tibble(
    turn = 0:final_turn,
    tests_ag = c(6L, 6L, 6L),
    tests_pcr = c(2L, 2L, 2L),
    allocation_spend = c(20, 20, 20),
    intervention_cost = c(0, 0, 0),
    total_turn_spend = c(20, 20, 20),
    cumulative_spend = c(20, 40, 60),
    observed_tests = c(5L, 5L, 5L),
    observed_positives = c(1L, 1L, 1L),
    reported_deaths = c(0L, 0L, 1L),
    active_interventions = c("None", "None", "None"),
    notes = c("", "", ""),
    queue_pending = c(0L, 0L, 0L)
  )

  new_branch_state(
    name = name,
    turn = final_turn,
    truth_state = list(
      turn = final_turn,
      incidence = as.integer(history[[length(history)]]),
      history = history
    ),
    truth_history = history,
    observation_state = list(
      queue = logs$observations[0, ],
      history = observed_history
    ),
    available_now = observed_history[0, ],
    run_log = run_log,
    turn_logs = list(
      truth = truth_log,
      observations = logs$observations,
      actions = logs$actions
    ),
    total_spend = total_spend,
    spend_current = list(
      total = total_spend,
      last_turn = 20,
      remaining = 0,
      within_budget = TRUE,
      interventions = "None"
    )
  )
}

test_that("dual branch report returns comparison metrics and aligned turns", {
  player_history <- list(
    c(5L, 0L, 0L),
    c(4L, 0L, 0L),
    c(3L, 0L, 0L)
  )
  baseline_history <- list(
    c(5L, 0L, 0L),
    c(7L, 0L, 0L),
    c(8L, 0L, 0L)
  )

  player_deaths <- list(c(0L, 0L, 0L), c(1L, 0L, 0L), c(0L, 0L, 0L))
  baseline_deaths <- list(c(0L, 0L, 0L), c(1L, 0L, 0L), c(2L, 0L, 0L))

  observed_stub <- tibble::tibble(
    borough_index = c(1L, 1L),
    borough_name = c("A", "A"),
    type = c("ag", "pcr"),
    n = c(10L, 10L),
    positives = c(2L, 1L),
    turn_sampled = c(0L, 1L),
    turn_available = c(0L, 1L)
  )

  player_branch <- make_branch_fixture(
    name = "player",
    history = player_history,
    deaths_by_turn = player_deaths,
    observed_history = observed_stub,
    total_spend = 300,
    pop_tbl = pop_stub
  )

  baseline_branch <- make_branch_fixture(
    name = "baseline",
    history = baseline_history,
    deaths_by_turn = baseline_deaths,
    observed_history = observed_stub,
    total_spend = 150,
    pop_tbl = pop_stub
  )

  dual <- build_dual_branch_report(player_branch, baseline_branch, pop_stub)

  expect_named(dual, c("player", "baseline", "comparison", "turn_comparison"))
  expect_equal(dual$comparison$cases_averted, 8)
  expect_equal(dual$comparison$deaths_averted, 2)
  expect_equal(dual$comparison$spend_delta, 150)
  expect_equal(dual$comparison$cost_per_case_averted, 18.75)
  expect_equal(dual$comparison$cost_per_death_averted, 75)
  expect_equal(nrow(dual$turn_comparison), length(player_history))

  reversed <- compute_comparison_metrics(dual$baseline, dual$player)
  expect_true(is.na(reversed$cost_per_case_averted))
  expect_true(is.na(reversed$cost_per_death_averted))
})
