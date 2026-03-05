library(testthat)

if (!exists("replay_branch_names")) {
  source(testthat::test_path("..", "..", "R", "util_replay.R"), local = FALSE)
}

make_branch_payload <- function() {
  player_truth <- tibble::tibble(
    branch = "player",
    turn = c(0L, 0L),
    borough_index = c(1L, 2L),
    gss_code = c("G1", "G2"),
    borough_name = c("A", "B"),
    incidence = c(3L, 1L),
    imports = c(0L, 0L),
    Rt = c(1.2, 1.2)
  )

  baseline_truth <- tibble::tibble(
    branch = "baseline",
    turn = c(0L, 0L, 1L, 1L),
    borough_index = c(1L, 2L, 1L, 2L),
    gss_code = c("G1", "G2", "G1", "G2"),
    borough_name = c("A", "B", "A", "B"),
    incidence = c(2L, 1L, 2L, 2L),
    imports = c(0L, 0L, 0L, 0L),
    Rt = c(1.1, 1.1, 1.1, 1.1)
  )

  obs_player <- tibble::tibble(
    branch = "player",
    turn_reported = 1L,
    turn_sampled = 0L,
    turn_available = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_returned = 10L,
    positives = 3L,
    positivity = 0.3
  )

  obs_baseline <- tibble::tibble(
    branch = "baseline",
    turn_reported = 1L,
    turn_sampled = 0L,
    turn_available = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_returned = 8L,
    positives = 1L,
    positivity = 0.125
  )

  actions_player <- tibble::tibble(
    branch = "player",
    turn = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_allocated = 10L,
    spend = 20,
    cost_per_test = 2,
    allocation_spend = 20,
    intervention_cost = 0,
    total_turn_spend = 20,
    cumulative_spend = 20,
    budget_remaining = 1980,
    interventions_active = "None"
  )
  run_log_player <- tibble::tibble(
    turn = c(0L, 1L),
    active_interventions = c("None", "Social distancing"),
    intervention_cost = c(0, 50)
  )

  actions_baseline <- tibble::tibble(
    branch = "baseline",
    turn = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_allocated = 5L,
    spend = 10,
    cost_per_test = 2,
    allocation_spend = 10,
    intervention_cost = 0,
    total_turn_spend = 10,
    cumulative_spend = 10,
    budget_remaining = 1990,
    interventions_active = "None"
  )
  run_log_baseline <- tibble::tibble(
    turn = c(0L, 1L),
    active_interventions = c("None", "None"),
    intervention_cost = c(0, 0)
  )

  list(
    branches = list(
      player = list(
        truth = player_truth,
        observations = obs_player,
        actions = actions_player,
        run_log = run_log_player,
        truth_history = list(c(3L, 1L), c(4L, 2L))
      ),
      baseline = list(
        truth = baseline_truth,
        observations = obs_baseline,
        actions = actions_baseline,
        run_log = run_log_baseline,
        truth_history = list(c(2L, 1L), c(2L, 2L))
      )
    )
  )
}

make_legacy_payload <- function() {
  list(
    truth = tibble::tibble(
      turn = c(0L, 0L),
      borough_index = c(1L, 2L),
      gss_code = c("G1", "G2"),
      borough_name = c("A", "B"),
      incidence = c(1L, 0L),
      imports = c(0L, 0L),
      Rt = c(1.2, 1.2)
    ),
    observations = tibble::tibble(
      turn_reported = 1L,
      turn_sampled = 0L,
      turn_available = 1L,
      borough_index = 1L,
      gss_code = "G1",
      borough_name = "A",
      test_type = "ag",
      n_returned = 10L,
      positives = 2L,
      positivity = 0.2
    ),
    actions = tibble::tibble(
      turn = 1L,
      borough_index = 1L,
      gss_code = "G1",
      borough_name = "A",
      test_type = "ag",
      n_allocated = 10L,
      spend = 20,
      cost_per_test = 2,
      allocation_spend = 20,
      intervention_cost = 0,
      total_turn_spend = 20,
      cumulative_spend = 20,
      budget_remaining = 1980,
      interventions_active = "None"
    ),
    truth_history = list(c(1L, 0L), c(2L, 1L))
  )
}

test_that("replay branch names detect branch payload and legacy payload", {
  expect_equal(replay_branch_names(make_branch_payload()), c("player", "baseline"))
  expect_equal(replay_branch_names(make_legacy_payload()), c("player"))
})

test_that("replay_get_branch_logs returns selected branch and falls back for missing branch", {
  payload <- make_branch_payload()

  baseline_logs <- replay_get_branch_logs(payload, branch = "baseline")
  expect_equal(nrow(baseline_logs$truth), 4)
  expect_equal(sum(baseline_logs$truth$incidence), 7)

  fallback_logs <- replay_get_branch_logs(payload, branch = "nonexistent")
  expect_equal(nrow(fallback_logs$truth), 2)
  expect_equal(sum(fallback_logs$truth$incidence), 4)

  legacy_logs <- replay_get_branch_logs(make_legacy_payload(), branch = "player")
  expect_true(is.list(legacy_logs$truth_history))
  expect_equal(nrow(legacy_logs$observations), 1)
  expect_true("run_log" %in% names(baseline_logs))
  expect_equal(nrow(baseline_logs$run_log), 2)
  expect_equal(nrow(legacy_logs$run_log), 0)
})

test_that("replay_filter_branch filters when branch column exists and no-ops otherwise", {
  with_branch <- tibble::tibble(branch = c("player", "baseline"), value = c(1, 2))
  without_branch <- tibble::tibble(value = c(1, 2))

  filtered <- replay_filter_branch(with_branch, "player")
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$value[[1]], 1)

  unfiltered <- replay_filter_branch(without_branch, "baseline")
  expect_equal(nrow(unfiltered), 2)
})

test_that("replay_resolve_truth_turns and replay_resolve_truth_log are branch-specific", {
  payload <- make_branch_payload()
  borough_names <- c("A", "B")

  turns_player <- replay_resolve_truth_turns(payload, branch = "player")
  turns_baseline <- replay_resolve_truth_turns(payload, branch = "baseline")

  expect_equal(turns_player, c(0L, 1L))
  expect_equal(turns_baseline, c(0L, 1L))

  truth_player <- replay_resolve_truth_log(payload, borough_names, branch = "player")
  truth_baseline <- replay_resolve_truth_log(payload, borough_names, branch = "baseline")

  expect_equal(sum(truth_player$incidence), 10)
  expect_equal(sum(truth_baseline$incidence), 7)
})

test_that("replay observation/action summaries respect branch selection", {
  payload <- make_branch_payload()
  merged_obs <- dplyr::bind_rows(
    payload$branches$player$observations,
    payload$branches$baseline$observations
  )
  merged_actions <- dplyr::bind_rows(
    payload$branches$player$actions,
    payload$branches$baseline$actions
  )

  obs_player <- replay_summarise_observed(merged_obs, target_turn = 1L, branch = "player")
  obs_baseline <- replay_summarise_observed(merged_obs, target_turn = 1L, branch = "baseline")

  expect_equal(sum(obs_player$tests), 10)
  expect_equal(sum(obs_baseline$tests), 8)

  actions_player <- replay_summarise_actions(merged_actions, target_turn = 1L, branch = "player")
  actions_baseline <- replay_summarise_actions(merged_actions, target_turn = 1L, branch = "baseline")

  expect_equal(sum(actions_player$tests_ag), 10)
  expect_equal(sum(actions_baseline$tests_ag), 5)
})

test_that("intervention timeline builds full turn grid from run log labels", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs")
  )
  run_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    active_interventions = c("None", "Social distancing, Targeted quarantine", "Rapid labs")
  )

  timeline <- replay_build_intervention_timeline(
    run_log = run_log,
    turns = 0:2,
    intervention_defs = defs
  )

  expect_equal(nrow(timeline), 3L * nrow(defs))
  active_t1 <- timeline |>
    dplyr::filter(turn == 1L, active) |>
    dplyr::pull(intervention_label) |>
    sort()
  expect_equal(active_t1, sort(c("Social distancing", "Targeted quarantine")))
})

test_that("intervention timeline falls back to action log labels when run log missing", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 0L, 1L),
    interventions_active = c("None", "None", "Rapid labs")
  )

  timeline <- replay_build_intervention_timeline(
    run_log = tibble::tibble(),
    turns = 0:1,
    intervention_defs = defs,
    action_log = action_log
  )

  rapid_active_t1 <- timeline |>
    dplyr::filter(turn == 1L, intervention_id == "rapid") |>
    dplyr::pull(active)
  expect_true(isTRUE(rapid_active_t1))
})

test_that("intervention timeline plot returns ggplot and includes fallback-derived active state", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 1L),
    interventions_active = c("None", "Rapid labs")
  )

  plot_obj <- replay_plot_intervention_timeline(
    run_log = tibble::tibble(),
    action_log = action_log,
    intervention_defs = defs,
    turns = 0:1
  )

  expect_s3_class(plot_obj, "ggplot")
  rapid_state <- plot_obj$data |>
    dplyr::filter(turn == 1L, intervention_id == "rapid") |>
    dplyr::pull(active)
  expect_true(isTRUE(rapid_state))
  expect_equal(sort(unique(plot_obj$data$strip_state)), c("Active", "Inactive"))
})

test_that("intervention timeline plot returns NULL when no turns are available", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine"),
    label = c("Social distancing", "Targeted quarantine")
  )

  expect_null(replay_plot_intervention_timeline(
    run_log = tibble::tibble(),
    action_log = tibble::tibble(),
    intervention_defs = defs,
    turns = integer()
  ))
})

test_that("prevalence plot no longer carries intervention fill scale", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine"),
    label = c("Social distancing", "Targeted quarantine")
  )
  truth_log <- tibble::tibble(
    turn = c(0L, 1L),
    borough_index = c(1L, 1L),
    incidence = c(4L, 6L)
  )
  baseline_truth_log <- tibble::tibble(
    turn = c(0L, 1L),
    borough_index = c(1L, 1L),
    incidence = c(3L, 5L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(0L, 1L),
    n_returned = c(20L, 20L),
    positives = c(1L, 2L)
  )
  run_log <- tibble::tibble(
    turn = c(0L, 1L),
    active_interventions = c("None", "Social distancing")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 1L),
    interventions_active = c("None", "Social distancing")
  )

  plot_obj <- replay_plot_prevalence(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000),
    window = 2L,
    baseline_truth_log = baseline_truth_log,
    player_run_log = run_log,
    player_action_log = action_log,
    intervention_defs = defs
  )

  expect_s3_class(plot_obj, "ggplot")
  expect_null(plot_obj$scales$get_scales("fill"))
  expect_equal(plot_obj$labels$y, "Prevalence (%)")
})
