library(testthat)

empty_results_tbl <- function() {
  tibble::tibble(
    borough_index = integer(),
    borough_name = character(),
    type = character(),
    n = integer(),
    positives = integer(),
    turn_sampled = integer(),
    turn_available = integer()
  )
}

make_autorun_fixture <- function(
    run_mode = "guided_auto",
    config_turns = 4L,
    policy_id = "all_boroughs",
    budget = list(budget = 300, costs = list(ag = 2, pcr = 10)),
    active_ids = character()) {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2", "C3"),
    borough_name = c("Alpha", "Beta", "Gamma"),
    pop = c(900, 1300, 1100)
  )

  list(
    run_mode = run_mode,
    config_turns = as.integer(config_turns),
    policy_id = policy_id,
    budget = budget,
    active_ids = active_ids,
    intervention_defs = intervention_catalog(),
    params_epi = list(Rt = 1.2, k = 0.7, imports_prob = 0.01, lambda_import = 0.2),
    params_mort = list(death_ifr = 0.001, death_delay = 1L),
    testing_params = list(
      sens = list(ag = 0.8, pcr = 0.95),
      spec = list(ag = 0.995, pcr = 0.995),
      delays = list(ag = 0L, pcr = 1L),
      prev_floor = 1e-6,
      sampling_mode = "symptomatic",
      sampling_pool_frac = 0.1
    ),
    active_kernel = matrix(
      c(
        0.8, 0.1, 0.1,
        0.2, 0.6, 0.2,
        0.1, 0.2, 0.7
      ),
      nrow = 3,
      byrow = TRUE
    ),
    pop_tbl = pop_tbl,
    effective_pop_tbl = pop_tbl,
    allocs_manual = tibble::tibble(
      borough_index = 1:3,
      borough_name = pop_tbl$borough_name,
      n_ag = c(8L, 4L, 2L),
      n_pcr = c(2L, 1L, 1L)
    ),
    turn = 0L,
    total_spend = 0,
    truth_state = init_truth(n_boroughs = 3, seed_boroughs = 1L, seed_cases = 20L),
    observation_state = init_observation_state(),
    available_now = empty_results_tbl(),
    intervention_state = init_intervention_state(),
    run_log = tibble::tibble(),
    turn_logs = init_turn_logs(pop_tbl),
    auto_running = FALSE,
    auto_turns_requested = 0L,
    auto_turns_completed = 0L,
    auto_stop_reason = NULL
  )
}

start_autorun_fixture <- function(state) {
  if (!identical(state$run_mode, "guided_auto")) {
    return(state)
  }

  config_turns <- as.integer(state$config_turns)
  if (is.na(config_turns) || config_turns < 1L) {
    config_turns <- 1L
  }
  config_turns <- min(50L, config_turns)

  should_reset <- state$auto_turns_requested <= 0L ||
    state$auto_turns_completed >= state$auto_turns_requested ||
    identical(state$auto_stop_reason, "user_stop") ||
    identical(state$auto_stop_reason, "completed") ||
    config_turns != state$auto_turns_requested

  if (should_reset) {
    state$auto_turns_requested <- config_turns
    state$auto_turns_completed <- 0L
  }

  state$auto_stop_reason <- NULL
  state$auto_running <- TRUE
  state
}

run_auto_iteration_fixture <- function(state) {
  intervention_cost <- compute_intervention_effects(
    selected = state$active_ids,
    catalog = state$intervention_defs,
    state = state$intervention_state,
    observation_history = state$observation_state$history,
    current_turn = state$turn,
    turn_budget = state$budget$budget
  )$cost
  policy_request <- list(
    policy_id = state$policy_id,
    allocs_manual = state$allocs_manual,
    pop_tbl = state$effective_pop_tbl,
    budget = state$budget,
    intervention_cost = intervention_cost,
    current_turn = state$turn,
    observation_state = state$observation_state,
    available_now = state$available_now
  )
  allocs_policy <- build_policy_allocations(policy_request)

  request <- list(
    current_turn = state$turn,
    allocs = allocs_policy,
    budget = state$budget,
    params_epi = state$params_epi,
    params_mort = state$params_mort,
    testing_params = state$testing_params,
    active_ids = state$active_ids,
    intervention_defs = state$intervention_defs,
    intervention_state = state$intervention_state,
    total_spend = state$total_spend,
    truth_state = state$truth_state,
    observation_state = state$observation_state,
    active_kernel = state$active_kernel,
    effective_pop_tbl = state$effective_pop_tbl,
    pop_tbl = state$pop_tbl,
    run_log = state$run_log,
    turn_logs = state$turn_logs,
    branch = "player"
  )

  result <- run_single_turn(request)
  if (identical(result$status, "budget_exceeded")) {
    state$auto_running <- FALSE
    state$auto_stop_reason <- "budget_exceeded"
    return(state)
  }

  state$total_spend <- result$total_spend
  state$truth_state <- result$truth_state
  state$observation_state <- result$observation_state
  state$available_now <- result$available_now
  state$intervention_state <- result$intervention_state
  state$turn <- result$next_turn
  state$run_log <- result$run_log
  state$turn_logs <- result$turn_logs

  state$auto_turns_completed <- state$auto_turns_completed + 1L
  if (state$auto_turns_completed >= state$auto_turns_requested) {
    state$auto_running <- FALSE
    state$auto_stop_reason <- "completed"
  }

  state
}

tick_autorun_fixture <- function(state) {
  if (!isTRUE(state$auto_running)) {
    return(state)
  }

  if (state$auto_turns_requested <= 0L || state$auto_turns_completed >= state$auto_turns_requested) {
    state$auto_running <- FALSE
    state$auto_stop_reason <- "completed"
    return(state)
  }

  run_auto_iteration_fixture(state)
}

derive_turn_seed_fixture <- function(master_seed, turn_index) {
  as.integer((as.integer(master_seed) * 1009L + as.integer(turn_index) * 9176L) %% .Machine$integer.max)
}

test_that("auto-run completes exactly N turns", {
  state <- make_autorun_fixture(config_turns = 4L, policy_id = "all_boroughs")
  state <- start_autorun_fixture(state)

  set.seed(777)
  while (isTRUE(state$auto_running)) {
    state <- tick_autorun_fixture(state)
  }

  expect_equal(state$auto_turns_completed, 4L)
  expect_equal(state$turn, 4L)
  expect_equal(state$auto_stop_reason, "completed")
})

test_that("pause and resume preserve counters and continue correctly", {
  state <- make_autorun_fixture(config_turns = 5L, policy_id = "all_boroughs")
  state <- start_autorun_fixture(state)

  set.seed(101)
  for (i in seq_len(2)) {
    state <- tick_autorun_fixture(state)
  }

  state$auto_running <- FALSE
  completed_before_resume <- state$auto_turns_completed

  state <- start_autorun_fixture(state)
  while (isTRUE(state$auto_running)) {
    state <- tick_autorun_fixture(state)
  }

  expect_equal(completed_before_resume, 2L)
  expect_equal(state$auto_turns_completed, 5L)
  expect_equal(state$turn, 5L)
  expect_equal(state$auto_stop_reason, "completed")
})

test_that("low-budget run with interventions does not false-stop under capped pricing", {
  low_budget <- list(budget = 20, costs = list(ag = 2, pcr = 10))
  state <- make_autorun_fixture(
    config_turns = 3L,
    policy_id = "manual",
    budget = low_budget,
    active_ids = c("distancing", "rapid_labs")
  )
  state$allocs_manual$n_ag <- 0L
  state$allocs_manual$n_pcr <- 0L

  state <- start_autorun_fixture(state)
  state <- tick_autorun_fixture(state)

  expect_true(state$auto_running)
  expect_null(state$auto_stop_reason)
  expect_equal(state$auto_turns_completed, 1L)
  expect_equal(state$turn, 1L)
  expect_lte(state$run_log$intervention_cost[[1]], round(0.60 * low_budget$budget))
})

test_that("manual mode does not auto-advance", {
  state <- make_autorun_fixture(run_mode = "manual", config_turns = 4L, policy_id = "all_boroughs")
  state <- start_autorun_fixture(state)

  for (i in seq_len(3)) {
    state <- tick_autorun_fixture(state)
  }

  expect_false(state$auto_running)
  expect_equal(state$auto_turns_completed, 0L)
  expect_equal(state$turn, 0L)
  expect_null(state$auto_stop_reason)
})

test_that("dual-branch auto-run preserves exact N-turn lockstep", {
  player <- make_autorun_fixture(config_turns = 3L, policy_id = "all_boroughs")
  baseline <- make_autorun_fixture(config_turns = 3L, policy_id = "manual")
  baseline$allocs_manual$n_ag <- 0L
  baseline$allocs_manual$n_pcr <- 0L

  player <- start_autorun_fixture(player)
  master_seed <- 2024L

  while (isTRUE(player$auto_running)) {
    turn_seed <- derive_turn_seed_fixture(master_seed, player$turn)

    player_intervention_cost <- compute_intervention_effects(
      selected = player$active_ids,
      catalog = player$intervention_defs,
      state = player$intervention_state,
      observation_history = player$observation_state$history,
      current_turn = player$turn,
      turn_budget = player$budget$budget
    )$cost
    player_allocs <- build_policy_allocations(list(
      policy_id = player$policy_id,
      allocs_manual = player$allocs_manual,
      pop_tbl = player$effective_pop_tbl,
      budget = player$budget,
      intervention_cost = player_intervention_cost,
      current_turn = player$turn,
      observation_state = player$observation_state,
      available_now = player$available_now
    ))

    baseline_allocs <- build_baseline_allocations(
      mode = "basic_surveillance",
      request = list(
        pop_tbl = baseline$effective_pop_tbl,
        budget = baseline$budget,
        intervention_cost = 0,
        current_turn = baseline$turn,
        observation_state = baseline$observation_state,
        available_now = baseline$available_now
      )
    )

    player_result <- run_single_turn(list(
      current_turn = player$turn,
      allocs = player_allocs,
      budget = player$budget,
      params_epi = player$params_epi,
      params_mort = player$params_mort,
      testing_params = player$testing_params,
      active_ids = player$active_ids,
      intervention_defs = player$intervention_defs,
      intervention_state = player$intervention_state,
      total_spend = player$total_spend,
      truth_state = player$truth_state,
      observation_state = player$observation_state,
      active_kernel = player$active_kernel,
      effective_pop_tbl = player$effective_pop_tbl,
      pop_tbl = player$pop_tbl,
      run_log = player$run_log,
      turn_logs = player$turn_logs,
      branch = "player",
      truth_seed = turn_seed
    ))

    baseline_result <- run_single_turn(list(
      current_turn = baseline$turn,
      allocs = baseline_allocs,
      budget = baseline$budget,
      params_epi = baseline$params_epi,
      params_mort = baseline$params_mort,
      testing_params = baseline$testing_params,
      active_ids = character(),
      intervention_defs = baseline$intervention_defs,
      intervention_state = baseline$intervention_state,
      total_spend = baseline$total_spend,
      truth_state = baseline$truth_state,
      observation_state = baseline$observation_state,
      active_kernel = baseline$active_kernel,
      effective_pop_tbl = baseline$effective_pop_tbl,
      pop_tbl = baseline$pop_tbl,
      run_log = baseline$run_log,
      turn_logs = baseline$turn_logs,
      branch = "baseline",
      truth_seed = turn_seed
    ))

    expect_equal(player_result$status, "ok")
    expect_equal(baseline_result$status, "ok")

    player$total_spend <- player_result$total_spend
    player$truth_state <- player_result$truth_state
    player$observation_state <- player_result$observation_state
    player$available_now <- player_result$available_now
    player$intervention_state <- player_result$intervention_state
    player$turn <- player_result$next_turn
    player$run_log <- player_result$run_log
    player$turn_logs <- player_result$turn_logs

    baseline$total_spend <- baseline_result$total_spend
    baseline$truth_state <- baseline_result$truth_state
    baseline$observation_state <- baseline_result$observation_state
    baseline$available_now <- baseline_result$available_now
    baseline$intervention_state <- baseline_result$intervention_state
    baseline$turn <- baseline_result$next_turn
    baseline$run_log <- baseline_result$run_log
    baseline$turn_logs <- baseline_result$turn_logs

    player$auto_turns_completed <- player$auto_turns_completed + 1L
    if (player$auto_turns_completed >= player$auto_turns_requested) {
      player$auto_running <- FALSE
      player$auto_stop_reason <- "completed"
    }
  }

  expect_equal(player$auto_turns_completed, 3L)
  expect_equal(player$turn, 3L)
  expect_equal(baseline$turn, 3L)
  expect_equal(player$auto_stop_reason, "completed")
})
