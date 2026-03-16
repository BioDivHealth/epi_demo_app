library(testthat)

make_turn_request_fixture <- function(current_turn = 0L) {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2", "C3"),
    borough_name = c("Alpha", "Beta", "Gamma"),
    pop = c(1000, 1400, 900)
  )

  list(
    current_turn = as.integer(current_turn),
    allocs = tibble::tibble(
      borough_index = 1:3,
      borough_name = pop_tbl$borough_name,
      n_ag = c(8L, 4L, 0L),
      n_pcr = c(0L, 5L, 3L)
    ),
    budget = list(
      budget = 250,
      costs = list(ag = 2, pcr = 6)
    ),
    params_epi = list(
      Rt = 1.2,
      k = 0.8,
      imports_prob = 0.05,
      lambda_import = 0.4
    ),
    params_mort = list(
      death_ifr = 0.01,
      death_delay = 1L
    ),
    testing_params = list(
      sens = list(ag = 0.85, pcr = 0.95),
      spec = list(ag = 0.98, pcr = 0.995),
      delays = list(ag = 0L, pcr = 1L),
      prev_floor = 1e-6,
      sampling_mode = "random",
      sampling_pool_frac = 0.1
    ),
    active_ids = c("distancing", "rapid_labs"),
    intervention_defs = intervention_catalog(),
    intervention_state = init_intervention_state(),
    total_spend = 40,
    truth_state = {
      state <- init_truth(
        n_boroughs = 3,
        seed_boroughs = c(1L, 3L),
        seed_cases = c(30L, 5L)
      )
      state$turn <- as.integer(current_turn)
      state
    },
    observation_state = init_observation_state(),
    active_kernel = matrix(
      c(
        0.7, 0.2, 0.1,
        0.2, 0.6, 0.2,
        0.1, 0.3, 0.6
      ),
      nrow = 3,
      byrow = TRUE
    ),
    effective_pop_tbl = pop_tbl,
    pop_tbl = pop_tbl,
    run_log = tibble::tibble(),
    turn_logs = init_turn_logs(pop_tbl)
  )
}

legacy_turn_reference <- function(request) {
  current_turn <- request$current_turn
  allocs <- request$allocs
  budget <- request$budget
  params_epi <- request$params_epi
  params_mort <- request$params_mort
  testing_params <- request$testing_params
  active_ids <- request$active_ids
  effects <- compute_intervention_effects(
    selected = active_ids,
    catalog = request$intervention_defs,
    state = request$intervention_state,
    observation_history = request$observation_state$history,
    current_turn = current_turn,
    turn_budget = request$budget$budget
  )
  intervention_label <- if (length(effects$labels) == 0) "None" else paste(effects$labels, collapse = ", ")
  intervention_phases <- intervention_status_summary(effects$status_by_intervention)

  params_epi$Rt <- params_epi$Rt * effects$Rt_multiplier
  params_epi$imports_prob <- min(1, max(0, params_epi$imports_prob * effects$imports_multiplier))

  testing_params$delays$ag <- max(0, testing_params$delays$ag + effects$delay_adjust$ag)
  testing_params$delays$pcr <- max(0, testing_params$delays$pcr + effects$delay_adjust$pcr)

  spend_stats <- summarise_budget(allocs, budget$costs, budget$budget)
  intervention_cost <- effects$cost
  turn_spend <- spend_stats$spend + intervention_cost
  remaining_budget <- budget$budget - turn_spend

  if (remaining_budget < -1e-6) {
    return(list(
      status = "budget_exceeded",
      error_message = "Adjust allocations/interventions to meet budget",
      total_spend = request$total_spend,
      spend_current = NULL,
      truth_state = NULL,
      truth_history = NULL,
      observation_state = NULL,
      available_now = NULL,
      next_turn = current_turn,
      turn_logs = NULL,
      run_log = NULL,
      notification = NULL
    ))
  }

  new_total_spend <- request$total_spend + turn_spend
  spend_snapshot <- list(
    total = new_total_spend,
    last_turn = turn_spend,
    remaining = remaining_budget,
    within_budget = remaining_budget >= -1e-6,
    interventions = intervention_phases
  )

  action_log <- append_action_log(
    action_log = request$turn_logs$actions,
    turn = current_turn,
    allocations = allocs,
    pop_tbl = request$pop_tbl,
    costs = budget$costs,
    meta = list(
      allocation_spend = spend_stats$spend,
      intervention_cost = intervention_cost,
      total_turn_spend = turn_spend,
      cumulative_spend = new_total_spend,
      budget_remaining = remaining_budget,
      interventions_active = intervention_label,
      intervention_statuses = intervention_phases
    )
  )

  obs <- observe_tests(
    state = request$observation_state,
    turn = current_turn,
    allocations = allocs,
    truth_incidence = request$truth_state$incidence,
    pop_tbl = request$effective_pop_tbl,
    params = testing_params
  )
  next_observation_state <- list(queue = obs$queue, history = obs$history)

  current_deaths <- 0
  if (!is.null(request$truth_state$cumulative_deaths)) {
    current_deaths <- sum(request$truth_state$cumulative_deaths)
  }

  observation_log <- append_observation_log(
    observation_log = request$turn_logs$observations,
    available_results = obs$available,
    turn_reported = current_turn,
    pop_tbl = request$pop_tbl
  )

  next_truth <- step_truth(
    state = request$truth_state,
    K = request$active_kernel,
    Rt_global = params_epi$Rt,
    k_overdisp = params_epi$k,
    imports_prob = params_epi$imports_prob,
    lambda_import = params_epi$lambda_import,
    pop = request$effective_pop_tbl$pop,
    death_ifr = params_mort$death_ifr,
    death_delay = params_mort$death_delay
  )

  truth_log <- append_truth_log(
    truth_log = request$turn_logs$truth,
    turn = next_truth$turn,
    incidence = next_truth$incidence,
    cumulative = next_truth$cumulative,
    deaths = next_truth$deaths,
    cumulative_deaths = next_truth$cumulative_deaths,
    imports = next_truth$last_imports,
    Rt = params_epi$Rt,
    pop_tbl = request$pop_tbl
  )

  entry_cur <- tibble::tibble(
    turn = current_turn,
    tests_ag = sum(allocs$n_ag),
    tests_pcr = sum(allocs$n_pcr),
    allocation_spend = spend_stats$spend,
    intervention_cost = intervention_cost,
    total_turn_spend = turn_spend,
    cumulative_spend = new_total_spend,
    observed_tests = sum(obs$available$n),
    observed_positives = sum(obs$available$positives),
    reported_deaths = current_deaths,
    active_interventions = intervention_label,
    intervention_statuses = intervention_phases,
    notes = "",
    queue_pending = sum(obs$queue$n)
  )

  rl <- request$run_log
  if (nrow(rl) == 0 || !(current_turn %in% rl$turn)) {
    run_log <- dplyr::bind_rows(rl, entry_cur)
  } else {
    run_log <- dplyr::rows_update(rl, entry_cur, by = "turn")
  }

  n_results <- sum(obs$available$n)
  n_positives <- sum(obs$available$positives)
  n_pending <- sum(obs$queue$n)
  summary_msg <- if (n_results > 0) {
    sprintf(
      "Turn %d complete: %d results returned (%d positive). %d tests pending.",
      current_turn, n_results, n_positives, n_pending
    )
  } else {
    sprintf(
      "Turn %d complete: No results yet. %d tests pending.",
      current_turn, n_pending
    )
  }

  list(
    status = "ok",
    error_message = NULL,
    total_spend = new_total_spend,
    spend_current = spend_snapshot,
    truth_state = next_truth,
    truth_history = next_truth$history,
    observation_state = next_observation_state,
    available_now = obs$available,
    intervention_state = effects$next_state,
    next_turn = current_turn + 1L,
    turn_logs = list(
      truth = truth_log,
      observations = observation_log,
      actions = action_log
    ),
    run_log = run_log,
    notification = list(
      message = summary_msg,
      type = if (n_positives > 0) "warning" else "message",
      duration = 5
    )
  )
}

test_that("run_single_turn matches legacy deterministic behavior", {
  request <- make_turn_request_fixture(current_turn = 2L)

  set.seed(2026)
  legacy <- legacy_turn_reference(request)
  set.seed(2026)
  engine <- run_single_turn(request)

  expect_equal(engine$status, "ok")
  expect_equal(engine$next_turn, legacy$next_turn)
  expect_equal(engine$total_spend, legacy$total_spend)
  expect_equal(engine$spend_current, legacy$spend_current)
  expect_equal(sum(engine$available_now$n), sum(legacy$available_now$n))
  expect_equal(sum(engine$available_now$positives), sum(legacy$available_now$positives))
  expect_equal(engine$truth_state$incidence, legacy$truth_state$incidence)
  expect_equal(nrow(engine$turn_logs$actions), nrow(legacy$turn_logs$actions))
  expect_equal(nrow(engine$turn_logs$observations), nrow(legacy$turn_logs$observations))
  expect_equal(nrow(engine$turn_logs$truth), nrow(legacy$turn_logs$truth))
  expect_true(all(engine$truth_state$incidence >= 0))
  expect_true(all(engine$available_now$n >= 0))
  expect_true(engine$spend_current$total >= 0)
  expect_true(engine$spend_current$last_turn >= 0)
})

test_that("budget failure returns expected status and message", {
  request <- make_turn_request_fixture()
  request$budget$budget <- 10
  request$allocs$n_ag <- c(30L, 30L, 30L)
  request$allocs$n_pcr <- c(20L, 20L, 20L)

  result <- run_single_turn(request)

  expect_equal(result$status, "budget_exceeded")
  expect_equal(result$error_message, "Adjust allocations/interventions to meet budget")
  expect_equal(result$total_spend, request$total_spend)
  expect_equal(result$next_turn, request$current_turn)
  expect_null(result$truth_state)
  expect_null(result$observation_state)
  expect_null(result$run_log)

  total_spend_after <- request$total_spend
  turn_after <- request$current_turn
  if (result$status == "ok") {
    total_spend_after <- result$total_spend
    turn_after <- result$next_turn
  }
  expect_equal(total_spend_after, request$total_spend)
  expect_equal(turn_after, request$current_turn)
})

test_that("notification payload matches warning and no-result paths", {
  warning_request <- make_turn_request_fixture()
  warning_request$active_ids <- character()
  warning_request$testing_params$sens <- list(ag = 1, pcr = 1)
  warning_request$testing_params$spec <- list(ag = 0, pcr = 0)
  warning_request$testing_params$delays <- list(ag = 0L, pcr = 0L)
  warning_request$allocs <- tibble::tibble(
    borough_index = 1:3,
    borough_name = warning_request$pop_tbl$borough_name,
    n_ag = c(5L, 0L, 0L),
    n_pcr = c(0L, 0L, 0L)
  )

  warning_result <- run_single_turn(warning_request)
  expect_equal(warning_result$status, "ok")
  expect_equal(warning_result$notification$type, "warning")
  expect_match(warning_result$notification$message, "results returned", fixed = TRUE)

  no_results_request <- make_turn_request_fixture()
  no_results_request$active_ids <- character()
  no_results_request$allocs$n_ag <- 0L
  no_results_request$allocs$n_pcr <- 0L
  no_results_request$testing_params$delays <- list(ag = 0L, pcr = 0L)

  no_results <- run_single_turn(no_results_request)
  expect_equal(no_results$status, "ok")
  expect_equal(no_results$notification$type, "message")
  expect_match(no_results$notification$message, "No results yet", fixed = TRUE)
})

test_that("turn indexing uses current turn for action and observation logs", {
  request <- make_turn_request_fixture(current_turn = 7L)
  request$testing_params$delays <- list(ag = 0L, pcr = 0L)

  result <- run_single_turn(request)

  expect_equal(result$status, "ok")
  expect_true(all(result$turn_logs$actions$turn == 7L))
  expect_true(all(result$turn_logs$observations$turn_reported == 7L))
  expect_true(all(result$turn_logs$truth$turn == result$truth_state$turn))
  expect_equal(result$truth_state$turn, 8L)
})
