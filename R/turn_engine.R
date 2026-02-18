# Shared turn engine -----------------------------------------------------------

validate_turn_request <- function(request) {
  if (!is.list(request)) {
    stop("request must be a list")
  }

  required_fields <- c(
    "current_turn",
    "allocs",
    "budget",
    "params_epi",
    "params_mort",
    "testing_params",
    "active_ids",
    "intervention_defs",
    "total_spend",
    "truth_state",
    "observation_state",
    "active_kernel",
    "effective_pop_tbl",
    "pop_tbl",
    "run_log",
    "turn_logs"
  )
  missing_fields <- setdiff(required_fields, names(request))
  if (length(missing_fields) > 0) {
    stop("request missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  if (!is.numeric(request$current_turn) || length(request$current_turn) != 1L || is.na(request$current_turn)) {
    stop("request$current_turn must be a single numeric value")
  }

  if (!inherits(request$allocs, "data.frame")) {
    stop("request$allocs must be a data frame")
  }
  alloc_cols <- c("borough_index", "borough_name", "n_ag", "n_pcr")
  missing_alloc_cols <- setdiff(alloc_cols, names(request$allocs))
  if (length(missing_alloc_cols) > 0) {
    stop("request$allocs missing required columns: ", paste(missing_alloc_cols, collapse = ", "))
  }

  if (!is.list(request$budget) || is.null(request$budget$budget) || is.null(request$budget$costs)) {
    stop("request$budget must be a list with entries 'budget' and 'costs'")
  }

  if (!is.list(request$params_epi)) {
    stop("request$params_epi must be a list")
  }

  if (!is.list(request$params_mort)) {
    stop("request$params_mort must be a list")
  }

  if (!is.list(request$testing_params)) {
    stop("request$testing_params must be a list")
  }

  if (!is.list(request$truth_state)) {
    stop("request$truth_state must be a list")
  }

  if (!is.list(request$observation_state) ||
      is.null(request$observation_state$queue) ||
      is.null(request$observation_state$history)) {
    stop("request$observation_state must be a list with entries 'queue' and 'history'")
  }

  if (!is.matrix(request$active_kernel)) {
    stop("request$active_kernel must be a matrix")
  }

  if (!inherits(request$effective_pop_tbl, "data.frame") || !"pop" %in% names(request$effective_pop_tbl)) {
    stop("request$effective_pop_tbl must be a data frame with a 'pop' column")
  }

  if (!inherits(request$pop_tbl, "data.frame")) {
    stop("request$pop_tbl must be a data frame")
  }

  if (!inherits(request$run_log, "data.frame")) {
    stop("request$run_log must be a data frame")
  }

  if (!is.list(request$turn_logs) ||
      !all(c("truth", "observations", "actions") %in% names(request$turn_logs))) {
    stop("request$turn_logs must be a list with entries 'truth', 'observations', and 'actions'")
  }

  invisible(TRUE)
}

build_turn_notification <- function(available, queue, turn_index) {
  n_results <- if (!is.null(available) && "n" %in% names(available)) sum(available$n) else 0
  n_positives <- if (!is.null(available) && "positives" %in% names(available)) sum(available$positives) else 0
  n_pending <- if (!is.null(queue) && "n" %in% names(queue)) sum(queue$n) else 0

  summary_msg <- if (n_results > 0) {
    sprintf(
      "Turn %d complete: %d results returned (%d positive). %d tests pending.",
      turn_index, n_results, n_positives, n_pending
    )
  } else {
    sprintf(
      "Turn %d complete: No results yet. %d tests pending.",
      turn_index, n_pending
    )
  }

  list(
    message = summary_msg,
    type = if (n_positives > 0) "warning" else "message",
    duration = 5
  )
}

update_run_log_for_turn <- function(run_log, entry_cur, current_turn) {
  if (nrow(run_log) == 0 || !(current_turn %in% run_log$turn)) {
    return(dplyr::bind_rows(run_log, entry_cur))
  }

  dplyr::rows_update(run_log, entry_cur, by = "turn")
}

apply_interventions_to_params <- function(params_epi, testing_params, effects) {
  params_epi$Rt <- params_epi$Rt * effects$Rt_multiplier
  params_epi$imports_prob <- min(1, max(0, params_epi$imports_prob * effects$imports_multiplier))

  testing_params$delays$ag <- max(0, testing_params$delays$ag + effects$delay_adjust$ag)
  testing_params$delays$pcr <- max(0, testing_params$delays$pcr + effects$delay_adjust$pcr)

  list(
    params_epi = params_epi,
    testing_params = testing_params
  )
}

compute_turn_spend <- function(allocs, budget, intervention_cost) {
  spend_stats <- summarise_budget(allocs, budget$costs, budget$budget)
  turn_spend <- spend_stats$spend + intervention_cost
  remaining_budget <- budget$budget - turn_spend

  list(
    spend_stats = spend_stats,
    turn_spend = turn_spend,
    remaining_budget = remaining_budget
  )
}

run_single_turn <- function(request) {
  validate_turn_request(request)

  current_turn <- as.integer(request$current_turn)
  allocs <- request$allocs
  budget <- request$budget
  params_epi <- request$params_epi
  params_mort <- request$params_mort
  testing_params <- request$testing_params
  active_ids <- request$active_ids

  effects <- compute_intervention_effects(active_ids, request$intervention_defs)
  intervention_label <- if (length(effects$labels) == 0) "None" else paste(effects$labels, collapse = ", ")

  adjusted <- apply_interventions_to_params(params_epi, testing_params, effects)
  params_epi <- adjusted$params_epi
  testing_params <- adjusted$testing_params

  spend <- compute_turn_spend(allocs, budget, effects$cost)

  if (spend$remaining_budget < -1e-6) {
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

  new_total_spend <- request$total_spend + spend$turn_spend
  spend_snapshot <- list(
    total = new_total_spend,
    last_turn = spend$turn_spend,
    remaining = spend$remaining_budget,
    within_budget = spend$remaining_budget >= -1e-6,
    interventions = intervention_label
  )

  action_log <- append_action_log(
    action_log = request$turn_logs$actions,
    turn = current_turn,
    allocations = allocs,
    pop_tbl = request$pop_tbl,
    costs = budget$costs,
    meta = list(
      allocation_spend = spend$spend_stats$spend,
      intervention_cost = effects$cost,
      total_turn_spend = spend$turn_spend,
      cumulative_spend = new_total_spend,
      budget_remaining = spend$remaining_budget,
      interventions_active = intervention_label
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
    allocation_spend = spend$spend_stats$spend,
    intervention_cost = effects$cost,
    total_turn_spend = spend$turn_spend,
    cumulative_spend = new_total_spend,
    observed_tests = sum(obs$available$n),
    observed_positives = sum(obs$available$positives),
    reported_deaths = current_deaths,
    active_interventions = intervention_label,
    notes = "",
    queue_pending = sum(obs$queue$n)
  )

  updated_run_log <- update_run_log_for_turn(request$run_log, entry_cur, current_turn)
  notification <- build_turn_notification(obs$available, obs$queue, current_turn)

  list(
    status = "ok",
    error_message = NULL,
    total_spend = new_total_spend,
    spend_current = spend_snapshot,
    truth_state = next_truth,
    truth_history = next_truth$history,
    observation_state = next_observation_state,
    available_now = obs$available,
    next_turn = current_turn + 1L,
    turn_logs = list(
      truth = truth_log,
      observations = observation_log,
      actions = action_log
    ),
    run_log = updated_run_log,
    notification = notification
  )
}
