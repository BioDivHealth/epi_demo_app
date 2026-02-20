# Branch state helpers --------------------------------------------------------

clone_branch_object <- function(x) {
  unserialize(serialize(x, NULL))
}

new_branch_state <- function(
    name,
    turn,
    truth_state,
    observation_state,
    available_now,
    run_log,
    turn_logs,
    total_spend,
    spend_current,
    truth_history = NULL) {
  if (is.null(truth_history)) {
    truth_history <- truth_state$history
  }

  branch <- list(
    name = as.character(name),
    turn = as.integer(turn),
    truth_state = truth_state,
    truth_history = truth_history,
    observation_state = observation_state,
    available_now = tibble::as_tibble(available_now),
    total_spend = as.numeric(total_spend),
    spend_current = spend_current,
    run_log = tibble::as_tibble(run_log),
    turn_logs = turn_logs
  )

  validate_branch_state(branch)
  branch
}

validate_branch_state <- function(branch) {
  if (!is.list(branch)) {
    stop("branch must be a list")
  }

  required_fields <- c(
    "name",
    "turn",
    "truth_state",
    "truth_history",
    "observation_state",
    "available_now",
    "total_spend",
    "spend_current",
    "run_log",
    "turn_logs"
  )
  missing_fields <- setdiff(required_fields, names(branch))
  if (length(missing_fields) > 0) {
    stop("branch missing required fields: ", paste(missing_fields, collapse = ", "))
  }

  if (!is.character(branch$name) || length(branch$name) != 1L || is.na(branch$name)) {
    stop("branch$name must be a single character value")
  }

  if (!is.numeric(branch$turn) || length(branch$turn) != 1L || is.na(branch$turn)) {
    stop("branch$turn must be a single numeric value")
  }

  if (!is.list(branch$truth_state)) {
    stop("branch$truth_state must be a list")
  }

  if (!is.list(branch$truth_history)) {
    stop("branch$truth_history must be a list")
  }

  if (!is.list(branch$observation_state) ||
      is.null(branch$observation_state$queue) ||
      is.null(branch$observation_state$history)) {
    stop("branch$observation_state must include queue and history")
  }

  if (!inherits(branch$available_now, "data.frame")) {
    stop("branch$available_now must be a data frame")
  }

  if (!is.numeric(branch$total_spend) || length(branch$total_spend) != 1L || is.na(branch$total_spend)) {
    stop("branch$total_spend must be a single numeric value")
  }

  if (!is.list(branch$spend_current)) {
    stop("branch$spend_current must be a list")
  }

  if (!inherits(branch$run_log, "data.frame")) {
    stop("branch$run_log must be a data frame")
  }

  if (!is.list(branch$turn_logs) ||
      !all(c("truth", "observations", "actions") %in% names(branch$turn_logs))) {
    stop("branch$turn_logs must include truth, observations, and actions")
  }

  invisible(TRUE)
}

initialise_branch_pair <- function(init_truth, player_observation, player_available, pop_tbl, budget_total) {
  if (!is.list(init_truth)) {
    stop("init_truth must be a list")
  }

  if (!is.list(player_observation) ||
      is.null(player_observation$queue) ||
      is.null(player_observation$history)) {
    stop("player_observation must include queue and history")
  }

  budget_total <- as.numeric(budget_total)
  if (!is.finite(budget_total)) {
    budget_total <- 0
  }

  empty_logs <- init_turn_logs(pop_tbl)
  base_spend <- list(
    total = 0,
    last_turn = 0,
    remaining = budget_total,
    within_budget = TRUE,
    interventions = "None"
  )

  if (!inherits(player_available, "data.frame")) {
    player_available <- tibble::tibble(
      borough_index = integer(),
      borough_name = character(),
      type = character(),
      n = integer(),
      positives = integer(),
      turn_sampled = integer(),
      turn_available = integer()
    )
  }
  baseline_available <- player_available[0, , drop = FALSE]

  player_branch <- new_branch_state(
    name = "player",
    turn = init_truth$turn,
    truth_state = clone_branch_object(init_truth),
    truth_history = clone_branch_object(init_truth$history),
    observation_state = player_observation,
    available_now = player_available,
    run_log = tibble::tibble(),
    turn_logs = clone_branch_object(empty_logs),
    total_spend = 0,
    spend_current = base_spend
  )

  baseline_branch <- new_branch_state(
    name = "baseline",
    turn = init_truth$turn,
    truth_state = clone_branch_object(init_truth),
    truth_history = clone_branch_object(init_truth$history),
    observation_state = init_observation_state(),
    available_now = baseline_available,
    run_log = tibble::tibble(),
    turn_logs = clone_branch_object(empty_logs),
    total_spend = 0,
    spend_current = base_spend
  )

  list(player = player_branch, baseline = baseline_branch)
}

apply_branch_turn_result <- function(branch, result) {
  validate_branch_state(branch)

  if (!is.list(result) || !identical(result$status, "ok")) {
    stop("result must be a successful run_single_turn output")
  }

  branch$turn <- as.integer(result$next_turn)
  branch$total_spend <- as.numeric(result$total_spend)
  branch$spend_current <- result$spend_current
  branch$truth_state <- result$truth_state
  branch$truth_history <- result$truth_history
  branch$observation_state <- result$observation_state
  branch$available_now <- result$available_now
  branch$turn_logs <- result$turn_logs
  branch$run_log <- result$run_log

  branch
}
