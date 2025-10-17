# Observation model -----------------------------------------------------------

#' Create an empty observation state
#'
#' @return A list storing a delay queue and observation history.
#' @export
init_observation_state <- function() {
  list(
    queue = tibble::tibble(
      borough_index = integer(),
      borough_name = character(),
      type = character(),
      n = integer(),
      positives = integer(),
      turn_sampled = integer(),
      turn_available = integer()
    ),
    history = tibble::tibble(
      borough_index = integer(),
      borough_name = character(),
      type = character(),
      n = integer(),
      positives = integer(),
      turn_sampled = integer(),
      turn_available = integer()
    )
  )
}

#' Simulate testing outcomes with delays
#'
#' @param state Observation state list.
#' @param turn Current turn (integer).
#' @param allocations Tidy tibble with columns borough_index, borough_name,
#'   n_pcr, n_ag.
#' @param truth_incidence Numeric vector of underlying infections per borough.
#' @param pop_tbl Population tibble with `pop` column.
#' @param params List with components `sens`, `spec`, `delays`, `prev_floor`.
#' @return Updated state with queue/history and tibble of available results.
#' @export
observe_tests <- function(
    state,
    turn,
    allocations,
    truth_incidence,
    pop_tbl,
    params) {
  if (missing(state) || is.null(state$queue)) {
    stop("state must be created with init_observation_state()")
  }

  if (!all(c("borough_index", "borough_name", "n_pcr", "n_ag") %in% names(allocations))) {
    stop("allocations missing required columns")
  }

  if (length(truth_incidence) != nrow(allocations)) {
    stop("truth_incidence must align with allocations")
  }

  prev_floor <- params$prev_floor %||% 1e-6

  prev <- truth_incidence / pop_tbl$pop
  prev <- pmax(prev, prev_floor)
  prev <- pmin(prev, 0.9)

  se <- params$sens
  sp <- params$spec
  delays <- params$delays

  simulate_type <- function(type, n_col) {
    n_tests <- allocations[[n_col]]
    if (all(n_tests == 0)) {
      return(tibble::tibble())
    }

    sens <- se[[type]]
    spec <- sp[[type]]
    delay <- delays[[type]]

    p_star <- sens * prev + (1 - spec) * (1 - prev)
    p_star <- pmin(pmax(p_star, 0), 1)

    positives <- stats::rbinom(length(n_tests), size = n_tests, prob = p_star)

    tibble::tibble(
      borough_index = allocations$borough_index,
      borough_name = allocations$borough_name,
      type = type,
      n = n_tests,
      positives = positives,
      turn_sampled = turn,
      turn_available = turn + delay
    ) |> 
      dplyr::filter(n > 0)
  }

  ag_results <- simulate_type("ag", "n_ag")
  pcr_results <- simulate_type("pcr", "n_pcr")

  new_results <- dplyr::bind_rows(ag_results, pcr_results)

  # Separate queued vs available
  prior_queue <- state$queue
  available_from_queue <- dplyr::filter(prior_queue, turn_available <= turn)
  retained_queue <- dplyr::filter(prior_queue, turn_available > turn)

  available_now <- available_from_queue
  if (nrow(new_results) > 0) {
    ready_now <- dplyr::filter(new_results, turn_available <= turn)
    future_results <- dplyr::filter(new_results, turn_available > turn)
    available_now <- dplyr::bind_rows(available_now, ready_now)
    retained_queue <- dplyr::bind_rows(retained_queue, future_results)
  }

  available_now <- dplyr::arrange(available_now, borough_index, type)
  history <- dplyr::bind_rows(state$history, available_now)

  list(
    queue = retained_queue,
    history = history,
    available = available_now,
    new_results = new_results
  )
}

#' Summarise testing spend against budget
#'
#' @param allocations Tibble with columns n_pcr, n_ag.
#' @param costs Named numeric vector or list with entries pcr, ag.
#' @param budget Total budget available this turn.
#' @return A list with total spend and remaining budget.
#' @export
summarise_budget <- function(allocations, costs, budget) {
  cost_pcr <- costs[["pcr"]] %||% 0
  cost_ag <- costs[["ag"]] %||% 0
  spend <- sum(allocations$n_pcr * cost_pcr + allocations$n_ag * cost_ag)

  list(
    spend = spend,
    remaining = budget - spend,
    within_budget = spend <= budget + 1e-6
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
