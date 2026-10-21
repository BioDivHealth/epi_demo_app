# Observation model -----------------------------------------------------------

#' Create an empty observation state
#'
#' @return A list storing a delay queue and observation history.
#' @export
init_observation_state <- function() {
  # plain data frames (not tibbles): these are built every turn, and base R
  # tables are much quicker when bots play thousands of games
  empty <- data.frame(
    borough_index = integer(),
    borough_name = character(),
    type = character(),
    n = integer(),
    positives = integer(),
    true_pos = integer(),
    infectious_pos = integer(),
    mode = character(),
    source = character(),
    turn_sampled = integer(),
    turn_available = integer(),
    stringsAsFactors = FALSE
  )
  list(queue = empty, history = empty)
}

#' Simulate testing outcomes with delays
#'
#' @param state Observation state list.
#' @param turn Current turn (integer).
#' @param allocations Tidy tibble with columns borough_index, borough_name,
#'   n_pcr, n_ag.
#' @param truth_incidence Numeric vector of underlying infections per borough.
#' @param pop_tbl Population tibble with `pop` column.
#' @param params List with components `sens`, `spec`, `delays`, `prev_floor`,
#'   and optionally `sampling_mode`, `sampling_pool_frac` and `care_seeking`
#'   (per-borough multiplier, symptomatic mode only), and `detectable`
#'   (list with `ag` / `pcr`: per-borough people still test-positive from
#'   recent infections, random mode only).
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

  sampling_mode <- params$sampling_mode %||% "random"
  if (sampling_mode == "symptomatic") {
    pool_frac <- params$sampling_pool_frac %||% 0.1
    pool_frac <- max(min(pool_frac, 1), 1e-6)

    # some boroughs are less likely to come forward for a test, so fewer of
    # their infected people end up in the tested pool (1 = no bias)
    care_seeking <- params$care_seeking %||% 1
    # people with no symptoms never come forward
    symptomatic_share <- params$symptomatic_share %||% 1
    prev <- truth_incidence * symptomatic_share * care_seeking / (pop_tbl$pop * pool_frac)
  }
  prev <- pmax(prev, prev_floor)
  prev <- pmin(prev, 0.9)

  se <- params$sens
  sp <- params$spec
  delays <- params$delays

  simulate_type <- function(type, n_col) {
    n_tests <- allocations[[n_col]]
    if (all(n_tests == 0)) {
      return(NULL)
    }

    sens <- se[[type]]
    spec <- sp[[type]]
    delay <- delays[[type]]

    # random testing finds anyone still test-positive from recent turns,
    # but only this turn's infections are still spreading
    prev_type <- prev
    current_share <- rep(1, length(n_tests))
    detectable <- params$detectable[[type]]
    if (sampling_mode == "random" && !is.null(detectable)) {
      prev_type <- pmin(pmax(detectable / pop_tbl$pop, prev_floor), 0.9)
      current_share <- ifelse(detectable > 0, pmin(1, truth_incidence / detectable), 0)
    }

    p_star <- sens * prev_type + (1 - spec) * (1 - prev_type)
    p_star <- pmin(pmax(p_star, 0), 1)

    positives <- stats::rbinom(length(n_tests), size = n_tests, prob = p_star)

    # how many of those positives are real infections, and how many of
    # those are still infectious (both hidden from players)
    true_share <- ifelse(p_star > 0, pmin(1, sens * prev_type / p_star), 0)
    true_pos <- stats::rbinom(length(positives), size = positives, prob = true_share)
    infectious_pos <- stats::rbinom(length(true_pos), size = true_pos, prob = current_share)

    results <- data.frame(
      borough_index = allocations$borough_index,
      borough_name = allocations$borough_name,
      type = type,
      n = as.integer(n_tests),
      positives = as.integer(positives),
      true_pos = as.integer(true_pos),
      infectious_pos = as.integer(infectious_pos),
      mode = sampling_mode,
      source = params$source %||% "player",
      turn_sampled = as.integer(turn),
      turn_available = as.integer(turn + delay),
      stringsAsFactors = FALSE
    )
    results[results$n > 0, , drop = FALSE]
  }

  ag_results <- simulate_type("ag", "n_ag")
  pcr_results <- simulate_type("pcr", "n_pcr")

  # rbind of NULLs is NULL; start from the empty table so columns always exist
  new_results <- rbind(state$queue[0, , drop = FALSE], ag_results, pcr_results)

  # results due now come back; later ones wait in the queue
  prior_queue <- state$queue
  due <- prior_queue$turn_available <= turn
  ready <- new_results$turn_available <= turn
  available_now <- rbind(prior_queue[due, , drop = FALSE], new_results[ready, , drop = FALSE])
  retained_queue <- rbind(prior_queue[!due, , drop = FALSE], new_results[!ready, , drop = FALSE])

  available_now <- available_now[order(available_now$borough_index, available_now$type), , drop = FALSE]
  rownames(available_now) <- NULL
  rownames(retained_queue) <- NULL
  history <- rbind(state$history, available_now)
  rownames(history) <- NULL

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
