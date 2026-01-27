# Turn logging utilities ----------------------------------------------------

#' Initialise empty turn-by-turn logs
#'
#' @param pop_tbl Population table with columns `borough_name`, `gss_code`, and `pop`.
#' @return A list with empty tibbles for truth, observations, and actions.
#' @export
init_turn_logs <- function(pop_tbl) {
  required_cols <- c("borough_name", "gss_code", "pop")
  if (!all(required_cols %in% names(pop_tbl))) {
    stop("pop_tbl must contain columns: ", paste(required_cols, collapse = ", "))
  }

  pop_tbl <- tibble::as_tibble(pop_tbl)
  pop_tbl <- pop_tbl |>
    dplyr::mutate(
      borough_index = dplyr::row_number()
    )

  truth_cols <- tibble::tibble(
    turn = integer(),
    borough_index = integer(),
    gss_code = character(),
    borough_name = character(),
    incidence = integer(),
    cumulative = integer(),
    deaths = integer(),
    cumulative_deaths = integer(),
    imports = integer(),
    Rt = numeric()
  )

  observation_cols <- tibble::tibble(
    turn_reported = integer(),
    turn_sampled = integer(),
    turn_available = integer(),
    borough_index = integer(),
    gss_code = character(),
    borough_name = character(),
    test_type = character(),
    n_returned = integer(),
    positives = integer(),
    positivity = numeric()
  )

  action_cols <- tibble::tibble(
    turn = integer(),
    borough_index = integer(),
    gss_code = character(),
    borough_name = character(),
    test_type = character(),
    n_allocated = integer(),
    spend = numeric(),
    cost_per_test = numeric(),
    allocation_spend = numeric(),
    intervention_cost = numeric(),
    total_turn_spend = numeric(),
    cumulative_spend = numeric(),
    budget_remaining = numeric(),
    interventions_active = character()
  )

  attr(truth_cols, "pop_reference") <- pop_tbl

  list(
    truth = truth_cols,
    observations = observation_cols,
    actions = action_cols
  )
}

#' Append a turn of hidden truth to the log
#'
#' @param truth_log Existing truth log tibble.
#' @param turn Integer turn index being recorded.
#' @param incidence Integer vector of incidence counts per borough.
#' @param cumulative Integer vector of cumulative counts per borough.
#' @param imports Integer vector of import counts per borough.
#' @param Rt Numeric reproduction number applied on this turn.
#' @param pop_tbl Population table used to supply borough metadata.
#' @return Updated truth log tibble.
#' @export
append_truth_log <- function(truth_log, turn, incidence, cumulative, deaths, cumulative_deaths, imports, Rt, pop_tbl) {
  incidence <- as.integer(incidence)
  cumulative <- as.integer(cumulative)
  deaths <- as.integer(deaths)
  cumulative_deaths <- as.integer(cumulative_deaths)
  imports <- as.integer(imports)

  n <- length(incidence)
  if (length(cumulative) != n || length(deaths) != n ||
      length(cumulative_deaths) != n || length(imports) != n) {
    stop("incidence, cumulative, deaths, cumulative_deaths, and imports must have the same length")
  }

  pop_tbl <- tibble::as_tibble(pop_tbl)
  if (nrow(pop_tbl) != n) {
    stop("pop_tbl row count must match incidence length")
  }

  addition <- pop_tbl |>
    dplyr::mutate(
      borough_index = dplyr::row_number(),
      turn = as.integer(turn),
      incidence = incidence,
      cumulative = cumulative,
      deaths = deaths,
      cumulative_deaths = cumulative_deaths,
      imports = imports,
      Rt = as.numeric(Rt)
    ) |>
    dplyr::select(
      turn,
      borough_index,
      gss_code,
      borough_name,
      incidence,
      cumulative,
      deaths,
      cumulative_deaths,
      imports,
      Rt
    )

  dplyr::bind_rows(truth_log, addition)
}

#' Append observed results for a turn
#'
#' @param observation_log Existing observation log tibble.
#' @param available_results Tibble of results available on the current turn.
#' @param turn_reported Integer turn index when results became visible to the user.
#' @param pop_tbl Population table for borough metadata.
#' @return Updated observation log tibble.
#' @export
append_observation_log <- function(observation_log, available_results, turn_reported, pop_tbl) {
  if (is.null(available_results) || nrow(available_results) == 0) {
    return(observation_log)
  }

  required_cols <- c(
    "borough_index", "borough_name", "type",
    "n", "positives", "turn_sampled", "turn_available"
  )
  missing_cols <- setdiff(required_cols, names(available_results))
  if (length(missing_cols) > 0) {
    stop("available_results missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  pop_tbl <- tibble::as_tibble(pop_tbl)
  addition <- available_results |>
    dplyr::left_join(
      pop_tbl |>
        dplyr::mutate(borough_index = dplyr::row_number()) |>
        dplyr::select(borough_index, gss_code),
      by = "borough_index"
    ) |>
    dplyr::mutate(
      turn_reported = as.integer(turn_reported),
      test_type = type,
      n_returned = as.integer(n),
      positives = as.integer(positives),
      positivity = dplyr::if_else(n_returned > 0, positives / n_returned, 0),
      turn_sampled = as.integer(turn_sampled),
      turn_available = as.integer(turn_available)
    ) |>
    dplyr::select(
      turn_reported,
      turn_sampled,
      turn_available,
      borough_index,
      gss_code,
      borough_name,
      test_type,
      n_returned,
      positives,
      positivity
    )

  dplyr::bind_rows(observation_log, addition)
}

#' Append allocation decisions for a turn
#'
#' @param action_log Existing action log tibble.
#' @param turn Integer turn on which allocations were chosen.
#' @param allocations Tibble with columns borough_index, borough_name, n_ag, n_pcr.
#' @param pop_tbl Population table providing `gss_code`.
#' @param costs Named numeric vector with entries `ag` and `pcr`.
#' @param meta Optional list carrying per-turn metadata, e.g. remaining budget.
#' @return Updated action log tibble.
#' @export
append_action_log <- function(action_log, turn, allocations, pop_tbl, costs, meta = list()) {
  required_cols <- c("borough_index", "borough_name", "n_ag", "n_pcr")
  missing_cols <- setdiff(required_cols, names(allocations))
  if (length(missing_cols) > 0) {
    stop("allocations missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  long_allocs <- dplyr::bind_rows(
    allocations |>
      dplyr::select(borough_index, borough_name, n = n_ag) |>
      dplyr::mutate(test_type = "ag"),
    allocations |>
      dplyr::select(borough_index, borough_name, n = n_pcr) |>
      dplyr::mutate(test_type = "pcr")
  ) |>
    dplyr::filter(n > 0)

  if (nrow(long_allocs) == 0) {
    return(action_log)
  }

  cost_ag <- costs[["ag"]]
  if (is.null(cost_ag)) cost_ag <- 0
  cost_pcr <- costs[["pcr"]]
  if (is.null(cost_pcr)) cost_pcr <- 0
  costs <- c(ag = cost_ag, pcr = cost_pcr)

  pop_tbl <- tibble::as_tibble(pop_tbl) |>
    dplyr::mutate(borough_index = dplyr::row_number()) |>
    dplyr::select(borough_index, gss_code)

  addition <- long_allocs |>
    dplyr::left_join(pop_tbl, by = "borough_index") |>
    dplyr::mutate(
      turn = as.integer(turn),
      n_allocated = as.integer(n),
      cost_per_test = dplyr::if_else(test_type == "ag", costs[["ag"]], costs[["pcr"]]),
      spend = cost_per_test * n_allocated
    ) |>
    dplyr::select(-n)

  meta_defaults <- list(
    allocation_spend = sum(addition$spend),
    intervention_cost = 0,
    total_turn_spend = sum(addition$spend),
    cumulative_spend = NA_real_,
    budget_remaining = NA_real_,
    interventions_active = "None"
  )
  meta_full <- utils::modifyList(meta_defaults, meta, keep.null = TRUE)

  addition <- addition |>
    dplyr::mutate(
      allocation_spend = meta_full$allocation_spend,
      intervention_cost = meta_full$intervention_cost,
      total_turn_spend = meta_full$total_turn_spend,
      cumulative_spend = meta_full$cumulative_spend,
      budget_remaining = meta_full$budget_remaining,
      interventions_active = meta_full$interventions_active
    ) |>
    dplyr::select(
      turn,
      borough_index,
      gss_code,
      borough_name,
      test_type,
      n_allocated,
      spend,
      cost_per_test,
      allocation_spend,
      intervention_cost,
      total_turn_spend,
      cumulative_spend,
      budget_remaining,
      interventions_active
    )

  dplyr::bind_rows(action_log, addition)
}

#' Estimate currently infectious counts using a rolling window
#'
#' @param truth_log Tibble produced by successive calls to `append_truth_log()`.
#'   Must contain columns `turn`, `borough_index`, `borough_name`, and `incidence`.
#' @param turn Integer turn at which to evaluate active infections.
#' @param window Integer number of turns to include (inclusive of `turn`).
#' @return Tibble with columns `borough_index`, `borough_name`, and `active`.
#' @export
compute_active_infections <- function(truth_log, turn, window = 3L) {
  if (missing(truth_log) || is.null(truth_log) || nrow(truth_log) == 0) {
    return(tibble::tibble(
      borough_index = integer(),
      borough_name = character(),
      active = integer()
    ))
  }

  if (!("turn" %in% names(truth_log)) || !("incidence" %in% names(truth_log))) {
    stop("truth_log must include columns 'turn' and 'incidence'")
  }

  if (!("borough_index" %in% names(truth_log)) || !("borough_name" %in% names(truth_log))) {
    stop("truth_log must include borough_index and borough_name columns")
  }

  if (length(turn) != 1 || !is.numeric(turn) || is.na(turn)) {
    stop("turn must be a single numeric value")
  }

  window <- as.integer(window)
  if (length(window) != 1 || is.na(window) || window < 1) {
    stop("window must be a positive integer")
  }

  turn_val <- as.integer(turn)
  lower_bound <- turn_val - window + 1L

  in_window <- truth_log[truth_log$turn <= turn_val & truth_log$turn >= lower_bound, , drop = FALSE]
  if (nrow(in_window) == 0) {
    base_set <- truth_log[truth_log$turn <= turn_val, c("borough_index", "borough_name"), drop = FALSE]
    base_set <- unique(base_set)
    if (nrow(base_set) == 0) {
      base_set <- truth_log[, c("borough_index", "borough_name"), drop = FALSE]
      base_set <- unique(base_set)
    }
    if (nrow(base_set) == 0) {
      return(tibble::tibble(
        borough_index = integer(),
        borough_name = character(),
        active = integer()
      ))
    }
    return(
      tibble::tibble(
        borough_index = base_set$borough_index,
        borough_name = base_set$borough_name,
        active = integer(nrow(base_set))
      )
    )
  }

  all_boros <- truth_log[truth_log$turn <= turn_val, c("borough_index", "borough_name"), drop = FALSE]
  all_boros <- unique(all_boros)

  active <- in_window |>
    dplyr::group_by(borough_index, borough_name) |>
    dplyr::summarise(active = sum(incidence, na.rm = TRUE), .groups = "drop")

  result <- dplyr::right_join(
    active,
    tibble::as_tibble(all_boros),
    by = c("borough_index", "borough_name")
  )
  result$active[is.na(result$active)] <- 0L
  result
}
