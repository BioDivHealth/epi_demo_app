# Scoring helpers ------------------------------------------------------------

#' Summarise performance at the end of a run
#'
#' @param truth_history List of integer vectors (incidence per turn).
#' @param observed_history Tibble of observed results (available outcomes).
#' @param pop_tbl Population tibble with `pop` column.
#' @param total_spend Numeric total spend accumulated across turns.
#' @return Named list of metrics.
#' @export
# Rt from a case series --------------------------------------------------------
#
# In this game one turn is one generation: each case's infections appear the
# next turn, so the serial interval is exactly 1 turn. Rt over a window of
# turns is then (cases in the window) / (cases one turn earlier), with
# uncertainty from Cori et al.'s method. (Assuming a longer serial interval, as an earlier
# version did, overstates Rt: 2.3 for a series growing 1.3x per turn.)
#
# Windows with fewer than `min_cases` cases the turn before give NA rather
# than a number driven by the prior.
estimate_rt_table <- function(incid_vector, window = 3, min_cases = 5) {
  incid_vector <- as.numeric(incid_vector)
  incid_vector[is.na(incid_vector)] <- 0
  n <- length(incid_vector)
  if (n < window + 1) {
    return(tibble::tibble(t_end = integer(), mean = numeric(), lower = numeric(), upper = numeric()))
  }

  t_end <- seq(window + 1, n)
  t_start <- t_end - window + 1

  # Cori et al. (2013), as in EpiEstim, with a 1-turn serial interval: the
  # posterior for R over a window is a gamma distribution with
  #   shape = prior shape + cases in the window
  #   rate  = 1 / prior scale + cases one turn earlier (the infectors)
  # and EpiEstim's default prior (mean 5, sd 5: shape 1, scale 5).
  # Matches EpiEstim::estimate_R(method = "non_parametric_si",
  # si_distr = c(0, 1)) exactly, without the package.
  prior_shape <- 1
  prior_scale <- 5

  cases <- vapply(seq_along(t_start), function(i) sum(incid_vector[t_start[i]:t_end[i]]), numeric(1))
  infectors <- vapply(seq_along(t_start), function(i) sum(incid_vector[(t_start[i] - 1):(t_end[i] - 1)]), numeric(1))

  shape <- prior_shape + cases
  scale <- 1 / (1 / prior_scale + infectors)

  # too few infectors to judge: no estimate rather than one driven by the prior
  enough <- infectors >= min_cases

  tibble::tibble(
    t_end = t_end,
    mean = ifelse(enough, shape * scale, NA_real_),
    lower = ifelse(enough, stats::qgamma(0.025, shape = shape, scale = scale), NA_real_),
    upper = ifelse(enough, stats::qgamma(0.975, shape = shape, scale = scale), NA_real_)
  )
}

# Latest Rt estimate from a case series (see estimate_rt_table())
estimate_rt_series <- function(incid_vector, ...) {
  tbl <- estimate_rt_table(incid_vector, ...)
  if (nrow(tbl) == 0) {
    return(list(mean = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  last <- tbl[nrow(tbl), ]
  list(mean = last$mean, lower = last$lower, upper = last$upper)
}

# Positives per turn, with every turn present (0 where nothing came back)
positives_by_turn <- function(observed_history, last_turn = NULL) {
  turns <- observed_history$turn_available
  if (is.null(last_turn)) {
    last_turn <- if (length(turns) == 0) 0 else max(turns)
  }
  counts <- numeric(last_turn + 1)
  if (length(turns) > 0) {
    by_turn <- tapply(observed_history$positives, turns, sum)
    counts[as.integer(names(by_turn)) + 1] <- by_turn
  }
  counts
}

score_run <- function(truth_history, observed_history, pop_tbl, total_spend = 0) {
  if (length(truth_history) == 0) {
    stop("truth_history must contain at least one turn")
  }

  truth_mat <- do.call(rbind, truth_history)
  total_incidence <- rowSums(truth_mat)
  peak_true <- max(total_incidence)

  final_truth <- truth_mat[nrow(truth_mat), ]
  true_prev_rate <- sum(final_truth) / sum(pop_tbl$pop)

  # first REAL case found by the player's own tests (not the free starter
  # alert, not a false positive)
  own_tests <- observed_history
  if ("source" %in% names(own_tests)) {
    own_tests <- own_tests[is.na(own_tests$source) | own_tests$source != "starter", , drop = FALSE]
  }
  real_found <- if ("true_pos" %in% names(own_tests)) own_tests$true_pos else own_tests$positives
  detection_turns <- own_tests$turn_available[real_found > 0]
  time_to_detection <- if (length(detection_turns) == 0 || all(is.na(detection_turns))) {
    NA_integer_
  } else {
    min(detection_turns, na.rm = TRUE)
  }
  if (is.infinite(time_to_detection)) {
    time_to_detection <- NA_integer_
  }

  observed_turns <- observed_history |>
    dplyr::group_by(turn_available) |>
    dplyr::summarise(positives = sum(positives), .groups = "drop")
  peak_observed <- if (nrow(observed_turns) == 0) 0 else max(observed_turns$positives)

  total_tests <- sum(observed_history$n)
  total_pos <- sum(observed_history$positives)
  pct_positive <- if (total_tests == 0) NA_real_ else 100 * total_pos / total_tests

  n_turns <- nrow(truth_mat)
  n_boros <- ncol(truth_mat)

  truth_df <- tibble::tibble(
    turn = rep(seq_len(n_turns) - 1L, each = n_boros),
    borough_index = rep(seq_len(n_boros), times = n_turns),
    true_prev = as.numeric(t(truth_mat)) / rep(pop_tbl$pop, times = n_turns)
  )

  inferred_df <- observed_history |>
    dplyr::group_by(turn_sampled, borough_index) |>
    dplyr::summarise(
      tests = sum(n),
      positives = sum(positives),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      inferred = ifelse(tests == 0, 0, (positives + 1) / (tests + 2))
    ) |>
    dplyr::rename(turn = turn_sampled)

  calib_df <- truth_df |>
    dplyr::left_join(inferred_df, by = c("turn", "borough_index"))

  calib_df$inferred[is.na(calib_df$inferred)] <- 0
  calib_df$tests[is.na(calib_df$tests)] <- 0

  turn_metrics <- calib_df |>
    dplyr::group_by(turn) |>
    dplyr::summarise(
      rmse = sqrt(mean((true_prev - inferred)^2)),
      tests = sum(tests, na.rm = TRUE),
      .groups = "drop"
    )

  if (nrow(turn_metrics) == 0) {
    calibration_gap <- NA_real_
  } else if (sum(turn_metrics$tests, na.rm = TRUE) > 0) {
    calibration_gap <- stats::weighted.mean(turn_metrics$rmse, w = turn_metrics$tests, na.rm = TRUE)
  } else {
    calibration_gap <- mean(turn_metrics$rmse, na.rm = TRUE)
  }

  observed_rt <- estimate_rt_series(positives_by_turn(observed_history, last_turn = n_turns - 1))
  truth_rt <- estimate_rt_series(total_incidence)

  # how many real cases the player's tests found, and how many of their
  # positives were false
  real_cases_found <- if ("true_pos" %in% names(own_tests)) sum(own_tests$true_pos) else NA_real_
  own_positives <- sum(own_tests$positives)
  false_positives <- if (is.na(real_cases_found)) NA_real_ else own_positives - real_cases_found
  total_true_infections <- sum(total_incidence)

  list(
    time_to_detection = time_to_detection,
    real_cases_found = real_cases_found,
    share_of_infections_found = if (total_true_infections > 0) real_cases_found / total_true_infections else NA_real_,
    total_true_infections = total_true_infections,
    own_positives = own_positives,
    false_positives = false_positives,
    peak_true_incidence = peak_true,
    peak_observed_positives = peak_observed,
    percent_positive = pct_positive,
    total_spend = total_spend,
    calibration_gap = calibration_gap,
    true_prevalence_rate = true_prev_rate,
    rt_observed_mean = observed_rt$mean,
    rt_observed_lower = observed_rt$lower,
    rt_observed_upper = observed_rt$upper,
    rt_truth_mean = truth_rt$mean,
    rt_truth_lower = truth_rt$lower,
    rt_truth_upper = truth_rt$upper
  )
}

#' Assemble a run report containing metrics and turn-by-turn details
#'
#' @param truth_history List of integer vectors (incidence per turn).
#' @param observed_history Tibble of observed results (available outcomes).
#' @param pop_tbl Population tibble with `pop` column.
#' @param total_spend Numeric total spend accumulated across turns.
#' @param run_log Tibble of per-turn summaries captured during gameplay.
#' @param truth_log Truth log tibble (e.g., from `append_truth_log()`), used for derived stats like imports.
#' @return List with `metrics` and `turn_summary` tibble.
#' @export
build_run_report <- function(truth_history, observed_history, pop_tbl, total_spend = 0, run_log = tibble::tibble(), truth_log = tibble::tibble()) {
  metrics <- score_run(
    truth_history = truth_history,
    observed_history = observed_history,
    pop_tbl = pop_tbl,
    total_spend = total_spend
  )

  if (!is.null(truth_log) && nrow(truth_log) > 0 && "cumulative" %in% names(truth_log)) {
    final_turn <- max(truth_log$turn, na.rm = TRUE)
    final_cumulative <- truth_log |>
      dplyr::filter(turn == final_turn) |>
      dplyr::summarise(total_cumulative = sum(cumulative, na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(total_cumulative)
    pop_total <- sum(pop_tbl$pop, na.rm = TRUE)
    if (is.finite(final_cumulative) && is.finite(pop_total) && pop_total > 0) {
      metrics$true_prevalence_rate <- final_cumulative / pop_total
    }
  }

  if (!is.null(truth_log) && nrow(truth_log) > 0 && "cumulative_deaths" %in% names(truth_log)) {
    final_turn <- max(truth_log$turn, na.rm = TRUE)
    final_deaths <- truth_log |>
      dplyr::filter(turn == final_turn) |>
      dplyr::summarise(total_deaths = sum(cumulative_deaths, na.rm = TRUE), .groups = "drop") |>
      dplyr::pull(total_deaths)
    if (is.finite(final_deaths)) {
      metrics$total_deaths <- final_deaths
    }
  }

  truth_totals <- vapply(truth_history, sum, numeric(1))
  true_hotspots <- vapply(truth_history, function(x) sum(x > 0), integer(1))
  turns <- seq_along(truth_totals) - 1L

  truth_df <- tibble::tibble(
    turn = turns,
    truth_total = truth_totals,
    boroughs_with_cases = true_hotspots
  )

  observed_df <- observed_history |>
    dplyr::group_by(turn_available) |>
    dplyr::summarise(
      observed_tests = sum(n),
      observed_positives = sum(positives),
      .groups = "drop"
    ) |>
    dplyr::rename(turn = turn_available)

  imports_df <- if (!is.null(truth_log) && nrow(truth_log) > 0) {
    truth_log |>
      dplyr::group_by(turn) |>
      dplyr::summarise(imports_total = sum(imports), .groups = "drop")
  } else {
    tibble::tibble(turn = integer(), imports_total = integer())
  }

  deaths_df <- if (!is.null(truth_log) && nrow(truth_log) > 0 && "deaths" %in% names(truth_log)) {
    truth_log |>
      dplyr::group_by(turn) |>
      dplyr::summarise(deaths_total = sum(deaths), .groups = "drop")
  } else {
    tibble::tibble(turn = integer(), deaths_total = integer())
  }

  log_clean <- run_log
  if (nrow(log_clean) == 0) {
    log_clean <- tibble::tibble(turn = integer())
  } else {
    log_clean <- log_clean |>
      dplyr::select(-dplyr::any_of(c("truth_total", "observed_tests", "observed_positives", "imports_total")))
  }

  turn_summary <- truth_df |>
    dplyr::left_join(observed_df, by = "turn") |>
    dplyr::left_join(imports_df, by = "turn") |>
    dplyr::left_join(deaths_df, by = "turn") |>
    dplyr::left_join(log_clean, by = "turn") |>
    dplyr::arrange(turn)

  numeric_defaults <- c(
    observed_tests = 0,
    observed_positives = 0,
    imports_total = 0,
    deaths_total = 0,
    tests_ag = 0,
    tests_pcr = 0,
    allocation_spend = 0,
    intervention_cost = 0,
    total_turn_spend = 0,
    cumulative_spend = 0,
    queue_pending = 0
  )

  for (field in names(numeric_defaults)) {
    if (field %in% names(turn_summary)) {
      turn_summary[[field]][is.na(turn_summary[[field]])] <- numeric_defaults[[field]]
    }
  }

  if ("total_turn_spend" %in% names(turn_summary)) {
    turn_summary$cumulative_spend <- cumsum(turn_summary$total_turn_spend)
  }

  if ("active_interventions" %in% names(turn_summary)) {
    turn_summary$active_interventions[is.na(turn_summary$active_interventions) | turn_summary$active_interventions == ""] <- "None"
  }

  if ("notes" %in% names(turn_summary)) {
    turn_summary$notes[is.na(turn_summary$notes)] <- ""
  }

  list(
    metrics = metrics,
    turn_summary = turn_summary
  )
}
