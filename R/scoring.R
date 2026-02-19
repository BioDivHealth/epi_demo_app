# Scoring helpers ------------------------------------------------------------

#' Summarise performance at the end of a run
#'
#' @param truth_history List of integer vectors (incidence per turn).
#' @param observed_history Tibble of observed results (available outcomes).
#' @param pop_tbl Population tibble with `pop` column.
#' @param total_spend Numeric total spend accumulated across turns.
#' @return Named list of metrics.
#' @export
estimate_rt_series <- function(incid_vector, mean_si = 3.5, std_si = 1.5) {
  if (length(incid_vector) < 3 || all(is.na(incid_vector))) {
    return(list(mean = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  window_size <- min(5, length(incid_vector))
  if (window_size < 3) {
    return(list(mean = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  t_end <- seq(window_size, length(incid_vector))
  t_start <- pmax(t_end - window_size + 1, 2)
  valid <- t_start <= t_end
  t_start <- t_start[valid]
  t_end <- t_end[valid]

  if (!length(t_start)) {
    return(list(mean = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  config <- EpiEstim::make_config(list(
    mean_si = mean_si,
    std_si = std_si,
    t_start = t_start,
    t_end = t_end
  ))

  res <- suppressWarnings(suppressMessages(tryCatch(
    EpiEstim::estimate_R(
      incid = incid_vector,
      method = "parametric_si",
      config = config
    ),
    error = function(e) NULL
  )))

  if (is.null(res) || nrow(res$R) == 0) {
    return(list(mean = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  last_row <- res$R[nrow(res$R), ]
  list(
    mean = last_row$`Mean(R)`,
    lower = last_row$`Quantile.0.025(R)`,
    upper = last_row$`Quantile.0.975(R)`
  )
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

  detection_turns <- observed_history |>
    dplyr::filter(positives > 0) |>
    dplyr::pull(turn_available)
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

  observed_series <- observed_history |>
    dplyr::group_by(turn_available) |>
    dplyr::summarise(positives = sum(positives), .groups = "drop") |>
    dplyr::arrange(turn_available)

  observed_rt <- estimate_rt_series(observed_series$positives)
  truth_rt <- estimate_rt_series(total_incidence)

  list(
    time_to_detection = time_to_detection,
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

compute_comparison_metrics <- function(player_report, baseline_report) {
  player_turns <- player_report$turn_summary
  baseline_turns <- baseline_report$turn_summary

  player_total_cases <- if ("truth_total" %in% names(player_turns)) {
    sum(player_turns$truth_total, na.rm = TRUE)
  } else {
    0
  }
  baseline_total_cases <- if ("truth_total" %in% names(baseline_turns)) {
    sum(baseline_turns$truth_total, na.rm = TRUE)
  } else {
    0
  }

  player_total_deaths <- if (!is.null(player_report$metrics$total_deaths)) {
    as.numeric(player_report$metrics$total_deaths)
  } else if ("deaths_total" %in% names(player_turns)) {
    sum(player_turns$deaths_total, na.rm = TRUE)
  } else {
    0
  }
  baseline_total_deaths <- if (!is.null(baseline_report$metrics$total_deaths)) {
    as.numeric(baseline_report$metrics$total_deaths)
  } else if ("deaths_total" %in% names(baseline_turns)) {
    sum(baseline_turns$deaths_total, na.rm = TRUE)
  } else {
    0
  }

  player_peak_turn <- if ("truth_total" %in% names(player_turns) && nrow(player_turns) > 0) {
    player_turns$turn[[which.max(player_turns$truth_total)]]
  } else {
    NA_integer_
  }
  baseline_peak_turn <- if ("truth_total" %in% names(baseline_turns) && nrow(baseline_turns) > 0) {
    baseline_turns$turn[[which.max(baseline_turns$truth_total)]]
  } else {
    NA_integer_
  }

  cases_averted <- baseline_total_cases - player_total_cases
  deaths_averted <- baseline_total_deaths - player_total_deaths
  spend_delta <- player_report$metrics$total_spend - baseline_report$metrics$total_spend

  list(
    player_total_cases = player_total_cases,
    baseline_total_cases = baseline_total_cases,
    player_total_deaths = player_total_deaths,
    baseline_total_deaths = baseline_total_deaths,
    cases_averted = cases_averted,
    deaths_averted = deaths_averted,
    player_peak_turn = player_peak_turn,
    baseline_peak_turn = baseline_peak_turn,
    peak_delay_turns = player_peak_turn - baseline_peak_turn,
    spend_delta = spend_delta,
    cost_per_case_averted = if (cases_averted > 0) spend_delta / cases_averted else NA_real_,
    cost_per_death_averted = if (deaths_averted > 0) spend_delta / deaths_averted else NA_real_
  )
}

build_dual_branch_report <- function(player_branch, baseline_branch, pop_tbl) {
  player_report <- build_run_report(
    truth_history = player_branch$truth_history,
    observed_history = player_branch$observation_state$history,
    pop_tbl = pop_tbl,
    total_spend = player_branch$total_spend,
    run_log = player_branch$run_log,
    truth_log = player_branch$turn_logs$truth
  )

  baseline_report <- build_run_report(
    truth_history = baseline_branch$truth_history,
    observed_history = baseline_branch$observation_state$history,
    pop_tbl = pop_tbl,
    total_spend = baseline_branch$total_spend,
    run_log = baseline_branch$run_log,
    truth_log = baseline_branch$turn_logs$truth
  )

  comparison <- compute_comparison_metrics(player_report, baseline_report)

  turn_comparison <- dplyr::full_join(
    player_report$turn_summary |>
      dplyr::select(
        turn,
        player_truth_total = truth_total,
        player_deaths_total = deaths_total,
        player_observed_positives = observed_positives,
        player_total_turn_spend = total_turn_spend,
        player_cumulative_spend = cumulative_spend
      ),
    baseline_report$turn_summary |>
      dplyr::select(
        turn,
        baseline_truth_total = truth_total,
        baseline_deaths_total = deaths_total,
        baseline_observed_positives = observed_positives,
        baseline_total_turn_spend = total_turn_spend,
        baseline_cumulative_spend = cumulative_spend
      ),
    by = "turn"
  ) |>
    dplyr::arrange(turn)

  list(
    player = player_report,
    baseline = baseline_report,
    comparison = comparison,
    turn_comparison = turn_comparison
  )
}
