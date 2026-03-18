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

clamp_range <- function(x, lower = 0, upper = 1) {
  x <- as.numeric(x)
  pmin(pmax(x, lower), upper)
}

compute_first_signal_turn <- function(observed_history) {
  observed_history <- tibble::as_tibble(observed_history)
  required_cols <- c("positives", "turn_available")
  if (nrow(observed_history) == 0 || !all(required_cols %in% names(observed_history))) {
    return(NA_integer_)
  }

  detection_turns <- observed_history |>
    dplyr::filter(positives > 0) |>
    dplyr::pull(turn_available)

  if (length(detection_turns) == 0 || all(is.na(detection_turns))) {
    return(NA_integer_)
  }

  first_turn <- suppressWarnings(as.integer(min(detection_turns, na.rm = TRUE)))
  if (is.infinite(first_turn)) {
    return(NA_integer_)
  }

  first_turn
}

compute_player_confirmation_turn <- function(observed_history, min_turn = 1L) {
  observed_history <- tibble::as_tibble(observed_history)
  required_cols <- c("turn_available", "borough_index", "positives")
  if (nrow(observed_history) == 0 || !all(required_cols %in% names(observed_history))) {
    return(NA_integer_)
  }

  eligible <- observed_history |>
    dplyr::filter(turn_available >= as.integer(min_turn))

  if ("source" %in% names(eligible)) {
    eligible <- eligible |>
      dplyr::filter(is.na(source) | source != "starter_intel")
  }

  if (nrow(eligible) == 0) {
    return(NA_integer_)
  }

  turn_summary <- eligible |>
    dplyr::group_by(turn_available, borough_index) |>
    dplyr::summarise(positives = sum(positives, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(turn_available) |>
    dplyr::summarise(
      positive_boroughs = sum(positives > 0, na.rm = TRUE),
      total_positives = sum(positives, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(positive_boroughs >= 2L | total_positives >= 3L)

  if (nrow(turn_summary) == 0) {
    return(NA_integer_)
  }

  as.integer(min(turn_summary$turn_available, na.rm = TRUE))
}

compute_mean_coverage_share <- function(observed_history, total_boroughs, min_turn = 1L) {
  observed_history <- tibble::as_tibble(observed_history)
  total_boroughs <- suppressWarnings(as.numeric(total_boroughs)[1])
  required_cols <- c("turn_available", "borough_index")

  if (!is.finite(total_boroughs) || total_boroughs <= 0 || nrow(observed_history) == 0 ||
      !all(required_cols %in% names(observed_history))) {
    return(0)
  }

  coverage_tbl <- observed_history |>
    dplyr::filter(turn_available >= as.integer(min_turn))

  if ("source" %in% names(coverage_tbl)) {
    coverage_tbl <- coverage_tbl |>
      dplyr::filter(is.na(source) | source != "starter_intel")
  }

  coverage_tbl <- coverage_tbl |>
    dplyr::group_by(turn_available) |>
    dplyr::summarise(
      boroughs_observed = dplyr::n_distinct(borough_index),
      .groups = "drop"
    )

  if (nrow(coverage_tbl) == 0) {
    return(0)
  }

  mean(coverage_tbl$boroughs_observed / total_boroughs)
}

score_player_confirmation <- function(turn_value) {
  turn_value <- suppressWarnings(as.integer(turn_value)[1])
  if (is.na(turn_value) || turn_value < 1L) {
    return(0L)
  }

  if (turn_value <= 5L) {
    return(10L)
  }
  if (turn_value <= 10L) {
    return(8L)
  }
  if (turn_value <= 15L) {
    return(6L)
  }
  if (turn_value <= 20L) {
    return(4L)
  }

  2L
}

score_coverage_share <- function(coverage_share) {
  coverage_share <- suppressWarnings(as.numeric(coverage_share)[1])
  if (!is.finite(coverage_share)) {
    return(0L)
  }

  as.integer(round(10 * clamp_range(coverage_share / 0.40, 0, 1)))
}

score_ascertainment_lift <- function(player_detected_fraction, baseline_detected_fraction) {
  player_detected_fraction <- suppressWarnings(as.numeric(player_detected_fraction)[1])
  baseline_detected_fraction <- suppressWarnings(as.numeric(baseline_detected_fraction)[1])

  if (!is.finite(player_detected_fraction) || player_detected_fraction <= 0) {
    return(0L)
  }

  if (!is.finite(baseline_detected_fraction) || baseline_detected_fraction < 0) {
    baseline_detected_fraction <- 0
  }

  ascertainment_lift <- player_detected_fraction / max(0.0001, baseline_detected_fraction)

  if (ascertainment_lift >= 4) {
    return(10L)
  }
  if (ascertainment_lift >= 3) {
    return(8L)
  }
  if (ascertainment_lift >= 2) {
    return(6L)
  }
  if (ascertainment_lift >= 1.5) {
    return(4L)
  }
  if (ascertainment_lift > 1) {
    return(2L)
  }

  0L
}

score_strategy_balance <- function(outbreak_control_score, surveillance_quality_score, intervention_share) {
  outbreak_control_score <- suppressWarnings(as.numeric(outbreak_control_score)[1])
  surveillance_quality_score <- suppressWarnings(as.numeric(surveillance_quality_score)[1])
  intervention_share <- suppressWarnings(as.numeric(intervention_share)[1])

  control_band <- if (is.finite(outbreak_control_score) && outbreak_control_score >= 35) {
    "strong"
  } else if (is.finite(outbreak_control_score) && outbreak_control_score >= 20) {
    "moderate"
  } else {
    "weak"
  }

  surveillance_band <- if (is.finite(surveillance_quality_score) && surveillance_quality_score >= 20) {
    "strong"
  } else if (is.finite(surveillance_quality_score) && surveillance_quality_score >= 15) {
    "moderate"
  } else {
    "weak"
  }

  base_score <- dplyr::case_when(
    control_band == "strong" && surveillance_band == "strong" ~ 20,
    control_band == "strong" && surveillance_band == "moderate" ~ 17,
    control_band == "moderate" && surveillance_band == "strong" ~ 17,
    control_band == "moderate" && surveillance_band == "moderate" ~ 15,
    control_band == "strong" && surveillance_band == "weak" ~ 10,
    control_band == "weak" && surveillance_band == "strong" ~ 10,
    control_band == "moderate" && surveillance_band == "weak" ~ 8,
    control_band == "weak" && surveillance_band == "moderate" ~ 8,
    TRUE ~ 3
  )

  if (!is.finite(intervention_share)) {
    intervention_share <- 0
  }

  if (intervention_share > 0.55 && surveillance_quality_score < 15) {
    base_score <- base_score - 3
  }
  if (intervention_share < 0.10 && outbreak_control_score < 20) {
    base_score <- base_score - 3
  }

  as.integer(round(clamp_range(base_score, 0, 20)))
}

build_response_score_metrics <- function(
    player_report,
    baseline_report,
    comparison,
    pop_tbl,
    player_observed_history = tibble::tibble()) {
  player_turns <- tibble::as_tibble(player_report$turn_summary)
  baseline_turns <- tibble::as_tibble(baseline_report$turn_summary)

  player_total_true_cases <- if ("truth_total" %in% names(player_turns)) {
    sum(player_turns$truth_total, na.rm = TRUE)
  } else {
    0
  }
  baseline_total_true_cases <- if ("truth_total" %in% names(baseline_turns)) {
    sum(baseline_turns$truth_total, na.rm = TRUE)
  } else {
    0
  }

  player_total_observed_positives <- if ("observed_positives" %in% names(player_turns)) {
    sum(player_turns$observed_positives, na.rm = TRUE)
  } else {
    0
  }
  baseline_total_observed_positives <- if ("observed_positives" %in% names(baseline_turns)) {
    sum(baseline_turns$observed_positives, na.rm = TRUE)
  } else {
    0
  }

  total_intervention_spend_player <- if ("intervention_cost" %in% names(player_turns)) {
    sum(player_turns$intervention_cost, na.rm = TRUE)
  } else {
    0
  }
  total_testing_spend_player <- if ("allocation_spend" %in% names(player_turns)) {
    sum(player_turns$allocation_spend, na.rm = TRUE)
  } else {
    0
  }

  player_detected_fraction <- player_total_observed_positives / max(1, player_total_true_cases)
  baseline_detected_fraction <- baseline_total_observed_positives / max(1, baseline_total_true_cases)
  ascertainment_lift <- if (player_detected_fraction > 0) {
    player_detected_fraction / max(0.0001, baseline_detected_fraction)
  } else {
    0
  }

  player_observed_history <- tibble::as_tibble(player_observed_history)
  total_boroughs <- nrow(tibble::as_tibble(pop_tbl))
  first_signal_turn <- compute_first_signal_turn(player_observed_history)
  player_confirmation_turn <- compute_player_confirmation_turn(player_observed_history, min_turn = 1L)
  mean_coverage_share <- compute_mean_coverage_share(player_observed_history, total_boroughs, min_turn = 1L)

  case_reduction_pct <- clamp_range(comparison$cases_averted / max(1, comparison$baseline_total_cases), 0, 1)
  death_reduction_pct <- clamp_range(comparison$deaths_averted / max(1, comparison$baseline_total_deaths), 0, 1)

  outbreak_control_score <- as.integer(round(
    50 * ((0.4 * case_reduction_pct) + (0.6 * death_reduction_pct))
  ))

  player_confirmation_score <- score_player_confirmation(player_confirmation_turn)
  coverage_score <- score_coverage_share(mean_coverage_share)
  ascertainment_lift_score <- score_ascertainment_lift(player_detected_fraction, baseline_detected_fraction)

  surveillance_quality_score <- as.integer(
    player_confirmation_score +
      coverage_score +
      ascertainment_lift_score
  )

  intervention_share <- total_intervention_spend_player / max(1, player_report$metrics$total_spend)
  strategy_balance_score <- score_strategy_balance(
    outbreak_control_score = outbreak_control_score,
    surveillance_quality_score = surveillance_quality_score,
    intervention_share = intervention_share
  )

  response_score <- as.integer(
    outbreak_control_score +
      surveillance_quality_score +
      strategy_balance_score
  )

  list(
    response_score = response_score,
    outbreak_control_score = outbreak_control_score,
    surveillance_quality_score = surveillance_quality_score,
    strategy_balance_score = strategy_balance_score,
    first_signal_turn = first_signal_turn,
    player_confirmation_turn = player_confirmation_turn,
    mean_coverage_share = mean_coverage_share,
    player_confirmation_score = player_confirmation_score,
    coverage_score = coverage_score,
    ascertainment_lift_score = ascertainment_lift_score,
    total_intervention_spend_player = total_intervention_spend_player,
    total_testing_spend_player = total_testing_spend_player,
    player_total_observed_positives = player_total_observed_positives,
    baseline_total_observed_positives = baseline_total_observed_positives,
    player_detected_fraction = player_detected_fraction,
    baseline_detected_fraction = baseline_detected_fraction,
    ascertainment_lift = ascertainment_lift,
    intervention_share = intervention_share
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

  time_to_detection <- compute_first_signal_turn(observed_history)

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

  if ("intervention_statuses" %in% names(turn_summary)) {
    turn_summary$intervention_statuses[is.na(turn_summary$intervention_statuses) | turn_summary$intervention_statuses == ""] <- "None"
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
  response_metrics <- build_response_score_metrics(
    player_report = player_report,
    baseline_report = baseline_report,
    comparison = comparison,
    pop_tbl = pop_tbl,
    player_observed_history = player_branch$observation_state$history
  )

  player_report$metrics <- c(player_report$metrics, response_metrics[c(
    "response_score",
    "outbreak_control_score",
    "surveillance_quality_score",
    "strategy_balance_score",
    "first_signal_turn",
    "player_confirmation_turn",
    "mean_coverage_share"
  )])

  comparison <- c(comparison, response_metrics[c(
    "total_intervention_spend_player",
    "total_testing_spend_player",
    "player_total_observed_positives",
    "baseline_total_observed_positives",
    "player_detected_fraction",
    "baseline_detected_fraction",
    "ascertainment_lift"
  )])

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
