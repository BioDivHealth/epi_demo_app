# Scoring helpers ------------------------------------------------------------

#' Summarise performance at the end of a run
#'
#' @param truth_history List of integer vectors (incidence per turn).
#' @param observed_history Tibble of observed results (available outcomes).
#' @param pop_tbl Population tibble with `pop` column.
#' @param total_spend Numeric total spend accumulated across turns.
#' @return Named list of metrics.
#' @export
score_run <- function(truth_history, observed_history, pop_tbl, total_spend = 0) {
  if (length(truth_history) == 0) {
    stop("truth_history must contain at least one turn")
  }

  truth_mat <- do.call(rbind, truth_history)
  total_incidence <- rowSums(truth_mat)
  peak_true <- max(total_incidence)

  final_truth <- truth_mat[nrow(truth_mat), ]
  true_prev <- final_truth / pop_tbl$pop

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

  inferred_prev <- observed_history |>
    dplyr::group_by(borough_index) |>
    dplyr::summarise(
      tests = sum(n),
      positives = sum(positives),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      inferred = ifelse(tests == 0, 0, (positives + 1) / (tests + 2))
    )

  inferred_prev <- inferred_prev$inferred
  if (length(inferred_prev) < length(true_prev)) {
    inferred_prev <- c(inferred_prev, rep(0, length(true_prev) - length(inferred_prev)))
  }

  calibration_gap <- sqrt(mean((true_prev - inferred_prev)^2))

  list(
    time_to_detection = time_to_detection,
    peak_true_incidence = peak_true,
    peak_observed_positives = peak_observed,
    percent_positive = pct_positive,
    total_spend = total_spend,
    calibration_gap = calibration_gap
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
    dplyr::left_join(log_clean, by = "turn") |>
    dplyr::arrange(turn)

  numeric_defaults <- c(
    observed_tests = 0,
    observed_positives = 0,
    imports_total = 0,
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
