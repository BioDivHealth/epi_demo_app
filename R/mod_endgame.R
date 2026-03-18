# End-game modal --------------------------------------------------------------

score_band_label <- function(score, max_score) {
  score <- suppressWarnings(as.numeric(score)[1])
  max_score <- suppressWarnings(as.numeric(max_score)[1])

  if (!is.finite(score) || !is.finite(max_score) || max_score <= 0) {
    return("Unavailable")
  }

  score_share <- score / max_score

  if (score_share >= 0.70) {
    return("Strong")
  }
  if (score_share >= 0.40) {
    return("Moderate")
  }

  "Weak"
}

score_band_class <- function(score, max_score) {
  label <- score_band_label(score, max_score)

  switch(
    label,
    Strong = "score-strong",
    Moderate = "score-moderate",
    Weak = "score-weak",
    "score-unavailable"
  )
}

format_score_fraction <- function(score, max_score) {
  score <- suppressWarnings(as.numeric(score)[1])
  max_score <- suppressWarnings(as.numeric(max_score)[1])

  if (!is.finite(score) || !is.finite(max_score) || max_score <= 0) {
    return("N/A")
  }

  paste0(as.integer(round(score)), "/", as.integer(round(max_score)))
}

format_turn_label <- function(turn_value, prefix = "Turn") {
  turn_value <- suppressWarnings(as.integer(turn_value)[1])

  if (is.na(turn_value)) {
    return("Never")
  }

  paste(prefix, turn_value)
}

build_response_verdict <- function(metrics) {
  get_metric <- function(name) {
    value <- metrics[[name]]
    suppressWarnings(as.numeric(value)[1])
  }

  outbreak_control_score <- get_metric("outbreak_control_score")
  surveillance_quality_score <- get_metric("surveillance_quality_score")
  strategy_balance_score <- get_metric("strategy_balance_score")

  if (!is.finite(outbreak_control_score) || !is.finite(surveillance_quality_score)) {
    return("Response score unavailable for this run.")
  }

  control_band <- if (outbreak_control_score >= 35) {
    "strong"
  } else if (outbreak_control_score >= 20) {
    "moderate"
  } else {
    "weak"
  }

  surveillance_band <- if (surveillance_quality_score >= 20) {
    "strong"
  } else if (surveillance_quality_score >= 15) {
    "moderate"
  } else {
    "weak"
  }

  base_sentence <- if (control_band == "strong" && surveillance_band == "strong") {
    "You combined strong outbreak control with a trustworthy surveillance picture."
  } else if (control_band == "strong" && surveillance_band == "moderate") {
    "You controlled the outbreak well and maintained a usable surveillance picture."
  } else if (control_band == "strong" && surveillance_band == "weak") {
    "Your interventions reduced harm, but surveillance was too thin to support confident decisions."
  } else if (control_band == "moderate" && surveillance_band == "strong") {
    "You built a strong picture of the outbreak, but only converted some of that visibility into control."
  } else if (control_band == "moderate" && surveillance_band == "moderate") {
    "A reasonably balanced response, with room to strengthen both control and surveillance."
  } else if (control_band == "moderate" && surveillance_band == "weak") {
    "The response achieved some control, but the surveillance picture remained thin."
  } else if (control_band == "weak" && surveillance_band == "strong") {
    "You built a good picture of the outbreak, but did not convert that visibility into enough control."
  } else if (control_band == "weak" && surveillance_band == "moderate") {
    "The surveillance picture was usable, but outcomes still lagged the baseline opportunity."
  } else {
    "The strategy was both hard to interpret and weak at reducing harm."
  }

  if (is.finite(strategy_balance_score) && strategy_balance_score < 8) {
    return(paste(base_sentence, "The testing and intervention mix was also poorly balanced."))
  }

  if (is.finite(strategy_balance_score) &&
      strategy_balance_score >= 15 &&
      !grepl("balanced", tolower(base_sentence), fixed = TRUE)) {
    return(paste(base_sentence, "Your testing and intervention choices stayed well balanced."))
  }

  base_sentence
}

build_endgame_modal <- function(metrics, comparison, detection_rate) {
  format_rt <- function(mean_rt, lower_rt, upper_rt) {
    if (is.na(mean_rt) || is.na(lower_rt) || is.na(upper_rt)) {
      "Rt unavailable"
    } else {
      sprintf("%.2f (%.2f\u2013%.2f)", mean_rt, lower_rt, upper_rt)
    }
  }

  metric_value <- function(name, default = NA_real_) {
    value <- metrics[[name]]
    if (is.null(value) || length(value) == 0) {
      return(default)
    }
    suppressWarnings(as.numeric(value)[1])
  }

  score_fill_pct <- function(score, max_score) {
    score <- suppressWarnings(as.numeric(score)[1])
    max_score <- suppressWarnings(as.numeric(max_score)[1])

    if (!is.finite(score) || !is.finite(max_score) || max_score <= 0) {
      return(0)
    }

    round(100 * max(0, min(score / max_score, 1)))
  }

  build_subscore_card <- function(id, label, score, max_score, note) {
    band_label <- score_band_label(score, max_score)
    band_class <- score_band_class(score, max_score)
    fill_pct <- score_fill_pct(score, max_score)

    tags$div(
      id = id,
      class = "endgame-subscore-card",
      tags$div(
        class = "endgame-subscore-value-row",
        tags$div(class = "endgame-subscore-label", label),
        tags$div(
          class = "endgame-subscore-value-wrap",
          tags$span(class = "endgame-subscore-value", format_score_fraction(score, max_score)),
          tags$span(class = paste("endgame-score-pill", band_class), band_label)
        )
      ),
      tags$div(
        class = "endgame-subscore-bar",
        tags$div(
          class = "endgame-subscore-bar-fill",
          style = paste0("width: ", fill_pct, "%;")
        )
      ),
      tags$div(class = "endgame-subscore-note", note)
    )
  }

  response_score <- metric_value("response_score")
  first_signal_turn <- metric_value("player_confirmation_turn", metric_value("first_signal_turn", metric_value("time_to_detection")))
  outbreak_control_score <- metric_value("outbreak_control_score")
  surveillance_quality_score <- metric_value("surveillance_quality_score")
  strategy_balance_score <- metric_value("strategy_balance_score")

  true_prev_pct <- if (is.na(metrics$true_prevalence_rate)) {
    "N/A"
  } else {
    sprintf("%.2f%%", 100 * metrics$true_prevalence_rate)
  }

  observed_rt_txt <- format_rt(
    metrics$rt_observed_mean,
    metrics$rt_observed_lower,
    metrics$rt_observed_upper
  )
  truth_rt_txt <- format_rt(
    metrics$rt_truth_mean,
    metrics$rt_truth_lower,
    metrics$rt_truth_upper
  )

  overall_score_label <- score_band_label(response_score, 100)
  overall_score_class <- score_band_class(response_score, 100)
  verdict_text <- build_response_verdict(metrics)
  response_score_display <- if (is.finite(response_score)) {
    as.integer(round(response_score))
  } else {
    "N/A"
  }

  modalDialog(
    title = tags$span(
      tags$i(class = "fa fa-eye me-2"),
      "Results"
    ),
    fluidRow(
      column(12,
        tags$div(
          id = "endgame-response_score_card",
          class = "endgame-score-card mb-3",
          tags$div(
            class = "endgame-score-header",
            tags$div(
              class = "endgame-score-title-group",
              tags$div(class = "endgame-score-kicker", "Response Score"),
              tags$div(
                class = "endgame-score-value-row",
                tags$span(
                  id = "endgame-response_score_value",
                  class = "endgame-score-value",
                  response_score_display
                ),
                tags$span(class = "endgame-score-total", "/100"),
                tags$span(class = paste("endgame-score-pill", overall_score_class), overall_score_label)
              ),
              tags$p(
                id = "endgame-response_verdict",
                class = "endgame-score-verdict",
                verdict_text
              )
            ),
            tags$div(
              class = "endgame-subscore-grid",
              build_subscore_card(
                id = "endgame-subscore-outbreak",
                label = "Outbreak control",
                score = outbreak_control_score,
                max_score = 50,
                note = "Compared with the baseline branch"
              ),
              build_subscore_card(
                id = "endgame-subscore-surveillance",
                label = "Surveillance quality",
                score = surveillance_quality_score,
                max_score = 30,
                note = "Based on confirmation, coverage, and ascertainment"
              ),
              build_subscore_card(
                id = "endgame-subscore-balance",
                label = "Strategy balance",
                score = strategy_balance_score,
                max_score = 20,
                note = "How well testing and interventions worked together"
              )
            )
          )
        )
      )
    ),
    fluidRow(
      class = "endgame-detail-row",
      column(6,
        tags$div(
          class = "card bg-light endgame-detail-card",
          tags$div(class = "card-header", tags$strong("Your Results")),
          tags$div(
            class = "card-body",
            tags$p(
              tags$strong("First signal: "),
              format_turn_label(first_signal_turn)
            ),
            tags$p(tags$strong("Peak observed: "), metrics$peak_observed_positives, " positives"),
            tags$p(tags$strong("Total spend: "), paste0("\u00a3", round(metrics$total_spend, 0))),
            tags$p(
              tags$strong("Detection rate: "),
              ifelse(is.na(detection_rate), "N/A", paste0(detection_rate, "% of true cases"))
            ),
            tags$p(tags$strong("Estimated Rt (observed): "), observed_rt_txt)
          )
        )
      ),
      column(6,
        tags$div(
          class = "card bg-warning-subtle endgame-detail-card",
          tags$div(class = "card-header", tags$strong("True Data")),
          tags$div(
            class = "card-body",
            tags$p(tags$strong("Peak true incidence: "), metrics$peak_true_incidence, " cases"),
            tags$p(tags$strong("Cumulative incidence by final turn: "), true_prev_pct),
            tags$p(tags$strong("Rt from truth incidence: "), truth_rt_txt)
          )
        )
      )
    ),
    fluidRow(
      column(12,
        tags$div(
          class = "card bg-info-subtle mb-3",
          tags$div(class = "card-header", tags$strong("Player vs Baseline")),
          tags$div(
            class = "card-body",
            tags$p(tags$strong("Cases averted: "), as.integer(round(comparison$cases_averted))),
            tags$p(tags$strong("Deaths averted: "), as.integer(round(comparison$deaths_averted))),
            tags$p(
              tags$strong("Peak delay: "),
              if (is.na(comparison$peak_delay_turns)) "N/A" else paste0(comparison$peak_delay_turns, " turns")
            ),
            tags$p(
              tags$strong("Spend delta (player - baseline): "),
              if (is.na(comparison$spend_delta)) "N/A" else paste0("\u00a3", round(comparison$spend_delta, 0))
            )
          )
        )
      )
    ),
    fluidRow(
      column(12,
        tags$div(
          class = "card bg-light mb-3",
          tags$div(class = "card-header", tags$strong("Key Outcome Plots")),
          tags$div(
            class = "card-body",
            tags$div(
              class = "mb-3",
              tags$strong("Prevalence, Cumulative Cases, Interventions, and Testing Over Turns"),
              plotOutput("endgame_prevalence_plot", height = "980px")
            )
          )
        )
      )
    ),
    tags$hr(),
    tags$div(
      class = "text-center",
      actionButton("show_replay", "View Replay", class = "btn-primary btn-lg")
    ),
    footer = modalButton("Close"),
    size = "xl",
    easyClose = TRUE
  )
}
