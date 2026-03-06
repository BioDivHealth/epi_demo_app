# End-game modal --------------------------------------------------------------

build_endgame_modal <- function(metrics, comparison, detection_rate) {
  format_rt <- function(mean_rt, lower_rt, upper_rt) {
    if (is.na(mean_rt) || is.na(lower_rt) || is.na(upper_rt)) {
      "Rt unavailable"
    } else {
      sprintf("%.2f (%.2f\u2013%.2f)", mean_rt, lower_rt, upper_rt)
    }
  }

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

  modalDialog(
    title = tags$span(
      tags$i(class = "fa fa-eye me-2"),
      "True Data Revealed"
    ),
    fluidRow(
      column(6,
        tags$div(
          class = "card bg-light mb-3",
          tags$div(class = "card-header", tags$strong("Your Results")),
          tags$div(
            class = "card-body",
            tags$p(
              tags$strong("Time to first detection: "),
              ifelse(is.na(metrics$time_to_detection), "Never detected!", paste("Turn", metrics$time_to_detection))
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
          class = "card bg-warning-subtle mb-3",
          tags$div(class = "card-header", tags$strong("True Data")),
          tags$div(
            class = "card-body",
            tags$p(tags$strong("Peak true incidence: "), metrics$peak_true_incidence, " cases"),
            tags$p(tags$strong("Calibration gap: "), sprintf("%.3f RMSE", metrics$calibration_gap)),
            tags$p(tags$strong("True cumulative prevalence (final turn): "), true_prev_pct),
            tags$p(tags$strong("Rt from truth incidence: "), truth_rt_txt),
            tags$p(
              class = "text-muted small",
              "The calibration gap measures how far your inferred prevalence was from the truth."
            )
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
              tags$strong("Prevalence, Cumulative Cases, and Interventions Over Turns"),
              plotOutput("endgame_prevalence_plot", height = "720px")
            ),
            tags$div(
              tags$strong("Testing Allocation Over Turns"),
              plotOutput("endgame_testing_plot", height = "280px")
            )
          )
        )
      )
    ),
    tags$hr(),
    tags$h5("Turn-by-Turn Branch Comparison"),
    tags$div(
      style = "max-height: 300px; overflow-y: auto;",
      tableOutput("run_summary_table")
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
