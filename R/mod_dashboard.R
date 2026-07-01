# Dashboard module -----------------------------------------------------------

compute_signal_quality_state <- function(
    observed_history,
    queue_state,
    current_turn,
    total_boroughs,
    window = 3L) {
  current_turn <- suppressWarnings(as.integer(current_turn)[1])
  if (!is.finite(current_turn) || is.na(current_turn) || current_turn < 0L) {
    current_turn <- 0L
  }

  total_boroughs <- suppressWarnings(as.integer(total_boroughs)[1])
  if (!is.finite(total_boroughs) || is.na(total_boroughs) || total_boroughs < 1L) {
    total_boroughs <- 33L
  }

  window <- suppressWarnings(as.integer(window)[1])
  if (!is.finite(window) || is.na(window) || window < 1L) {
    window <- 3L
  }

  history <- tibble::as_tibble(observed_history)
  if (!"borough_index" %in% names(history)) history$borough_index <- integer(nrow(history))
  if (!"n" %in% names(history)) history$n <- integer(nrow(history))
  if (!"turn_available" %in% names(history)) history$turn_available <- integer(nrow(history))
  if (!"source" %in% names(history)) history$source <- NA_character_

  player_history <- history |>
    dplyr::filter(is.na(source) | source != "starter_intel")

  if (nrow(history) > 0 && nrow(player_history) == 0) {
    return(list(
      state = "starter_only",
      headline = "Starter alert only",
      headline_class = "signal-starter",
      helper_text = "Wait for your own returned results before trusting the trend.",
      summary_text = NULL,
      rows = NULL,
      metrics = list(
        signal_quality_points = 0L,
        coverage_n = 0L,
        pending_tests = 0L,
        returned_tests_last_window = 0L,
        turns_with_results = 0L
      )
    ))
  }

  # Anchor the dashboard window on the last completed turn with observable returns.
  # The current planning turn has not yet produced its own results, so including it
  # would systematically undercount continuity by one.
  signal_turn <- max(0L, current_turn - 1L)
  window_start <- max(0L, signal_turn - (window - 1L))
  recent_history <- player_history |>
    dplyr::filter(turn_available >= window_start, turn_available <= signal_turn)

  pending_tbl <- tibble::as_tibble(queue_state)
  if (!"n" %in% names(pending_tbl)) pending_tbl$n <- integer(nrow(pending_tbl))
  pending_tests <- if (nrow(pending_tbl) == 0) 0L else as.integer(sum(pending_tbl$n, na.rm = TRUE))

  hint_for <- function(key, status) {
    if (identical(status, "Strong")) return(NULL)
    switch(
      key,
      coverage = "Broaden testing to reduce blind spots.",
      timeliness = "Backlog is starting to outpace returned results.",
      NULL
    )
  }

  row_item <- function(key, label, status, value_text) {
    list(
      key = key,
      label = label,
      status = status,
      status_class = paste0("status-", tolower(status)),
      value_text = value_text,
      hint = hint_for(key, status)
    )
  }

  coverage_n <- if (nrow(recent_history) == 0) 0L else dplyr::n_distinct(recent_history$borough_index)
  if (coverage_n >= 12L) {
    coverage_status <- "Strong"
    coverage_pts <- 3L
  } else if (coverage_n >= 6L) {
    coverage_status <- "Fair"
    coverage_pts <- 2L
  } else if (coverage_n >= 1L) {
    coverage_status <- "Weak"
    coverage_pts <- 1L
  } else {
    coverage_status <- "None"
    coverage_pts <- 0L
  }
  coverage_text <- paste0(coverage_n, "/", total_boroughs, " boroughs observed over last 3 completed turns")

  returned_tests_last_window <- if (nrow(recent_history) == 0) 0L else as.integer(sum(recent_history$n, na.rm = TRUE))
  if (returned_tests_last_window <= 0L) {
    if (pending_tests > 0L) {
      freshness_status <- "Weak"
      freshness_pts <- 1L
    } else {
      freshness_status <- "None"
      freshness_pts <- 0L
    }
  } else {
    freshness_ratio <- pending_tests / returned_tests_last_window
    if (freshness_ratio <= 0.5) {
      freshness_status <- "Strong"
      freshness_pts <- 3L
    } else if (freshness_ratio <= 1.5) {
      freshness_status <- "Fair"
      freshness_pts <- 2L
    } else {
      freshness_status <- "Weak"
      freshness_pts <- 1L
    }
  }
  timeliness_text <- paste0(pending_tests, " tests still pending")

  confidence_n <- returned_tests_last_window
  if (confidence_n >= 300L) {
    confidence_status <- "Strong"
    confidence_pts <- 3L
  } else if (confidence_n >= 120L) {
    confidence_status <- "Fair"
    confidence_pts <- 2L
  } else if (confidence_n >= 1L) {
    confidence_status <- "Weak"
    confidence_pts <- 1L
  } else {
    confidence_status <- "None"
    confidence_pts <- 0L
  }
  turns_with_results <- if (nrow(recent_history) == 0) 0L else recent_history |>
    dplyr::distinct(turn_available) |>
    nrow()
  if (turns_with_results >= 3L) {
    continuity_status <- "Strong"
    continuity_pts <- 3L
  } else if (turns_with_results >= 2L) {
    continuity_status <- "Fair"
    continuity_pts <- 2L
  } else if (turns_with_results >= 1L) {
    continuity_status <- "Weak"
    continuity_pts <- 1L
  } else {
    continuity_status <- "None"
    continuity_pts <- 0L
  }
  signal_quality_points <- coverage_pts + freshness_pts + confidence_pts + continuity_pts

  if (signal_quality_points >= 10L) {
    headline <- "Strong"
    headline_class <- "signal-strong"
  } else if (signal_quality_points >= 6L) {
    headline <- "Fair"
    headline_class <- "signal-fair"
  } else if (signal_quality_points >= 1L) {
    headline <- "Weak"
    headline_class <- "signal-weak"
  } else {
    headline <- "No signal yet"
    headline_class <- "signal-none"
  }

  if (signal_quality_points == 0L) {
    helper_text <- if (nrow(player_history) == 0) {
      "No returned results from your own testing yet."
    } else {
      "No recent returned results in the current signal window."
    }
    summary_text <- NULL
    rows <- NULL
    state <- "none"
  } else {
    helper_text <- NULL
    summary_text <- paste0(
      "Based on ", format(confidence_n, big.mark = ",", scientific = FALSE, trim = TRUE),
      " returned results across ", turns_with_results, " of the last ",
      window, " completed turns."
    )
    rows <- list(
      row_item("coverage", "Coverage", coverage_status, coverage_text),
      row_item("timeliness", "Timeliness", freshness_status, timeliness_text)
    )
    state <- "normal"
  }

  list(
    state = state,
    headline = headline,
    headline_class = headline_class,
    helper_text = helper_text,
    summary_text = summary_text,
    rows = rows,
    metrics = list(
      signal_quality_points = signal_quality_points,
      signal_turn = signal_turn,
      window_start = window_start,
      coverage_n = coverage_n,
      pending_tests = pending_tests,
      returned_tests_last_window = returned_tests_last_window,
      turns_with_results = turns_with_results
    )
  )
}

compute_batch_signal_summary <- function(
    observed_history,
    queue_state,
    current_turn,
    total_boroughs,
    run_status) {
  current_turn <- suppressWarnings(as.integer(current_turn)[1])
  if (!is.finite(current_turn) || is.na(current_turn) || current_turn < 0L) {
    current_turn <- 0L
  }

  total_boroughs <- suppressWarnings(as.integer(total_boroughs)[1])
  if (!is.finite(total_boroughs) || is.na(total_boroughs) || total_boroughs < 1L) {
    total_boroughs <- 33L
  }

  mode <- run_status$mode
  if (is.null(mode) || !identical(mode, "guided_auto")) {
    return(list(
      visible = FALSE,
      state = "hidden",
      label = "This batch",
      summary_text = NULL,
      metrics = list(
        signal_turn = max(0L, current_turn - 1L),
        batch_turns_requested = 0L,
        batch_turns_completed = 0L,
        batch_turns_used = 0L,
        batch_start = NA_integer_,
        coverage_n = 0L,
        returned_tests = 0L,
        pending_tests = 0L
      )
    ))
  }

  history <- tibble::as_tibble(observed_history)
  if (!"borough_index" %in% names(history)) history$borough_index <- integer(nrow(history))
  if (!"n" %in% names(history)) history$n <- integer(nrow(history))
  if (!"turn_available" %in% names(history)) history$turn_available <- integer(nrow(history))
  if (!"source" %in% names(history)) history$source <- NA_character_

  player_history <- history |>
    dplyr::filter(is.na(source) | source != "starter_intel")

  queue_tbl <- tibble::as_tibble(queue_state)
  if (!"n" %in% names(queue_tbl)) queue_tbl$n <- integer(nrow(queue_tbl))
  pending_tests <- if (nrow(queue_tbl) == 0) 0L else as.integer(sum(queue_tbl$n, na.rm = TRUE))

  signal_turn <- max(0L, current_turn - 1L)
  turns_requested <- suppressWarnings(as.integer(run_status$turns_requested)[1])
  turns_completed <- suppressWarnings(as.integer(run_status$turns_completed)[1])

  if (!is.finite(turns_requested) || is.na(turns_requested) || turns_requested < 1L) {
    turns_requested <- 0L
  }
  if (!is.finite(turns_completed) || is.na(turns_completed) || turns_completed < 0L) {
    turns_completed <- 0L
  }

  turns_used <- min(turns_requested, turns_completed, signal_turn + 1L)

  if (turns_used <= 0L) {
    return(list(
      visible = TRUE,
      state = "waiting",
      label = "This batch",
      summary_text = "No completed auto turns yet",
      metrics = list(
        signal_turn = signal_turn,
        batch_turns_requested = turns_requested,
        batch_turns_completed = turns_completed,
        batch_turns_used = 0L,
        batch_start = NA_integer_,
        coverage_n = 0L,
        returned_tests = 0L,
        pending_tests = pending_tests
      )
    ))
  }

  batch_start <- signal_turn - turns_used + 1L
  batch_history <- player_history |>
    dplyr::filter(turn_available >= batch_start, turn_available <= signal_turn)

  coverage_n <- if (nrow(batch_history) == 0) 0L else dplyr::n_distinct(batch_history$borough_index)
  returned_tests <- if (nrow(batch_history) == 0) 0L else as.integer(sum(batch_history$n, na.rm = TRUE))
  fmt_int <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE, trim = TRUE)

  list(
    visible = TRUE,
    state = "summary",
    label = "This batch",
    summary_text = paste0(
      turns_used, "/", turns_requested, " turns | ",
      coverage_n, "/", total_boroughs, " boroughs covered | ",
      fmt_int(returned_tests), " returns"
    ),
    metrics = list(
      signal_turn = signal_turn,
      batch_turns_requested = turns_requested,
      batch_turns_completed = turns_completed,
      batch_turns_used = turns_used,
      batch_start = batch_start,
      coverage_n = coverage_n,
      returned_tests = returned_tests,
      pending_tests = pending_tests
    )
  )
}

mod_dashboard_ui <- function(id, turn_ui = NULL) {
  ns <- shiny::NS(id)
  header_content <- if (!is.null(turn_ui)) {
    shiny::div(
      class = "dashboard-card-header-inner d-flex justify-content-between align-items-center w-100",
      shiny::span("Results Dashboard"),
      turn_ui
    )
  } else {
    "Results Dashboard"
  }
  bslib::card(
    style = "height: 100%; overflow-y: auto;",
    bslib::card_header(header_content),
    bslib::card_body(
      class = "p-2",
      shiny::div(
        class = "row g-2 mb-2",
        shiny::div(
        class = "col-4",
          shiny::div(
            class = "dashboard-stat-card dashboard-stat-card--positives",
            shiny::tags$div(class = "dashboard-stat-label", "Total Positive Tests"),
            shiny::div(class = "dashboard-stat-value dashboard-stat-value--positives", shiny::textOutput(ns("total_positives"), inline = TRUE))
          )
        ),
        shiny::div(
        class = "col-4",
          shiny::div(
            class = "dashboard-stat-card dashboard-stat-card--pending",
            shiny::tags$div(class = "dashboard-stat-label", "Pending Tests"),
            shiny::div(class = "dashboard-stat-value dashboard-stat-value--pending", shiny::textOutput(ns("pending_count"), inline = TRUE))
          )
      ),
      shiny::div(
        class = "col-4",
          shiny::div(
            class = "dashboard-stat-card dashboard-stat-card--deaths",
            shiny::tags$div(class = "dashboard-stat-label", "Cumulative Deaths"),
            shiny::div(class = "dashboard-stat-value dashboard-stat-value--deaths", shiny::textOutput(ns("reported_deaths"), inline = TRUE))
          )
        )
      ),
      shiny::div(
        class = "dashboard-feedback-stack",
        shiny::uiOutput(ns("signal_quality_panel")),
        shiny::uiOutput(ns("budget_mix_panel"))
      ),
      shiny::div(
        style = "height: 180px;",
        shiny::plotOutput(ns("incidence_plot"), height = "100%")
      ),
      shiny::hr(class = "my-2"),
      shiny::div(
        class = "small",
        shiny::tags$strong("Estimated Rt"),
        shiny::div(
          style = "height: 140px;",
          shiny::plotOutput(ns("rt_plot"), height = "100%")
        )
      ),
      shiny::uiOutput(ns("comparison_cards")),
      shiny::uiOutput(ns("debrief_prompts"))
    )
  )
}

mod_dashboard_server <- function(
    id,
    observed_history,
    queue_state,
    spend_info,
    available_now,
    reported_deaths,
    current_turn = shiny::reactive(0L),
    total_boroughs = 33L,
    planned_turn_mix = shiny::reactive(NULL),
    comparison_report = shiny::reactive(NULL),
    run_status = shiny::reactive(NULL),
    app_ready = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ready <- shiny::reactive({
      isTRUE(app_ready())
    })

    observed_series <- shiny::reactive({
      hist <- observed_history()
      if (is.null(hist) || nrow(hist) == 0) {
        return(tibble::tibble(turn = integer(), positives = integer()))
      }

      hist |>
        dplyr::group_by(turn = turn_available) |>
        dplyr::summarise(positives = sum(positives), .groups = "drop") |>
        dplyr::arrange(turn)
    })

    signal_quality_state <- shiny::reactive({
      compute_signal_quality_state(
        observed_history = observed_history(),
        queue_state = queue_state(),
        current_turn = current_turn(),
        total_boroughs = total_boroughs
      )
    })

    batch_signal_summary <- shiny::reactive({
      compute_batch_signal_summary(
        observed_history = observed_history(),
        queue_state = queue_state(),
        current_turn = current_turn(),
        total_boroughs = total_boroughs,
        run_status = run_status()
      )
    })

    budget_mix_state <- shiny::reactive({
      mix <- planned_turn_mix()
      if (is.null(mix) || !is.list(mix)) {
        return(list(
          testing_spend = 0,
          intervention_spend = 0,
          total_spend = 0,
          leftover = 0,
          testing_share = 0,
          intervention_share = 0,
          leftover_share = 1,
          over_cap = FALSE,
          cap_limit = 0,
          over_cap_by = 0,
          within_budget = TRUE,
          label = "No turn plan yet",
          label_class = "mix-none"
        ))
      }
      mix
    })

    output$signal_quality_panel <- shiny::renderUI({
      req(ready())
      state <- signal_quality_state()
      batch <- batch_signal_summary()
      rows <- state$rows

      row_ui <- if (is.null(rows) || length(rows) == 0) {
        NULL
      } else {
        lapply(rows, function(row) {
          shiny::div(
            class = "signal-quality-row",
            shiny::div(
              class = "signal-quality-row-top",
              shiny::div(class = "signal-quality-label", row$label),
              shiny::div(class = paste("signal-quality-status", row$status_class), row$status)
            ),
            shiny::div(class = "signal-quality-value", row$value_text)
          )
        })
      }

      shiny::div(
        class = "dashboard-inline-panel signal-quality-panel",
        shiny::div(
          class = "signal-quality-header",
          shiny::tags$strong("Recent signal"),
          shiny::span(
            class = paste("signal-quality-chip", state$headline_class),
            state$headline
          )
        ),
        if (!is.null(state$helper_text) && nzchar(state$helper_text)) {
          shiny::div(class = "signal-quality-helper", state$helper_text)
        },
        if (!is.null(row_ui)) {
          shiny::div(class = "signal-quality-rows", row_ui)
        },
        if (!is.null(state$summary_text) && nzchar(state$summary_text)) {
          shiny::div(class = "signal-quality-summary", state$summary_text)
        },
        if (isTRUE(batch$visible)) {
          shiny::div(
            class = paste("signal-quality-batch", paste0("signal-quality-batch--", batch$state)),
            shiny::div(class = "signal-quality-batch-label", batch$label),
            shiny::div(class = "signal-quality-batch-value", batch$summary_text)
          )
        }
      )
    })

    output$budget_mix_panel <- shiny::renderUI({
      req(ready())
      mix <- budget_mix_state()

      raw_shares <- c(
        intervention = max(mix$intervention_share, 0),
        testing = max(mix$testing_share, 0),
        leftover = max(mix$leftover_share, 0)
      )
      clamped_shares <- pmin(raw_shares, 1)
      total_share <- sum(clamped_shares)
      display_shares <- if (total_share > 1) clamped_shares / total_share else clamped_shares

      segment_ui <- list()
      if (mix$total_spend <= 0) {
        segment_ui <- list(
          shiny::div(
            class = "budget-mix-segment budget-mix-segment--leftover",
            style = "width: 100%;"
          )
        )
      } else {
        if (display_shares[["intervention"]] > 0) {
          segment_ui <- c(segment_ui, list(
            shiny::div(
              class = "budget-mix-segment budget-mix-segment--intervention",
              style = paste0("width: ", round(100 * display_shares[["intervention"]], 1), "%;")
            )
          ))
        }
        if (display_shares[["testing"]] > 0) {
          segment_ui <- c(segment_ui, list(
            shiny::div(
              class = "budget-mix-segment budget-mix-segment--testing",
              style = paste0("width: ", round(100 * display_shares[["testing"]], 1), "%;")
            )
          ))
        }
        if (display_shares[["leftover"]] > 0) {
          segment_ui <- c(segment_ui, list(
            shiny::div(
              class = "budget-mix-segment budget-mix-segment--leftover",
              style = paste0("width: ", round(100 * display_shares[["leftover"]], 1), "%;")
            )
          ))
        }
      }

      helper_text <- if (isTRUE(mix$over_cap)) {
        "Reduce interventions to at most 50% of turn budget."
      } else if (mix$total_spend <= 0) {
        "Add tests or interventions to preview the current turn mix."
      } else {
        NULL
      }

      shiny::div(
        class = "dashboard-inline-panel budget-mix-panel",
        shiny::div(
          class = "budget-mix-header",
          shiny::tags$strong("This turn's budget mix"),
          if (!identical(mix$label, "Balanced mix")) {
            shiny::span(class = paste("budget-mix-label", mix$label_class), mix$label)
          }
        ),
        shiny::div(class = "budget-mix-bar", segment_ui),
        shiny::div(
          class = "budget-mix-split",
          shiny::div(class = "budget-mix-split-primary", paste0(round(100 * max(mix$testing_share, 0)), "% testing")),
          shiny::div(class = "budget-mix-split-secondary", paste0(round(100 * max(mix$intervention_share, 0)), "% interventions"))
        ),
        if (!is.null(helper_text)) {
          shiny::div(
            class = "budget-mix-helper",
            helper_text
          )
        }
      )
    })

    output$total_positives <- shiny::renderText({
      req(ready())
      hist <- observed_history()
      if (is.null(hist) || nrow(hist) == 0) return("0")
      sum(hist$positives)
    })

    output$pending_count <- shiny::renderText({
      req(ready())
      queue <- queue_state()
      if (is.null(queue) || nrow(queue) == 0) return("0")
      sum(queue$n)
    })

    output$reported_deaths <- shiny::renderText({
      req(ready())
      deaths <- reported_deaths()
      if (is.null(deaths) || is.na(deaths)) return("0")
      round(deaths)
    })

    output$incidence_plot <- shiny::renderPlot({
      req(ready())
      series <- observed_series()
      if (nrow(series) == 0) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No results yet", color = "grey50") +
          ggplot2::theme_void()
      } else {
        ggplot2::ggplot(series, ggplot2::aes(x = turn, y = positives)) +
          ggplot2::geom_col(fill = "#d95f02", alpha = 0.8) +
          ggplot2::geom_line(color = "#d95f02", linewidth = 0.8) +
          ggplot2::geom_point(color = "#d95f02", size = 2) +
          ggplot2::labs(x = "Turn", y = "Positives") +
          ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 5)) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(5, 5, 5, 5)
          )
      }
    })

    output$rt_plot <- shiny::renderPlot({
      req(ready())
      series <- observed_series()
      if (nrow(series) < 3) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Need more data", color = "grey50", size = 3) +
                 ggplot2::theme_void())
      }

      incid_vector <- series$positives
      window_size <- min(5, length(incid_vector))
      if (window_size < 3) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Insufficient data", color = "grey50", size = 3) +
                 ggplot2::theme_void())
      }

      t_end <- seq(window_size, length(incid_vector))
      t_start <- pmax(t_end - window_size + 1, 2)
      valid <- t_start <= t_end
      t_start <- t_start[valid]
      t_end <- t_end[valid]

      if (!length(t_start)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Rt unavailable", color = "grey50", size = 3) +
                 ggplot2::theme_void())
      }

      config <- EpiEstim::make_config(list(
        mean_si = 3.5,
        std_si = 1.5,
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
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Rt unavailable", color = "grey50", size = 3) +
                 ggplot2::theme_void())
      }

      df <- tibble::tibble(
        turn = res$R$t_end,
        mean_R = res$R$`Mean(R)`,
        lower = res$R$`Quantile.0.025(R)`,
        upper = res$R$`Quantile.0.975(R)`
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = turn, y = mean_R)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#7570b3", alpha = 0.2) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60", linewidth = 0.5) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 4)) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(2, 2, 2, 2)
        )

      if (nrow(df) < 2) {
        p + ggplot2::geom_point(color = "#7570b3", size = 3)
      } else {
        p + ggplot2::geom_line(color = "#7570b3", linewidth = 1)
      }
    })

    output$comparison_cards <- shiny::renderUI({
      req(ready())
      report <- comparison_report()
      if (is.null(report) || is.null(report$comparison)) {
        return(NULL)
      }

      cmp <- report$comparison

      format_currency <- function(x, digits = 0) {
        if (is.null(x) || !is.finite(x) || is.na(x)) return("N/A")
        paste0("£", round(x, digits))
      }

      shiny::div(
        class = "mt-2",
        shiny::tags$h6("Baseline comparison"),
        shiny::div(
          class = "row g-2",
          shiny::div(
            class = "col-6",
            shiny::div(
              class = "p-2 bg-light rounded small",
              shiny::tags$strong("Cases averted: "),
              as.integer(round(cmp$cases_averted))
            )
          ),
          shiny::div(
            class = "col-6",
            shiny::div(
              class = "p-2 bg-light rounded small",
              shiny::tags$strong("Deaths averted: "),
              as.integer(round(cmp$deaths_averted))
            )
          ),
          shiny::div(
            class = "col-6",
            shiny::div(
              class = "p-2 bg-light rounded small",
              shiny::tags$strong("Cost per case averted: "),
              format_currency(cmp$cost_per_case_averted, digits = 1)
            )
          ),
          shiny::div(
            class = "col-6",
            shiny::div(
              class = "p-2 bg-light rounded small",
              shiny::tags$strong("Cost per death averted: "),
              format_currency(cmp$cost_per_death_averted, digits = 1)
            )
          )
        )
      )
    })

    output$debrief_prompts <- shiny::renderUI({
      req(ready())
      report <- comparison_report()
      if (is.null(report) || is.null(report$comparison)) {
        return(NULL)
      }

      shiny::div(
        class = "mt-2",
        shiny::tags$h6("Debrief prompts"),
        shiny::tags$ul(
          class = "small mb-0",
          shiny::tags$li("Surveillance vs control: Did your testing mostly improve visibility, or did interventions materially shift true cases/deaths versus baseline?"),
          shiny::tags$li("Budget trade-off: Which turns required sacrificing tests to fund interventions, and how did that change observed positivity confidence?"),
          shiny::tags$li("Interpretation discipline: Compare player and baseline trends across multiple turns, not single-turn spikes, before concluding strategy quality.")
        )
      )
    })
  })
}
