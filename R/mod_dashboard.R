# Dashboard module -----------------------------------------------------------
#
# Everything a player can see: what happened last turn, this turn's numbers,
# and the history of positives, tests and hospital reports. Only returned
# test results and public reports are used (never the hidden truth).

kpi_card <- function(label, output_id, colour = "text-dark") {
  shiny::div(
    class = "col-6 col-xl-3",
    shiny::div(
      class = "p-2 bg-light rounded text-center h-100",
      shiny::tags$small(class = "text-muted d-block", label),
      shiny::div(class = paste("fs-5 fw-bold", colour), shiny::textOutput(output_id, inline = TRUE))
    )
  )
}

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    style = "height: 100%; overflow-y: auto;",
    bslib::card_header("What you can see"),
    bslib::card_body(
      class = "p-2",

      # what happened last turn (stays until the next turn)
      shiny::div(
        class = "p-2 mb-2 rounded small",
        style = "background: #fff8e1; border-left: 4px solid #f0ad4e;",
        shiny::htmlOutput(ns("turn_log"))
      ),

      # this turn's numbers
      shiny::div(
        class = "row g-2 mb-2",
        kpi_card("Positives back this turn", ns("positives_now"), "text-danger"),
        kpi_card("Share positive this turn", ns("positivity_now")),
        kpi_card("Hospital admissions this turn", ns("admissions_now")),
        kpi_card("Tests still waiting", ns("pending_count"), "text-primary")
      ),
      shiny::div(
        class = "small text-muted mb-2",
        shiny::textOutput(ns("totals_text"), inline = TRUE)
      ),

      shiny::tags$strong(class = "small", "Positives (bars) and hospital admissions (line) per turn"),
      shiny::div(style = "height: 170px;", shiny::plotOutput(ns("incidence_plot"), height = "100%")),
      shiny::tags$strong(class = "small", "Tests with results back per turn"),
      shiny::div(style = "height: 110px;", shiny::plotOutput(ns("tests_plot"), height = "100%")),
      shiny::hr(class = "my-2"),
      shiny::tags$strong(class = "small", "Estimated Rt from your positives (1 = flat)"),
      shiny::div(style = "height: 140px;", shiny::plotOutput(ns("rt_plot"), height = "100%"))
    )
  )
}

mod_dashboard_server <- function(
    id,
    observed_history,
    queue_state,
    spend_info = shiny::reactive(NULL),
    available_now,
    reports,
    turn_log = shiny::reactive(character()),
    system_status = shiny::reactive(NULL),
    app_ready = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ready <- shiny::reactive({
      isTRUE(app_ready())
    })

    # positives and tests per turn, every turn present (0 if nothing back)
    observed_series <- shiny::reactive({
      hist <- observed_history()
      if (is.null(hist) || nrow(hist) == 0) {
        return(tibble::tibble(turn = integer(), positives = numeric(), tests = numeric()))
      }
      last_turn <- max(hist$turn_available)
      positives <- numeric(last_turn + 1)
      tests <- numeric(last_turn + 1)
      by_turn <- hist |>
        dplyr::group_by(turn_available) |>
        dplyr::summarise(positives = sum(positives), tests = sum(n), .groups = "drop")
      positives[by_turn$turn_available + 1] <- by_turn$positives
      tests[by_turn$turn_available + 1] <- by_turn$tests
      tibble::tibble(turn = 0:last_turn, positives = positives, tests = tests)
    })

    # hospital admissions reported per turn
    admissions_series <- shiny::reactive({
      rep <- reports()
      if (is.null(rep) || nrow(rep) == 0) {
        return(tibble::tibble(turn = integer(), admissions = integer()))
      }
      rep |>
        dplyr::group_by(turn) |>
        dplyr::summarise(admissions = sum(admissions), .groups = "drop")
    })

    output$turn_log <- shiny::renderUI({
      req(ready())
      log <- turn_log()
      if (length(log) == 0) {
        return(shiny::HTML("Set your plan, then press <b>Advance turn</b>."))
      }
      shiny::HTML(paste(log, collapse = "<br>"))
    })

    # this turn's numbers ------------------------------------------------------

    output$positives_now <- shiny::renderText({
      req(ready())
      now <- available_now()
      positives <- if (is.null(now) || nrow(now) == 0) 0 else sum(now$positives)

      # change from the turn before, from the per-turn series
      series <- observed_series()
      change <- ""
      if (nrow(series) >= 2) {
        diff <- series$positives[nrow(series)] - series$positives[nrow(series) - 1]
        change <- if (diff > 0) paste0(" (+", diff, ")") else if (diff < 0) paste0(" (", diff, ")") else " (same)"
      }
      paste0(format(positives, big.mark = ","), change)
    })

    output$positivity_now <- shiny::renderText({
      req(ready())
      now <- available_now()
      if (is.null(now) || sum(now$n) == 0) return("-")
      paste0(round(100 * sum(now$positives) / sum(now$n), 1), "%")
    })

    output$admissions_now <- shiny::renderText({
      req(ready())
      adm <- admissions_series()
      if (nrow(adm) == 0) return("0")
      format(adm$admissions[nrow(adm)], big.mark = ",")
    })

    output$pending_count <- shiny::renderText({
      req(ready())
      queue <- queue_state()
      if (is.null(queue) || nrow(queue) == 0) return("0")
      format(sum(queue$n), big.mark = ",")
    })

    output$totals_text <- shiny::renderText({
      req(ready())
      hist <- observed_history()
      rep <- reports()
      total_pos <- if (is.null(hist) || nrow(hist) == 0) 0 else sum(hist$positives)
      total_tests <- if (is.null(hist) || nrow(hist) == 0) 0 else sum(hist$n)
      total_adm <- if (is.null(rep) || nrow(rep) == 0) 0 else sum(rep$admissions)
      total_deaths <- if (is.null(rep) || nrow(rep) == 0) 0 else sum(rep$deaths)
      paste0(
        "So far: ", format(total_pos, big.mark = ","), " positives from ",
        format(total_tests, big.mark = ","), " tests · ",
        format(total_adm, big.mark = ","), " admissions · ",
        format(total_deaths, big.mark = ","), " deaths reported"
      )
    })

    # charts -----------------------------------------------------------------

    # x axis always shows at least a few turns, so one bar is not a wall
    turn_axis <- function(max_turn) {
      ggplot2::scale_x_continuous(limits = c(-0.5, max(4, max_turn) + 0.5), breaks = scales::breaks_pretty(n = 5))
    }

    output$incidence_plot <- shiny::renderPlot({
      req(ready())
      series <- observed_series()
      admissions <- admissions_series()

      if (nrow(series) == 0 && nrow(admissions) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No results yet", color = "grey50") +
                 ggplot2::theme_void())
      }

      max_turn <- max(c(series$turn, admissions$turn, 0))
      ggplot2::ggplot() +
        ggplot2::geom_col(data = series, ggplot2::aes(x = turn, y = positives), fill = "#d95f02", alpha = 0.8, width = 0.8) +
        ggplot2::geom_line(data = admissions, ggplot2::aes(x = turn, y = admissions), color = "grey20", linewidth = 0.8) +
        ggplot2::geom_point(data = admissions, ggplot2::aes(x = turn, y = admissions), color = "grey20", size = 1.5) +
        turn_axis(max_turn) +
        ggplot2::labs(x = "Turn", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.margin = ggplot2::margin(5, 5, 5, 5))
    })

    # more tests means more positives: show the tests alongside
    output$tests_plot <- shiny::renderPlot({
      req(ready())
      series <- observed_series()
      if (nrow(series) == 0) {
        return(ggplot2::ggplot() + ggplot2::theme_void())
      }
      ggplot2::ggplot(series, ggplot2::aes(x = turn, y = tests)) +
        ggplot2::geom_col(fill = "#7f8c8d", alpha = 0.7, width = 0.8) +
        turn_axis(max(series$turn)) +
        ggplot2::scale_y_continuous(labels = scales::label_comma()) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.margin = ggplot2::margin(2, 5, 2, 5))
    })

    output$rt_plot <- shiny::renderPlot({
      req(ready())
      series <- observed_series()
      if (nrow(series) < 4) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Needs a few turns of results", color = "grey50", size = 3) +
                 ggplot2::theme_void())
      }

      # Rt with a 1-turn serial interval (one turn = one generation)
      rt <- estimate_rt_table(series$positives)
      rt <- rt[!is.na(rt$mean), ]
      if (nrow(rt) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Not enough positives yet", color = "grey50", size = 3) +
                 ggplot2::theme_void())
      }

      df <- tibble::tibble(
        turn = series$turn[rt$t_end],
        mean_R = rt$mean,
        lower = rt$lower,
        upper = rt$upper
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = turn, y = mean_R)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#7570b3", alpha = 0.2) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60", linewidth = 0.5) +
        turn_axis(max(series$turn)) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.margin = ggplot2::margin(2, 5, 2, 5))

      if (nrow(df) < 2) {
        p + ggplot2::geom_point(color = "#7570b3", size = 3)
      } else {
        p + ggplot2::geom_line(color = "#7570b3", linewidth = 1)
      }
    })
  })
}
