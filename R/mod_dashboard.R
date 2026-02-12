# Dashboard module -----------------------------------------------------------

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    style = "height: 100%; overflow-y: auto;",
    bslib::card_header("Results Dashboard"),
    bslib::card_body(
      class = "p-2",
      shiny::div(
        class = "row g-2 mb-2",
        shiny::div(
        class = "col-4",
          shiny::div(
            class = "p-2 bg-light rounded text-center",
            shiny::tags$small(class = "text-muted", "Total Positives"),
            shiny::div(class = "fs-4 fw-bold text-danger", shiny::textOutput(ns("total_positives"), inline = TRUE))
          )
        ),
        shiny::div(
        class = "col-4",
          shiny::div(
            class = "p-2 bg-light rounded text-center",
            shiny::tags$small(class = "text-muted", "Pending Tests"),
            shiny::div(class = "fs-4 fw-bold text-primary", shiny::textOutput(ns("pending_count"), inline = TRUE))
          )
      ),
      shiny::div(
        class = "col-4",
          shiny::div(
            class = "p-2 bg-light rounded text-center",
            shiny::tags$small(class = "text-muted", "Cumulative Deaths"),
            shiny::div(class = "fs-4 fw-bold text-dark", shiny::textOutput(ns("reported_deaths"), inline = TRUE))
          )
        )
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
      shiny::hr(class = "my-2"),
      shiny::div(
        class = "small",
        shiny::verbatimTextOutput(ns("spend_text"))
      )
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
    app_ready = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
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

    output$spend_text <- shiny::renderPrint({
      req(ready())
      info <- spend_info()
      hist <- observed_history()

      total_tests <- if (is.null(hist) || nrow(hist) == 0) 0 else sum(hist$n)
      total_pos <- if (is.null(hist) || nrow(hist) == 0) 0 else sum(hist$positives)
      positivity <- if (total_tests == 0) NA else round(100 * total_pos / total_tests, 1)

      cat(
        "Cumulative spend: £", round(info$total, 0), "\n",
        "Positivity rate: ", if (is.na(positivity)) "-" else paste0(positivity, "%"), "\n",
        "Interventions: ", if (is.null(info$interventions) || info$interventions == "") "None" else info$interventions,
        sep = ""
      )
    })
  })
}
