# Dashboard module -----------------------------------------------------------

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = 1,
    bslib::card(
      bslib::card_header("Observed incidence"),
      shiny::plotOutput(ns("incidence_plot"), height = 240)
    ),
    bslib::card(
      bslib::card_header("Estimated Rt"),
      shiny::plotOutput(ns("rt_plot"), height = 240)
    ),
    bslib::card(
      bslib::card_header("Queues"),
      shiny::tableOutput(ns("queue_table"))
    ),
    bslib::card(
      bslib::card_header("Spend & positivity"),
      shiny::verbatimTextOutput(ns("spend_text")),
      shiny::verbatimTextOutput(ns("positivity_text"))
    )
  )
}

mod_dashboard_server <- function(
    id,
    observed_history,
    queue_state,
    spend_info,
    available_now) {
  shiny::moduleServer(id, function(input, output, session) {

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

    output$incidence_plot <- shiny::renderPlot({
      series <- observed_series()
      if (nrow(series) == 0) {
        ggplot2::ggplot() +
          ggplot2::labs(title = "No observed results yet") +
          ggplot2::theme_void()
      } else {
        ggplot2::ggplot(series, ggplot2::aes(x = turn, y = positives)) +
          ggplot2::geom_col(fill = "#1b9e77") +
          ggplot2::labs(x = "Turn", y = "Observed positives") +
          ggplot2::theme_minimal()
      }
    })

    output$rt_plot <- shiny::renderPlot({
      series <- observed_series()
      if (nrow(series) < 3) {
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Waiting for more observations") +
                 ggplot2::theme_void())
      }

      incid_vector <- series$positives
      window_size <- min(5, length(incid_vector))
      if (window_size < 3) {
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Rt unavailable") +
                 ggplot2::theme_void())
      }

      t_end <- seq(window_size, length(incid_vector))
      t_start <- pmax(t_end - window_size + 1, 2)
      valid <- t_start <= t_end
      t_start <- t_start[valid]
      t_end <- t_end[valid]

      if (!length(t_start)) {
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Rt unavailable") +
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
                 ggplot2::labs(title = "Rt unavailable") +
                 ggplot2::theme_void())
      }

      df <- tibble::tibble(
        turn = res$R$t_end,
        mean_R = res$R$`Mean(R)`,
        lower = res$R$`Quantile.0.025(R)`,
        upper = res$R$`Quantile.0.975(R)`
      )

      p <- ggplot2::ggplot(df, ggplot2::aes(x = turn, y = mean_R)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), fill = "#d95f02", alpha = 0.2) +
        ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
        ggplot2::labs(x = "Turn", y = "Estimated Rt") +
        ggplot2::theme_minimal()

      if (nrow(df) < 2) {
        p + ggplot2::geom_point(color = "#d95f02", size = 3)
      } else {
        p + ggplot2::geom_line(color = "#d95f02")
      }
    })

    output$queue_table <- shiny::renderTable({
      queue <- queue_state()
      if (is.null(queue) || nrow(queue) == 0) {
        return(data.frame(Message = "No pending results"))
      }

      queue |>
        dplyr::group_by(turn_available, type) |>
        dplyr::summarise(n = sum(n), .groups = "drop") |>
        dplyr::arrange(turn_available)
    })

    output$spend_text <- shiny::renderPrint({
      info <- spend_info()
      if (is.null(info)) {
        return(cat("No spend yet"))
      }

      cat(
        "Last turn spend: £", round(info$last_turn, 1),
        "\nCumulative spend: £", round(info$total, 1),
        "\nRemaining budget this turn: £", round(info$remaining, 1),
        "\nWithin budget: ", ifelse(isTRUE(info$within_budget), "yes", "no"),
        "\nActive interventions: ", if (is.null(info$interventions) || info$interventions == "") "None" else info$interventions,
        sep = ""
      )
    })

    output$positivity_text <- shiny::renderPrint({
      hist <- observed_history()
      if (is.null(hist) || nrow(hist) == 0) {
        return(cat("No observed tests yet"))
      }

      total_tests <- sum(hist$n)
      total_pos <- sum(hist$positives)

      turn_tests <- sum(available_now()$n)
      turn_pos <- sum(available_now()$positives)

      cat(
        "Overall positivity: ", round(100 * total_pos / total_tests, 1), "%\n",
        "This turn: ", if (turn_tests == 0) "no results" else paste0(round(100 * turn_pos / turn_tests, 1), "%"),
        sep = ""
      )
    })
  })
}
