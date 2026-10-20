# Surveillance module ----------------------------------------------------------
#
# Everything the response team can see: warnings, the latest report, and one
# chart of three signals over time (positive tests, hospital admissions,
# deaths). Share positive and Rt are shown as numbers in the top bar, from
# signals() returned here. Only returned test results and public reports are
# used (never the hidden truth).

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Surveillance"),
    bslib::card_body(
      class = "p-2",

      # warnings
      shiny::htmlOutput(ns("warnings")),

      # the latest report (stays until the next one)
      shiny::div(
        class = "p-2 mb-2 rounded small",
        style = "background: var(--bs-tertiary-bg); border-left: 4px solid var(--bs-secondary-color);",
        shiny::htmlOutput(ns("turn_log"))
      ),

      # one chart: three signals on one log scale, so lines that run parallel
      # tell the same story and lines that pull apart disagree
      shiny::div(style = "height: 280px;", shiny::plotOutput(ns("signals_plot"), height = "100%")),
      shiny::div(class = "small text-muted", shiny::textOutput(ns("totals_text"), inline = TRUE))
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
    date_label = function(turn) turn,
    gens_per_turn = shiny::reactive(1),
    dark_mode = shiny::reactive(FALSE),
    app_ready = shiny::reactive(TRUE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ready <- shiny::reactive({
      isTRUE(app_ready())
    })

    # every signal per report (turn), every turn present -----------------------
    signals <- shiny::reactive({
      hist <- observed_history()
      rep <- reports()

      # test results taken on turn t come back with the report for turn t + 1,
      # alongside that report's hospital numbers, so they are plotted there
      # (plotting them at t left every chart ending on an empty report)
      last_turn <- max(c(0, hist$turn_available + 1, rep$turn))
      turns <- 0:last_turn
      confirmed <- numeric(length(turns))
      tests <- numeric(length(turns))
      suspected <- numeric(length(turns))
      deaths <- numeric(length(turns))

      if (!is.null(hist) && nrow(hist) > 0) {
        by_turn <- hist |>
          dplyr::group_by(turn_available) |>
          dplyr::summarise(positives = sum(positives), tests = sum(n), .groups = "drop")
        confirmed[by_turn$turn_available + 2] <- by_turn$positives
        tests[by_turn$turn_available + 2] <- by_turn$tests
      }
      if (!is.null(rep) && nrow(rep) > 0) {
        by_turn <- rep |>
          dplyr::group_by(turn) |>
          dplyr::summarise(admissions = sum(admissions), deaths = sum(deaths), .groups = "drop")
        suspected[by_turn$turn + 1] <- by_turn$admissions
        deaths[by_turn$turn + 1] <- by_turn$deaths
      }

      # Rt from confirmed cases: growth from one report to the next, turned
      # into growth per generation of infection (a report spans
      # gens_per_turn generations), so 1 still means flat
      rt <- rep(NA_real_, length(turns))
      rt_tbl <- estimate_rt_table(confirmed)
      if (nrow(rt_tbl) > 0) {
        rt[rt_tbl$t_end] <- rt_tbl$mean^(1 / gens_per_turn())
      }

      tibble::tibble(
        turn = turns,
        confirmed = confirmed,
        tests = tests,
        share_positive = ifelse(tests > 0, 100 * confirmed / tests, NA_real_),
        suspected = suspected,
        deaths = deaths,
        rt = rt
      )
    })

    output$turn_log <- shiny::renderUI({
      req(ready())
      log <- turn_log()
      if (length(log) == 0) {
        return(shiny::HTML("Set your response, then press <b>Advance to next report</b>."))
      }
      shiny::HTML(paste0("<b>", log[1], "</b><br>", paste(log[-1], collapse = "<br>")))
    })

    # warnings -----------------------------------------------------------------

    output$warnings <- shiny::renderUI({
      req(ready())
      s <- signals()
      status <- system_status()
      n <- nrow(s)
      found <- list()

      add <- function(level, text) {
        found[[length(found) + 1]] <<- list(level = level, text = text)
      }

      # rising fast: up by half since the last report, from a meaningful base
      rising <- function(now, before, minimum) {
        !is.na(now) && !is.na(before) && now >= minimum && now >= 1.5 * max(before, 1)
      }

      if (!is.null(status)) {
        if (status$bed_pressure >= 1) {
          add("danger", "Hospitals are full: patients without a bed are dying.")
        } else if (status$bed_pressure >= 0.8) {
          add("danger", sprintf("Hospital beds %d%% full.", round(100 * status$bed_pressure)))
        } else if (status$bed_pressure >= 0.5) {
          add("warning", sprintf("Hospital beds over half full (%d%%).", round(100 * status$bed_pressure)))
        }
        if (status$lab_backlog > 0) {
          add("warning", "Labs are behind: PCR results are delayed.")
        }
        if (!is.null(status$pot_share) && status$pot_share < 0.1) {
          add("danger", "Budget almost spent.")
        } else if (!is.null(status$pot_share) && status$pot_share < 0.25) {
          add("warning", "Budget running low.")
        }
      }

      if (n >= 2) {
        now <- s[n, ]
        before <- s[n - 1, ]
        if (rising(now$confirmed, before$confirmed, 10)) {
          add("warning", sprintf("Confirmed cases rising fast (%s to %s).", before$confirmed, now$confirmed))
        }
        if (rising(now$suspected, before$suspected, 5)) {
          add("warning", sprintf("Hospital admissions rising fast (%s to %s).", before$suspected, now$suspected))
        }
        if (now$deaths >= 3 && now$deaths > before$deaths) {
          add("danger", sprintf("Deaths rising (%s to %s).", before$deaths, now$deaths))
        }
        if (rising(now$share_positive, before$share_positive, 2)) {
          add("warning", sprintf("Share of tests positive rising (%.1f%% to %.1f%%).", before$share_positive, now$share_positive))
        }
        if (!is.na(now$rt) && now$rt > 1 && !is.na(before$rt) && now$rt > before$rt) {
          add(if (now$rt > 1.5) "danger" else "warning", sprintf("Rt above 1 and rising (%.2f).", now$rt))
        }
      }

      if (length(found) == 0) {
        return(shiny::div(class = "small text-success mb-2", "No warnings."))
      }
      shiny::div(
        class = "mb-2",
        lapply(found, function(w) {
          shiny::div(class = paste0("alert alert-", w$level, " py-1 px-2 mb-1 small"), w$text)
        })
      )
    })

    # one chart of three signals ----------------------------------------------

    output$signals_plot <- shiny::renderPlot({
      req(ready())
      s <- signals()

      series <- c(
        confirmed = "positive tests",
        suspected = "hospital admissions",
        deaths = "deaths"
      )
      series_colours <- c(
        "positive tests" = "#4fc3f7",
        "hospital admissions" = "#f6d55c",
        "deaths" = "#e15759"
      )
      long <- dplyr::bind_rows(lapply(names(series), function(m) {
        tibble::tibble(turn = s$turn, value = s[[m]], series = series[[m]])
      }))
      long$series <- factor(long$series, levels = series)

      # latest value and change since the report before, at the end of each line
      ends <- long |>
        dplyr::group_by(series) |>
        dplyr::arrange(turn, .by_group = TRUE) |>
        dplyr::summarise(
          turn = dplyr::last(turn),
          value = dplyr::last(value),
          change = if (dplyr::n() >= 2) dplyr::last(value) - dplyr::nth(value, -2) else NA_real_,
          .groups = "drop"
        )
      ends$label <- sprintf(
        "%s %s%s",
        format(round(ends$value), big.mark = ","), ends$series,
        ifelse(is.na(ends$change), "", sprintf(" (%s%s)", ifelse(ends$change >= 0, "+", ""), format(round(ends$change), big.mark = ",")))
      )

      max_turn <- max(4, max(s$turn))
      col <- chart_colours(dark_mode())

      # log scale that still shows zero: 0, 1, 10, 100, ...
      log_breaks <- function(lim) {
        b <- c(0, 10^(0:8))
        b[b <= max(lim)]
      }

      ggplot2::ggplot(long, ggplot2::aes(x = turn, y = value, colour = series)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::geom_point(size = 1.4) +
        ggplot2::geom_text(
          data = ends, ggplot2::aes(label = label),
          hjust = -0.05, size = 3.2, fontface = "bold", show.legend = FALSE
        ) +
        ggplot2::scale_colour_manual(values = series_colours, guide = "none") +
        ggplot2::scale_x_continuous(
          limits = c(0, max_turn + 3.5),
          breaks = function(lim) {
            b <- scales::breaks_pretty(n = 5)(c(0, max_turn))
            b[b >= 0 & b <= max_turn & b == round(b)]
          },
          labels = date_label
        ) +
        ggplot2::scale_y_continuous(
          trans = scales::pseudo_log_trans(sigma = 1, base = 10),
          breaks = log_breaks,
          labels = scales::label_comma(),
          expand = ggplot2::expansion(mult = c(0.02, 0.1))
        ) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Each report (log scale: parallel lines are growing at the same rate)"
        ) +
        chart_theme(dark_mode(), base_size = 10) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9, face = "plain", colour = col$muted),
          plot.margin = ggplot2::margin(2, 5, 2, 5)
        )
    }, bg = "transparent")

    output$totals_text <- shiny::renderText({
      req(ready())
      hist <- observed_history()
      rep <- reports()
      queue <- queue_state()
      total_pos <- if (is.null(hist) || nrow(hist) == 0) 0 else sum(hist$positives)
      total_tests <- if (is.null(hist) || nrow(hist) == 0) 0 else sum(hist$n)
      total_adm <- if (is.null(rep) || nrow(rep) == 0) 0 else sum(rep$admissions)
      total_deaths <- if (is.null(rep) || nrow(rep) == 0) 0 else sum(rep$deaths)
      waiting <- if (is.null(queue) || nrow(queue) == 0) 0 else sum(queue$n)
      paste0(
        "Totals so far: ", format(total_pos, big.mark = ","), " confirmed from ",
        format(total_tests, big.mark = ","), " tests · ",
        format(total_adm, big.mark = ","), " admissions · ",
        format(total_deaths, big.mark = ","), " deaths · ",
        format(waiting, big.mark = ","), " tests awaiting results"
      )
    })

    # share positive and Rt are shown in the top bar
    list(signals = signals)
  })
}
