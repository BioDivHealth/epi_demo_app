# Replay module -------------------------------------------------------------

mod_replay_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Turn-by-turn replay"),
    shiny::uiOutput(ns("notice")),
    shiny::div(
      class = "d-flex flex-wrap gap-3 align-items-center mb-3",
      shiny::sliderInput(
        inputId = ns("turn_slider"),
        label = "Turn",
        min = 0,
        max = 0,
        value = 0,
        step = 1,
        width = "300px"
      ),
      shiny::actionButton(
        inputId = ns("play_toggle"),
        label = "Play",
        class = "btn-secondary"
      ),
      shiny::sliderInput(
        inputId = ns("play_delay"),
        label = "Seconds per turn",
        min = 0.5,
        max = 4,
        value = 2,
        step = 0.25,
        width = "220px"
      ),
      shiny::selectInput(
        inputId = ns("truth_metric"),
        label = "Truth metric",
        choices = c(
          "Active infections" = "active",
          "New infections" = "new"
        ),
        selected = "active",
        width = "220px"
      ),
      shiny::sliderInput(
        inputId = ns("infectious_period"),
        label = "Infectious period (turns)",
        min = 1,
        max = 7,
        value = 3,
        step = 1,
        width = "220px"
      ),
      shiny::checkboxInput(
        inputId = ns("show_actions"),
        label = "Show allocation markers",
        value = TRUE
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::div(class = "fw-bold mb-1", "Observed results & actions"),
        leaflet::leafletOutput(ns("map_observed"), height = 380)
      ),
      shiny::column(
        width = 6,
        shiny::div(class = "fw-bold mb-1", "Hidden truth (revealed after ending the run)"),
        leaflet::leafletOutput(ns("map_truth"), height = 380)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::plotOutput(ns("incidence_plot"), height = 250)
      ),
      shiny::column(
        width = 6,
        shiny::plotOutput(ns("testing_plot"), height = 250)
      )
    ),
    shiny::div(
      class = "mt-3",
      shiny::tableOutput(ns("actions_table"))
    )
  )
}

mod_replay_server <- function(id, logs, boros, ended) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    play_state <- shiny::reactiveVal(FALSE)
    has_ended <- shiny::reactive({
      isTRUE(ended())
    })

    borough_names <- boros$borough_name
    if (is.null(borough_names)) {
      borough_names <- as.character(seq_len(nrow(boros)))
    }
    centroids <- sf::st_geometry(boros) |>
      sf::st_centroid(of_largest_polygon = TRUE) |>
      sf::st_coordinates()
    bbox <- sf::st_bbox(boros)
    bounds_list <- list(
      c(bbox[["ymin"]] - 0.02, bbox[["xmin"]] - 0.02),
      c(bbox[["ymax"]] + 0.02, bbox[["xmax"]] + 0.02)
    )
    reset_js <- sprintf(
      "function(btn, map){ map.fitBounds([[%.6f, %.6f],[%.6f, %.6f]]); }",
      bounds_list[[1]][1], bounds_list[[1]][2],
      bounds_list[[2]][1], bounds_list[[2]][2]
    )

    centroid_tbl <- tibble::tibble(
      borough_index = seq_len(nrow(boros)),
      borough_name = borough_names,
      lng = centroids[, 1],
      lat = centroids[, 2]
    )

    sync_guard <- shiny::reactiveVal(NULL)

    coalesce_scalar <- function(x, fallback = 0) {
      if (is.null(x) || length(x) == 0) {
        fallback
      } else {
        val <- x[1]
        if (is.na(val)) fallback else val
      }
    }

    safe_max <- function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        0
      } else {
        max(x, na.rm = TRUE)
      }
    }

    output$notice <- shiny::renderUI({
      if (!has_ended()) {
        shiny::div(
          class = "text-muted mb-2",
          "End the simulation to unlock the replay viewer."
        )
      } else {
        shiny::span(class = "text-muted", "Use the slider to scrub through turns or press Play for an animated replay.")
      }
    })

    output$map_observed <- leaflet::renderLeaflet({
      leaflet::leaflet(
        boros,
        options = leaflet::leafletOptions(
          maxBounds = bounds_list,
          minZoom = 9
        )
      ) |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = "#cccccc",
          label = ~borough_name,
          group = "observed",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        ) |>
        leaflet::addEasyButton(
          leaflet::easyButton(
            icon = "fa-home",
            title = "Reset view",
            onClick = htmlwidgets::JS(reset_js)
          )
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    })

    output$map_truth <- leaflet::renderLeaflet({
      leaflet::leaflet(
        boros,
        options = leaflet::leafletOptions(
          maxBounds = bounds_list,
          minZoom = 9
        )
      ) |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = "#cccccc",
          label = ~borough_name,
          group = "truth",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        ) |>
        leaflet::addEasyButton(
          leaflet::easyButton(
            icon = "fa-home",
            title = "Reset view",
            onClick = htmlwidgets::JS(reset_js)
          )
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    })

    observeEvent(input$map_observed_bounds, {
      bounds <- input$map_observed_bounds
      if (is.null(bounds)) {
        return()
      }
      if (identical(sync_guard(), "observed")) {
        sync_guard(NULL)
        return()
      }
      sync_guard("truth")
      leaflet::leafletProxy(ns("map_truth")) |>
        leaflet::fitBounds(
          lng1 = bounds$west,
          lat1 = bounds$south,
          lng2 = bounds$east,
          lat2 = bounds$north
        )
    })

    observeEvent(input$map_truth_bounds, {
      bounds <- input$map_truth_bounds
      if (is.null(bounds)) {
        return()
      }
      if (identical(sync_guard(), "truth")) {
        sync_guard(NULL)
        return()
      }
      sync_guard("observed")
      leaflet::leafletProxy(ns("map_observed")) |>
        leaflet::fitBounds(
          lng1 = bounds$west,
          lat1 = bounds$south,
          lng2 = bounds$east,
          lat2 = bounds$north
        )
    })

    available_turns <- shiny::reactive({
      log_data <- logs()
      truth_log <- log_data$truth
      if (is.null(truth_log) || nrow(truth_log) == 0) {
        return(integer())
      }
      sort(unique(truth_log$turn))
    })

    shiny::observeEvent(list(available_turns(), has_ended()), {
      if (!has_ended()) {
        play_state(FALSE)
        shiny::updateActionButton(session, "play_toggle", label = "Play")
        shiny::updateSliderInput(session, "turn_slider", min = 0, max = 0, value = 0, step = 1)
        return()
      }

      turns <- available_turns()
      if (length(turns) == 0) {
        return()
      }

      current_value <- input$turn_slider
      if (is.null(current_value) || !(current_value %in% turns)) {
        current_value <- max(turns)
      }

      shiny::updateSliderInput(
        session,
        "turn_slider",
        min = min(turns),
        max = max(turns),
        value = current_value,
        step = 1
      )
    })

    shiny::observeEvent(input$play_toggle, {
      if (!has_ended()) {
        shiny::showNotification("Finish the simulation first to enable playback.", type = "message")
        return()
      }
      next_state <- !play_state()
      play_state(next_state)
      shiny::updateActionButton(session, "play_toggle", label = if (next_state) "Pause" else "Play")
    })

    shiny::observe({
      if (!has_ended() || !play_state()) {
        return()
      }

      turns <- available_turns()
      if (length(turns) <= 1) {
        play_state(FALSE)
        shiny::updateActionButton(session, "play_toggle", label = "Play")
        return()
      }

      current <- isolate(input$turn_slider)
      if (is.null(current) || !(current %in% turns)) {
        current <- min(turns)
      }
      idx <- match(current, turns)
      next_turn <- if (!is.na(idx) && idx < length(turns)) turns[idx + 1] else NA_integer_

      if (is.na(next_turn)) {
        play_state(FALSE)
        shiny::updateActionButton(session, "play_toggle", label = "Play")
        return()
      }

      delay_sec <- input$play_delay
      if (is.null(delay_sec) || !is.finite(delay_sec)) {
        delay_sec <- 2
      }
      delay_ms <- max(0.25, delay_sec) * 1000
      shiny::invalidateLater(delay_ms, session)
      shiny::updateSliderInput(session, "turn_slider", value = next_turn)
    })

    selected_turn <- shiny::reactive({
      shiny::req(has_ended())
      turns <- available_turns()
      shiny::req(length(turns) > 0)
      value <- input$turn_slider
      if (is.null(value) || !(value %in% turns)) {
        max(turns)
      } else {
        value
      }
    })

    truth_turn <- shiny::reactive({
      log_data <- logs()
      truth_log <- log_data$truth
      shiny::req(nrow(truth_log) > 0)
      truth_log |>
        dplyr::filter(turn == selected_turn()) |>
        dplyr::select(borough_index, gss_code, borough_name, incidence, imports, Rt)
    })

    observed_turn <- shiny::reactive({
      log_data <- logs()
      obs_log <- log_data$observations
      if (is.null(obs_log) || nrow(obs_log) == 0) {
        return(tibble::tibble())
      }
      obs_log |>
        dplyr::filter(turn_reported == selected_turn()) |>
        dplyr::group_by(borough_index, gss_code, borough_name) |>
        dplyr::summarise(
          tests = sum(n_returned),
          positives = sum(positives),
          positivity = ifelse(tests > 0, positives / tests, NA_real_),
          .groups = "drop"
        )
    })

    actions_turn <- shiny::reactive({
      log_data <- logs()
      action_log <- log_data$actions
      if (is.null(action_log) || nrow(action_log) == 0) {
        return(tibble::tibble())
      }
      target_turn <- selected_turn()
      action_log |>
        dplyr::filter(turn == target_turn) |>
        dplyr::group_by(borough_index, gss_code, borough_name) |>
        dplyr::summarise(
          tests_ag = sum(ifelse(test_type == "ag", n_allocated, 0L)),
          tests_pcr = sum(ifelse(test_type == "pcr", n_allocated, 0L)),
          spend = sum(spend, na.rm = TRUE),
          allocation_spend = safe_max(allocation_spend),
          intervention_cost = safe_max(intervention_cost),
          total_turn_spend = safe_max(total_turn_spend),
          cumulative_spend = safe_max(cumulative_spend),
          budget_remaining = safe_max(budget_remaining),
          interventions_active = dplyr::first(na.omit(interventions_active)),
          .groups = "drop"
        )
    })

    shiny::observe({
      shiny::req(has_ended())
      truth_df <- truth_turn()
      obs_df <- observed_turn()
      actions_df <- actions_turn()

      n_boros <- length(borough_names)
      truth_new_vals <- rep(NA_real_, n_boros)
      truth_imports <- rep(0L, n_boros)
      idx_truth <- match(truth_df$borough_name, borough_names)
      valid_truth <- which(!is.na(idx_truth))
      if (length(valid_truth) > 0) {
        pos <- idx_truth[valid_truth]
        truth_new_vals[pos] <- truth_df$incidence[valid_truth]
        truth_imports[pos] <- truth_df$imports[valid_truth]
      }

      window <- input$infectious_period
      if (is.null(window) || !is.finite(window)) {
        window <- 3
      }
      window <- max(1L, as.integer(round(window)))

      truth_log <- logs()$truth
      active_df <- compute_active_infections(truth_log, turn = selected_turn(), window = window)
      truth_active_vals <- rep(NA_real_, n_boros)
      if (nrow(active_df) > 0) {
        idx_active <- match(active_df$borough_name, borough_names)
        valid_active <- which(!is.na(idx_active))
        if (length(valid_active) > 0) {
          pos <- idx_active[valid_active]
          truth_active_vals[pos] <- active_df$active[valid_active]
        }
      }

      tests_vals <- rep(NA_integer_, n_boros)
      positivity_vals <- rep(NA_real_, n_boros)
      if (!is.null(obs_df) && nrow(obs_df) > 0) {
        idx_obs <- match(obs_df$borough_name, borough_names)
        valid_obs <- which(!is.na(idx_obs))
        if (length(valid_obs) > 0) {
          pos <- idx_obs[valid_obs]
          tests_vals[pos] <- obs_df$tests[valid_obs]
          positivity_vals[pos] <- obs_df$positivity[valid_obs]
        }
      }

      tests_ag_vals <- rep(0L, n_boros)
      tests_pcr_vals <- rep(0L, n_boros)
      if (!is.null(actions_df) && nrow(actions_df) > 0) {
        idx_actions <- match(actions_df$borough_name, borough_names)
        valid_act <- which(!is.na(idx_actions))
        if (length(valid_act) > 0) {
          pos <- idx_actions[valid_act]
          tests_ag_vals[pos] <- actions_df$tests_ag[valid_act]
          tests_pcr_vals[pos] <- actions_df$tests_pcr[valid_act]
        }
      }

      observed_palette <- leaflet::colorNumeric("YlOrRd", domain = c(0, 1), na.color = "#cccccc")
      fill_cols_obs <- observed_palette(positivity_vals)
      fill_cols_obs[is.na(positivity_vals)] <- "#cccccc"

      observed_labels <- vapply(
        seq_len(n_boros),
        function(i) {
          borough <- borough_names[i]
          tests_this <- tests_vals[i]
          obs_rate <- positivity_vals[i]
          observed_txt <- if (is.na(tests_this) || tests_this == 0) {
            "No returned results"
          } else {
            sprintf("%s tests, %.1f%% positive", tests_this, 100 * obs_rate)
          }

          actions_txt <- if (!isTRUE(input$show_actions)) {
            ""
          } else {
            ag <- tests_ag_vals[i]
            pcr <- tests_pcr_vals[i]
            if ((is.na(ag) || ag == 0) && (is.na(pcr) || pcr == 0)) {
              "Allocated tests: none"
            } else {
              sprintf(
                "Allocated tests: Ag %s, PCR %s",
                coalesce_scalar(ag, 0),
                coalesce_scalar(pcr, 0)
              )
            }
          }

          paste(
            sprintf("<strong>%s</strong>", borough),
            observed_txt,
            actions_txt,
            sep = "<br/>"
          )
        },
        character(1)
      )

      observed_proxy <- leaflet::leafletProxy(ns("map_observed"), data = boros) |>
        leaflet::clearGroup("observed") |>
        leaflet::clearGroup("action_markers") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = fill_cols_obs,
          label = lapply(observed_labels, htmltools::HTML),
          group = "observed",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )

      if (isTRUE(input$show_actions) && nrow(actions_df) > 0) {
        points_df <- actions_df |>
          dplyr::left_join(centroid_tbl, by = "borough_name") |>
          dplyr::mutate(total_tests = tests_ag + tests_pcr)

        if (nrow(points_df) > 0) {
          radius <- 4 + sqrt(pmax(points_df$total_tests, 0))
          radius[!is.finite(radius)] <- 4

          point_labels <- vapply(
            seq_len(nrow(points_df)),
            function(i) {
              sprintf(
                "%s<br/>Ag: %s, PCR: %s<br/>Spend £%s",
                points_df$borough_name[i],
                coalesce_scalar(points_df$tests_ag[i], 0),
                coalesce_scalar(points_df$tests_pcr[i], 0),
                round(coalesce_scalar(points_df$spend[i], 0), 1)
              )
            },
            character(1)
          )

          observed_proxy <- observed_proxy |>
            leaflet::addCircleMarkers(
              lng = points_df$lng,
              lat = points_df$lat,
              radius = radius,
              color = "#2b8cbe",
              fillColor = "#2b8cbe",
              fillOpacity = 0.6,
              stroke = FALSE,
              label = lapply(point_labels, htmltools::HTML),
              group = "action_markers"
            )
        }
      }

      truth_metric <- input$truth_metric
      if (is.null(truth_metric) || !truth_metric %in% c("active", "new")) {
        truth_metric <- "active"
      }

      truth_display_vals <- if (truth_metric == "active") truth_active_vals else truth_new_vals

      max_truth <- suppressWarnings(max(truth_display_vals, na.rm = TRUE))
      if (!is.finite(max_truth) || max_truth <= 0) {
        max_truth <- 1
      }
      truth_palette <- leaflet::colorNumeric("Blues", domain = c(0, max_truth), na.color = "#cccccc")
      truth_fill <- truth_palette(truth_display_vals)
      truth_fill[is.na(truth_display_vals)] <- "#cccccc"

      fmt_count <- function(x) {
        if (is.na(x)) {
          "0"
        } else {
          format(round(x), big.mark = ",")
        }
      }

      truth_labels <- vapply(
        seq_len(n_boros),
        function(i) {
          new_val <- truth_new_vals[i]
          active_val <- truth_active_vals[i]
          imports_val <- truth_imports[i]
          lines <- c(
            sprintf("<strong>%s</strong>", borough_names[i])
          )
          if (is.na(new_val) && is.na(active_val)) {
            lines <- c(lines, "No truth recorded")
          } else {
            lines <- c(
              lines,
              sprintf("New infections this turn: %s", fmt_count(new_val)),
              sprintf("Active infections (%s-turn window): %s", window, fmt_count(active_val)),
              sprintf("Imports this turn: %s", fmt_count(imports_val))
            )
          }
          htmltools::HTML(paste(lines, collapse = "<br/>"))
        },
        character(1)
      )

      leaflet::leafletProxy(ns("map_truth"), data = boros) |>
        leaflet::clearGroup("truth") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#08519c",
          fillOpacity = 0.7,
          fillColor = truth_fill,
          label = lapply(truth_labels, htmltools::HTML),
          group = "truth",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )
    })

    output$actions_table <- shiny::renderTable({
      shiny::req(has_ended())
      df <- actions_turn()
      if (is.null(df) || nrow(df) == 0) {
        return(tibble::tibble(
          borough = "No allocations recorded this turn",
          antigen = NA,
          PCR = NA,
          spend = NA
        ))
      }

      df |>
        dplyr::select(
          borough = borough_name,
          antigen = tests_ag,
          PCR = tests_pcr,
          spend
        ) |>
        dplyr::arrange(desc(spend))
    }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 1, na = "")

    output$incidence_plot <- shiny::renderPlot({
      shiny::req(has_ended())
      log_data <- logs()
      truth_ts <- log_data$truth |>
        dplyr::group_by(turn) |>
        dplyr::summarise(truth_incidence = sum(incidence), .groups = "drop")
      obs_ts_raw <- log_data$observations

      obs_ts <- if (is.null(obs_ts_raw) || nrow(obs_ts_raw) == 0) {
        tibble::tibble(turn = truth_ts$turn, observed_positives = 0)
      } else {
        obs_ts_raw |>
          dplyr::group_by(turn_reported) |>
          dplyr::summarise(observed_positives = sum(positives), .groups = "drop") |>
          dplyr::rename(turn = turn_reported)
      }

      combined <- dplyr::full_join(truth_ts, obs_ts, by = "turn")
      if (nrow(combined) == 0) {
        return(NULL)
      }
      combined$truth_incidence[is.na(combined$truth_incidence)] <- 0
      combined$observed_positives[is.na(combined$observed_positives)] <- 0

      if (nrow(combined) <= 1) {
        return(
          ggplot2::ggplot(combined, ggplot2::aes(x = turn)) +
            ggplot2::geom_point(
              ggplot2::aes(y = truth_incidence, colour = "Hidden truth"),
              size = 3
            ) +
            ggplot2::geom_point(
              ggplot2::aes(y = observed_positives, colour = "Observed positives"),
              size = 3,
              shape = 17
            ) +
            ggplot2::labs(
              x = "Turn",
              y = "Incidence / Positives",
              colour = NULL
            ) +
            ggplot2::scale_colour_manual(values = c("Hidden truth" = "#08519c", "Observed positives" = "#ef6548")) +
            ggplot2::theme_minimal()
        )
      }

      ggplot2::ggplot(combined, ggplot2::aes(x = turn)) +
        ggplot2::geom_line(
          ggplot2::aes(y = truth_incidence, colour = "Hidden truth"),
          linewidth = 1
        ) +
        ggplot2::geom_line(
          ggplot2::aes(y = observed_positives, colour = "Observed positives"),
          linewidth = 1,
          linetype = "dashed"
        ) +
        ggplot2::labs(
          x = "Turn",
          y = "Incidence / Positives",
          colour = NULL
        ) +
        ggplot2::scale_colour_manual(values = c("Hidden truth" = "#08519c", "Observed positives" = "#ef6548")) +
        ggplot2::theme_minimal()
    })

    output$testing_plot <- shiny::renderPlot({
      shiny::req(has_ended())
      action_log <- logs()$actions
      if (is.null(action_log) || nrow(action_log) == 0) {
        return(NULL)
      }

      actions_ts <- action_log |>
        dplyr::group_by(turn) |>
        dplyr::summarise(
          antigen = sum(ifelse(test_type == "ag", n_allocated, 0L)),
          PCR = sum(ifelse(test_type == "pcr", n_allocated, 0L)),
          .groups = "drop"
        )

      if (nrow(actions_ts) == 0) {
        return(NULL)
      }

      actions_long <- dplyr::bind_rows(
        actions_ts |>
          dplyr::transmute(turn, test_type = "Antigen", tests = antigen),
        actions_ts |>
          dplyr::transmute(turn, test_type = "PCR", tests = PCR)
      )

      ggplot2::ggplot(actions_long, ggplot2::aes(x = turn, y = tests, fill = test_type)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::labs(
          x = "Turn",
          y = "Tests allocated",
          fill = "Test type"
        ) +
        ggplot2::scale_fill_manual(values = c("Antigen" = "#74c476", "PCR" = "#6baed6")) +
        ggplot2::theme_minimal()
    })
  })
}
