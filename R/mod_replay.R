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
          "New infections" = "new",
          "Deaths" = "deaths"
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

mod_replay_server <- function(id, logs, boros, pop_tbl, ended, open_signal = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(open_signal)) {
      open_signal <- shiny::reactiveVal(0L)
    }

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
    maps_ready <- shiny::reactiveVal(FALSE)
    log_data <- shiny::reactive({
      logs()
    })
    truth_log <- shiny::reactive({
      replay_resolve_truth_log(log_data(), borough_names)
    })

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
      open_signal()
      replay_base_map(
        boros = boros,
        bounds_list = bounds_list,
        reset_js = reset_js,
        bbox = bbox,
        group = "observed"
      )
    })

    output$map_truth <- leaflet::renderLeaflet({
      open_signal()
      replay_base_map(
        boros = boros,
        bounds_list = bounds_list,
        reset_js = reset_js,
        bbox = bbox,
        group = "truth"
      )
    })

    observeEvent(input$map_observed_bounds, {
      bounds <- input$map_observed_bounds
      if (is.null(bounds)) {
        return()
      }
      maps_ready(TRUE)
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
      maps_ready(TRUE)
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
      replay_resolve_truth_turns(log_data())
    })

    log_signature <- shiny::reactive({
      log_snapshot <- log_data()
      turns <- replay_resolve_truth_turns(log_snapshot)
      obs_rows <- if (!is.null(log_snapshot$observations)) nrow(log_snapshot$observations) else 0
      action_rows <- if (!is.null(log_snapshot$actions)) nrow(log_snapshot$actions) else 0
      truth_rows <- if (!is.null(log_snapshot$truth)) nrow(log_snapshot$truth) else 0
      paste(length(turns), obs_rows, action_rows, truth_rows, sep = "|")
    })

    shiny::observeEvent(open_signal(), {
      maps_ready(FALSE)
      sync_guard(NULL)
      play_state(FALSE)
      shiny::updateActionButton(session, "play_toggle", label = "Play")
      shiny::isolate(animation_turn(NULL))

      session$onFlushed(function() {
        maps_ready(TRUE)
      }, once = TRUE)

    }, ignoreInit = TRUE)

    # Observe both available turns and when slider input becomes available
    shiny::observe({
      turns <- available_turns()
      ended <- has_ended()
      slider_val <- input$turn_slider
      
      # Only proceed if we have ended and have turn data
      if (!ended) {
        return()
      }
      
      if (length(turns) == 0) {
        return()
      }

      if (is.null(input$turn_slider)) {
        shiny::invalidateLater(100, session)
        return()
      }

      # Determine value to set
      if (is.null(slider_val) || !(slider_val %in% turns)) {
        target_value <- max(turns)
      } else {
        target_value <- slider_val
      }

      # Update the slider - this will work once the UI is rendered
      shiny::updateSliderInput(
        session,
        "turn_slider",
        min = min(turns),
        max = max(turns),
        value = target_value,
        step = 1
      )
    })

    shiny::observeEvent(input$play_toggle, {
      if (!has_ended()) {
        shiny::showNotification("Finish the simulation first to enable playback.", type = "message")
        return()
      }

      turns <- available_turns()
      if (length(turns) == 0) {
        shiny::showNotification("No turn data available for replay.", type = "warning")
        return()
      }

      # If starting playback, reset to first turn
      if (!play_state()) {
        current <- input$turn_slider
        # If at the last turn or invalid, reset to first turn
        if (is.null(current) || current >= max(turns)) {
          shiny::updateSliderInput(session, "turn_slider", value = min(turns))
        }
      }

      next_state <- !play_state()
      play_state(next_state)
      shiny::updateActionButton(session, "play_toggle", label = if (next_state) "Pause" else "Play")
    })

    # Track current animation turn (separate from slider to avoid race conditions)
    animation_turn <- shiny::reactiveVal(NULL)

    shiny::observeEvent(log_signature(), {
      play_state(FALSE)
      shiny::updateActionButton(session, "play_toggle", label = "Play")
      shiny::isolate(animation_turn(NULL))

      if (!has_ended()) {
        return()
      }
      turns <- available_turns()
      if (length(turns) == 0 || is.null(input$turn_slider)) {
        return()
      }
      shiny::updateSliderInput(session, "turn_slider", value = max(turns))
    }, ignoreInit = TRUE)

    shiny::observe({
      # Only react to play_state and invalidateLater, not to animation_turn changes
      if (!has_ended() || !play_state()) {
        shiny::isolate(animation_turn(NULL))
        return()
      }

      turns <- shiny::isolate(available_turns())
      if (length(turns) <= 1) {
        play_state(FALSE)
        shiny::updateActionButton(session, "play_toggle", label = "Play")
        shiny::isolate(animation_turn(NULL))
        return()
      }

      # Get current turn - use isolate to avoid triggering on animation_turn changes
      current <- shiny::isolate(animation_turn())
      if (is.null(current) || !(current %in% turns)) {
        # First iteration - start from slider value or beginning
        slider_val <- shiny::isolate(input$turn_slider)
        if (is.null(slider_val) || slider_val >= max(turns) || !(slider_val %in% turns)) {
          current <- min(turns)
        } else {
          current <- slider_val
        }
      }

      idx <- match(current, turns)
      next_turn <- if (!is.na(idx) && idx < length(turns)) turns[idx + 1] else NA_integer_

      if (is.na(next_turn)) {
        play_state(FALSE)
        shiny::updateActionButton(session, "play_toggle", label = "Play")
        shiny::isolate(animation_turn(NULL))
        return()
      }

      delay_sec <- shiny::isolate(input$play_delay)
      if (is.null(delay_sec) || !is.finite(delay_sec)) {
        delay_sec <- 2
      }
      delay_ms <- max(0.25, delay_sec) * 1000
      
      # Schedule next update BEFORE updating the turn
      shiny::invalidateLater(delay_ms, session)

      # Update both our tracked turn and the slider (use isolate to prevent re-trigger)
      shiny::isolate(animation_turn(next_turn))
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
      truth_log_data <- truth_log()
      shiny::req(nrow(truth_log_data) > 0)
      truth_log_data |>
        dplyr::filter(turn == selected_turn()) |>
        dplyr::select(borough_index, gss_code, borough_name, incidence, deaths, imports, Rt)
    })

    observed_turn <- shiny::reactive({
      replay_summarise_observed(log_data()$observations, selected_turn())
    })

    actions_turn <- shiny::reactive({
      replay_summarise_actions(log_data()$actions, selected_turn())
    })

    shiny::observe({
      shiny::req(has_ended())
      if ((is.null(input$map_observed_bounds) || is.null(input$map_truth_bounds)) && !maps_ready()) {
        shiny::invalidateLater(100, session)
        return()
      }
      if (!maps_ready()) {
        shiny::invalidateLater(100, session)
        return()
      }

      truth_df <- truth_turn()
      obs_df <- observed_turn()
      actions_df <- actions_turn()

      window <- input$infectious_period
      if (is.null(window) || !is.finite(window)) {
        window <- 3
      }

      observed_layers <- replay_build_observed_layers(
        borough_names = borough_names,
        obs_df = obs_df,
        actions_df = actions_df,
        show_actions = isTRUE(input$show_actions)
      )

      observed_proxy <- leaflet::leafletProxy(ns("map_observed"), data = boros, deferUntilFlush = FALSE) |>
        leaflet::clearGroup("observed") |>
        leaflet::clearGroup("action_markers") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = observed_layers$fill_cols,
          label = lapply(observed_layers$labels, htmltools::HTML),
          group = "observed",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )

      if (isTRUE(input$show_actions)) {
        points_df <- replay_build_action_points(actions_df, centroid_tbl)
        if (nrow(points_df) > 0) {
          observed_proxy <- observed_proxy |>
            leaflet::addCircleMarkers(
              lng = points_df$lng,
              lat = points_df$lat,
              radius = points_df$radius,
              color = "#2b8cbe",
              fillColor = "#2b8cbe",
              fillOpacity = 0.6,
              stroke = FALSE,
              label = lapply(points_df$label, htmltools::HTML),
              group = "action_markers"
            )
        }
      }

      truth_layers <- replay_build_truth_layers(
        borough_names = borough_names,
        truth_df = truth_df,
        truth_log = truth_log(),
        selected_turn = selected_turn(),
        window = window,
        truth_metric = input$truth_metric
      )

      leaflet::leafletProxy(ns("map_truth"), data = boros, deferUntilFlush = FALSE) |>
        leaflet::clearGroup("truth") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#08519c",
          fillOpacity = 0.7,
          fillColor = truth_layers$fill_cols,
          label = lapply(truth_layers$labels, htmltools::HTML),
          group = "truth",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )
    })

    output$actions_table <- shiny::renderTable({
      shiny::req(has_ended())
      replay_action_table(actions_turn())
    }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 1, na = "")

    output$incidence_plot <- shiny::renderPlot({
      shiny::req(has_ended())
      replay_plot_prevalence(
        truth_log = truth_log(),
        obs_log = log_data()$observations,
        pop_tbl = pop_tbl,
        window = input$infectious_period
      )
    })

    output$testing_plot <- shiny::renderPlot({
      shiny::req(has_ended())
      replay_plot_testing(log_data()$actions)
    })
  })
}
