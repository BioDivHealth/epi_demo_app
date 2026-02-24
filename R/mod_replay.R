# Replay module -------------------------------------------------------------

mod_replay_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Turn-by-turn replay"),
    shiny::uiOutput(ns("notice")),
    shiny::uiOutput(ns("branch_notice")),
    shiny::div(
      class = "d-flex flex-wrap gap-3 align-items-center mb-3",
      shiny::radioButtons(
        inputId = ns("replay_branch"),
        label = "Replay branch",
        choices = c(
          "Player branch" = "player",
          "Baseline branch" = "baseline"
        ),
        selected = "player",
        inline = TRUE,
        width = "320px"
      ),
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
      shiny::tags$strong("Selected turn branch comparison"),
      shiny::tableOutput(ns("turn_comparison_table"))
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
    branch_choices <- shiny::reactive({
      replay_branch_names(log_data())
    })
    selected_branch <- shiny::reactive({
      choices <- branch_choices()
      shiny::req(length(choices) > 0)
      selected <- input$replay_branch
      if (is.null(selected) || !selected %in% choices) {
        choices[[1]]
      } else {
        selected
      }
    })
    branch_logs <- shiny::reactive({
      replay_get_branch_logs(log_data(), branch = selected_branch())
    })
    player_logs <- shiny::reactive({
      replay_get_branch_logs(log_data(), branch = "player")
    })
    truth_log <- shiny::reactive({
      replay_resolve_truth_log(log_data(), borough_names, branch = selected_branch())
    })
    player_truth_log <- shiny::reactive({
      if ("player" %in% branch_choices()) {
        replay_resolve_truth_log(log_data(), borough_names, branch = "player")
      } else {
        replay_resolve_truth_log(log_data(), borough_names, branch = selected_branch())
      }
    })
    baseline_truth_log <- shiny::reactive({
      if (!("baseline" %in% branch_choices())) {
        return(tibble::tibble())
      }
      replay_resolve_truth_log(log_data(), borough_names, branch = "baseline")
    })

    output$notice <- shiny::renderUI({
      if (!has_ended()) {
        shiny::div(
          class = "text-muted mb-2",
          "End the simulation to unlock the replay viewer."
        )
      } else {
        turns <- available_turns()
        if (length(turns) == 0) {
          shiny::span(class = "text-muted", "No turn data available for replay yet.")
        } else if (length(turns) == 1) {
          shiny::span(class = "text-muted", "Only one turn recorded. Advance a few turns before ending to see the replay.")
        } else {
          shiny::span(class = "text-muted", "Use the slider to scrub through turns or press Play for an animated replay.")
        }
      }
    })

    output$branch_notice <- shiny::renderUI({
      if (!has_ended()) {
        return(NULL)
      }
      turns <- available_turns()
      if (length(turns) == 0) {
        return(NULL)
      }

      target_turn <- input$turn_slider
      if (is.null(target_turn) || !target_turn %in% turns) {
        target_turn <- max(turns)
      }

      obs_log <- branch_logs()$observations
      obs_turn <- replay_summarise_observed(
        obs_log = obs_log,
        target_turn = target_turn,
        branch = selected_branch()
      )
      total_tests <- if (nrow(obs_turn) == 0) 0 else sum(obs_turn$tests, na.rm = TRUE)
      if (total_tests > 0) {
        return(NULL)
      }

      label <- if (identical(selected_branch(), "baseline")) {
        "Baseline branch"
      } else {
        "Player branch"
      }

      shiny::div(
        class = "alert alert-light py-2 mb-2 small",
        sprintf(
          "%s has no returned test results for this turn. Interpret observed metrics cautiously and compare trend context across turns.",
          label
        )
      )
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
      turns <- replay_resolve_truth_turns(log_data(), branch = selected_branch())
      turns <- turns[is.finite(turns)]
      as.integer(turns)
    })

    log_signature <- shiny::reactive({
      current_branch <- selected_branch()
      logs_for_branch <- replay_get_branch_logs(log_data(), branch = current_branch)
      turns <- replay_resolve_truth_turns(log_data(), branch = current_branch)
      obs_rows <- nrow(replay_filter_branch(logs_for_branch$observations, current_branch))
      action_rows <- nrow(replay_filter_branch(logs_for_branch$actions, current_branch))
      truth_rows <- nrow(replay_filter_branch(logs_for_branch$truth, current_branch))
      paste(current_branch, length(turns), obs_rows, action_rows, truth_rows, sep = "|")
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

    shiny::observe({
      choices <- branch_choices()
      if (length(choices) == 0) {
        choices <- c("player")
      }
      labels <- stats::setNames(
        choices,
        vapply(
          choices,
          function(x) {
            if (identical(x, "player")) {
              "Player branch"
            } else if (identical(x, "baseline")) {
              "Baseline branch"
            } else {
              tools::toTitleCase(gsub("_", " ", x))
            }
          },
          character(1)
        )
      )

      current <- shiny::isolate(input$replay_branch)
      target <- if (!is.null(current) && current %in% choices) {
        current
      } else if ("player" %in% choices) {
        "player"
      } else {
        choices[[1]]
      }

      shiny::updateRadioButtons(
        session = session,
        inputId = "replay_branch",
        choices = labels,
        selected = target
      )
    })

    apply_turns_to_slider <- function(turns, value_override = NULL) {
      if (length(turns) == 0) {
        return()
      }
      turns <- sort(unique(as.integer(turns)))
      turns <- turns[is.finite(turns)]
      if (length(turns) == 0) {
        return()
      }

      update_fn <- function() {
        current <- shiny::isolate(input$turn_slider)
        if (!is.null(value_override) && value_override %in% turns) {
          target_value <- value_override
        } else if (!is.null(current) && current %in% turns) {
          target_value <- current
        } else {
          target_value <- max(turns)
        }
        shiny::updateSliderInput(
          session,
          "turn_slider",
          min = min(turns),
          max = max(turns),
          value = target_value,
          step = 1
        )
      }

      if (is.null(input$turn_slider)) {
        session$onFlushed(update_fn, once = TRUE)
      } else {
        update_fn()
      }
    }

    shiny::observeEvent(open_signal(), {
      if (!has_ended()) {
        return()
      }
      turns <- available_turns()
      if (length(turns) == 0) {
        return()
      }
      apply_turns_to_slider(turns, value_override = max(turns))
    }, ignoreInit = TRUE)

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
    }, ignoreInit = TRUE)

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
      if (length(turns) == 0) {
        return()
      }
      apply_turns_to_slider(turns, value_override = max(turns))
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
      replay_summarise_observed(
        obs_log = branch_logs()$observations,
        target_turn = selected_turn(),
        branch = selected_branch()
      )
    })

    actions_turn <- shiny::reactive({
      replay_summarise_actions(
        action_log = branch_logs()$actions,
        target_turn = selected_turn(),
        branch = selected_branch()
      )
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

    output$turn_comparison_table <- shiny::renderTable({
      shiny::req(has_ended())
      comparison_df <- log_data()$turn_comparison
      if (is.null(comparison_df) || !is.data.frame(comparison_df) || nrow(comparison_df) == 0) {
        return(tibble::tibble(note = "Turn-level branch comparison is unavailable for this replay payload."))
      }
      comparison_df <- tibble::as_tibble(comparison_df)
      if (!"turn" %in% names(comparison_df)) {
        return(tibble::tibble(note = "Turn-level branch comparison is unavailable for this replay payload."))
      }

      selected <- comparison_df |>
        dplyr::filter(turn == selected_turn()) |>
        dplyr::select(
          dplyr::any_of(c(
            "turn",
            "player_truth_total",
            "baseline_truth_total",
            "player_deaths_total",
            "baseline_deaths_total",
            "player_observed_positives",
            "baseline_observed_positives",
            "player_total_turn_spend",
            "baseline_total_turn_spend"
          ))
        )

      if (nrow(selected) == 0) {
        return(tibble::tibble(note = "No branch comparison row is available for the selected turn."))
      }
      selected
    }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 1, na = "")

    output$incidence_plot <- shiny::renderPlot({
      shiny::req(has_ended())
      replay_plot_prevalence(
        truth_log = player_truth_log(),
        obs_log = player_logs()$observations,
        pop_tbl = pop_tbl,
        window = input$infectious_period,
        baseline_truth_log = baseline_truth_log()
      )
    })

    output$testing_plot <- shiny::renderPlot({
      shiny::req(has_ended())
      replay_plot_testing(branch_logs()$actions)
    })
  })
}
