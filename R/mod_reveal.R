# Replay: what you saw, and what really happened ------------------------------
#
# After the response, the whole outbreak plays back on one clock, a week at a
# time, from the day the first person was infected:
#   left  - what the response team saw: nothing at all during the unseen
#           start, then their own map and report for each week
#   right - what really happened: new infections in each area, hidden until
#           the player presses Reveal
# so students can see, for example, that the main cluster was somewhere else
# all along.

# Everything shown for one week of the replay ----------------------------------
#
# step 0 is the week of the first infection; the first report is at step
# `truth_offset`. Returns what was seen (results back that week, that week's
# hospital and death reports, the report text) and what was true.
reveal_frame <- function(state, step, config, report_lines = list()) {
  offset <- state$truth_offset %||% 0L
  turn <- step - offset

  # what was true that week: new infections per area, and totals so far
  history <- state$truth$history
  infections <- history[[min(step + 1L, length(history))]]
  so_far <- Reduce(`+`, history[seq_len(min(step + 1L, length(history)))])
  deaths <- state$truth$death_history
  deaths_so_far <- sum(unlist(deaths[seq_len(min(step + 1L, length(deaths)))]))

  # what was seen: nothing before the first report; then test results that
  # came back with that week's report, and that week's hospital reports
  seen <- turn >= 0
  results <- NULL
  reports <- NULL
  lines <- character()
  if (seen) {
    obs <- state$obs$history
    results <- obs[obs$turn_available == turn - 1L, , drop = FALSE]
    reports <- state$reports[state$reports$turn == turn, , drop = FALSE]
    if (length(report_lines) >= turn + 1L) lines <- report_lines[[turn + 1L]]
  }

  list(
    step = step,
    turn = turn,
    date = turn_date(turn, config),
    seen = seen,
    results = results,
    reports = reports,
    report_lines = lines,
    infections = infections,
    infections_this_week = sum(infections),
    infections_so_far = sum(so_far),
    deaths_so_far = deaths_so_far
  )
}

mod_reveal_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "d-flex flex-wrap align-items-end gap-3 mb-2",
      shiny::div(style = "min-width: 320px; flex: 1;", shiny::uiOutput(ns("clock_ui"))),
      shiny::div(class = "fw-semibold", shiny::textOutput(ns("clock_label"), inline = TRUE))
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::div(
          class = "d-flex align-items-center gap-3",
          shiny::tags$h5(class = "mb-1", "What you saw"),
          shiny::div(
            class = "small ms-auto map-layer-switch",
            shiny::radioButtons(
              ns("seen_layer"), label = NULL, inline = TRUE,
              choices = c("Tests positive" = "positivity", "Admissions" = "admissions", "Deaths" = "deaths"),
              selected = "admissions"
            )
          )
        ),
        leaflet::leafletOutput(ns("map_seen"), height = "340px"),
        shiny::div(class = "small mt-2", style = "min-height: 5em;", shiny::uiOutput(ns("seen_text")))
      ),
      shiny::column(
        6,
        shiny::div(
          class = "d-flex align-items-center gap-3",
          shiny::tags$h5(class = "mb-1", "What really happened"),
          shiny::div(class = "ms-auto", shiny::actionButton(ns("reveal"), "Reveal", class = "btn-warning btn-sm"))
        ),
        leaflet::leafletOutput(ns("map_true"), height = "340px"),
        shiny::div(class = "small mt-2", style = "min-height: 5em;", shiny::uiOutput(ns("true_text")))
      )
    )
  )
}

mod_reveal_server <- function(id, state, config, report_lines, boros, pop_tbl, true_start = shiny::reactive(NULL),
                              first_report = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    revealed <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$reveal, revealed(TRUE))

    # engine vectors are in the population table's order; the map has its own
    order_on_map <- match(boros$borough_name, pop_tbl$borough_name)
    to_map_index <- function(i) match(pop_tbl$borough_name[i], boros$borough_name)
    pop <- pop_tbl$pop[order_on_map]
    centroids <- sf::st_geometry(boros) |>
      sf::st_centroid(of_largest_polygon = TRUE) |>
      sf::st_coordinates()
    bbox <- sf::st_bbox(boros)

    base_map <- function() {
      leaflet::leaflet(boros, options = leaflet::leafletOptions(scrollWheelZoom = FALSE, zoomControl = FALSE)) |>
        leaflet::addPolygons(
          layerId = boros$borough_name, weight = 0.8, color = "#ffffff", opacity = 0.8,
          fillOpacity = 0.9, fillColor = "#3d4f61", label = ~borough_name, group = "areas"
        ) |>
        leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    }
    output$map_seen <- leaflet::renderLeaflet(base_map())
    output$map_true <- leaflet::renderLeaflet(base_map())

    # the clock: one step per week, from the first infection to the end
    n_steps <- shiny::reactive({
      s <- state()
      req(s)
      length(s$truth$history) - 1L
    })
    output$clock_ui <- shiny::renderUI({
      shiny::sliderInput(
        ns("step"), label = NULL, min = 0, max = n_steps(), value = 0, step = 1, ticks = FALSE,
        width = "100%",
        animate = shiny::animationOptions(interval = 900, loop = FALSE, playButton = "Play", pauseButton = "Pause")
      )
    })

    frame <- shiny::reactive({
      req(state(), !is.null(input$step))
      reveal_frame(state(), as.integer(input$step), config(), report_lines())
    })

    output$clock_label <- shiny::renderText({
      f <- frame()
      when <- if (f$turn < 0) {
        sprintf("%d week%s before the first report", -f$turn, if (f$turn == -1) "" else "s")
      } else if (f$turn == 0) {
        "first report"
      } else {
        sprintf("week %d of the response", f$turn)
      }
      paste0(format_date(f$date), " · ", when)
    })

    # left: what you saw ----------------------------------------------------------
    shiny::observe({
      f <- frame()
      layer_id <- input$seen_layer %||% "admissions"
      layers <- map_layers(f$results, f$reports, pop_tbl, boros$borough_name)
      layer <- layers[[layer_id]]
      fill <- if (f$seen) map_colours(layer$values, layer$breaks) else rep("#3d4f61", nrow(boros))

      proxy <- leaflet::leafletProxy(ns("map_seen"), data = boros) |>
        leaflet::clearGroup("areas") |>
        leaflet::addPolygons(
          layerId = boros$borough_name, weight = 0.8, color = "#ffffff", opacity = 0.8,
          fillOpacity = 0.9, fillColor = fill, label = boros$borough_name, group = "areas"
        ) |>
        leaflet::clearGroup("marks") |>
        leaflet::removeControl("seen_legend") |>
        leaflet::addLegend(
          layerId = "seen_legend", position = "bottomright", colors = c("#3d4f61", map_palette),
          labels = c(layer$none_label, layer$step_labels), title = layer$title, opacity = 0.9
        )

      first <- to_map_index(first_report())
      if (f$seen && length(first) == 1 && !is.na(first)) {
        proxy |> leaflet::addCircleMarkers(
          lng = centroids[first, 1], lat = centroids[first, 2], radius = 7, color = "#ffffff",
          weight = 2, fillColor = "#ff4d4d", fillOpacity = 1, label = "First report", group = "marks"
        )
      }
    })

    output$seen_text <- shiny::renderUI({
      f <- frame()
      if (!f$seen) {
        return(shiny::tags$em("Nothing reported yet: nobody knows this outbreak exists."))
      }
      shiny::tagList(lapply(f$report_lines, shiny::div))
    })

    # right: what really happened (hidden until Reveal) ------------------------------
    shiny::observe({
      f <- frame()
      shown <- revealed()
      infections <- f$infections[order_on_map]
      rate <- ifelse(infections > 0, 1e5 * infections / pop, NA_real_)
      breaks <- c(1, 3, 10, 30, 100, 300, 1000)
      fill <- if (shown) map_colours(rate, breaks) else rep("#2a3440", nrow(boros))

      proxy <- leaflet::leafletProxy(ns("map_true"), data = boros) |>
        leaflet::clearGroup("areas") |>
        leaflet::addPolygons(
          layerId = boros$borough_name, weight = 0.8, color = "#ffffff", opacity = if (shown) 0.8 else 0.3,
          fillOpacity = 0.9, fillColor = fill,
          label = if (shown) sprintf("%s: %s new infections", boros$borough_name, format(infections, big.mark = ",")) else NULL,
          group = "areas"
        ) |>
        leaflet::clearGroup("marks") |>
        leaflet::removeControl("true_legend")

      if (shown) {
        proxy <- proxy |> leaflet::addLegend(
          layerId = "true_legend", position = "bottomright", colors = c("#3d4f61", map_palette),
          labels = c("none", "under 1", "1 to 3", "3 to 10", "10 to 30", "30 to 100", "100 to 300", "300 to 1,000", "1,000 or more"),
          title = "New infections<br>per 100,000<br>(this week)", opacity = 0.9
        )
        start <- to_map_index(true_start())
        if (length(start) == 1 && !is.na(start)) {
          proxy |> leaflet::addCircleMarkers(
            lng = centroids[start, 1], lat = centroids[start, 2], radius = 7, color = "#ffffff",
            weight = 2, fillColor = "#000000", fillOpacity = 1, label = "Where it really began", group = "marks"
          )
        }
      }
    })

    output$true_text <- shiny::renderUI({
      f <- frame()
      if (!revealed()) {
        return(shiny::tags$em("Hidden. Play it through, then press Reveal to see what was really happening."))
      }
      shiny::tagList(
        shiny::div(sprintf("%s new infections this week", format(f$infections_this_week, big.mark = ","))),
        shiny::div(sprintf("%s infected so far, %s died so far",
                           format(f$infections_so_far, big.mark = ","), format(f$deaths_so_far, big.mark = ",")))
      )
    })
  })
}
