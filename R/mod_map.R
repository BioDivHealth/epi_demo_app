# Map module -----------------------------------------------------------------
#
# Two pieces of UI share one server:
#   mod_map_ui()      - the situation map (results, chosen and restricted areas)
#   mod_response_ui() - the response: who/where/which test/how much, control
#                       measures, and what clicking the map does
# The plan carries over from turn to turn until changed.

mod_map_ui <- function(id, intervention_defs = intervention_catalog()) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Situation map"),
    leaflet::leafletOutput(ns("map"), height = "400px"),
    shiny::div(class = "small text-muted mt-1", shiny::textOutput(ns("turn_positivity"), inline = TRUE))
  )
}

mod_response_ui <- function(id, intervention_defs = intervention_catalog()) {
  ns <- shiny::NS(id)
  tp <- teaching_params()
  bslib::card(
    bslib::card_header("Response"),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = c(12, 12, 12, 12), lg = c(3, 3, 4, 2)),
      fill = FALSE,

      # testing: who and where
      shiny::div(
        class = "small",
        shiny::radioButtons(
          ns("who"),
          label = "Test who",
          choices = c("People with symptoms" = "symptomatic", "Random sample of residents" = "random"),
          selected = "symptomatic"
        ),
        shiny::radioButtons(
          ns("where"),
          label = "Where",
          choices = c(
            "Everywhere (by population)" = "everywhere",
            "Areas with positive results so far" = "positives",
            "Areas I choose on the map" = "chosen"
          ),
          selected = "everywhere"
        )
      ),

      # testing: which test and how much
      shiny::div(
        class = "small",
        shiny::radioButtons(
          ns("test_type"),
          label = "Test",
          choices = c("Antigen" = "ag", "PCR" = "pcr"),
          selected = "pcr"
        ),
        shiny::sliderInput(
          ns("spend"),
          label = paste0("Spend on tests per report (", tp$currency, ")"),
          min = 0,
          max = tp$tests$budget,
          value = 0,
          step = 5000,
          ticks = FALSE,
          sep = ","
        ),
        shiny::div(class = "text-muted", shiny::textOutput(ns("allocation_summary"), inline = TRUE))
      ),

      # control measures and what they will do next
      shiny::div(
        class = "small",
        shiny::tags$label(class = "form-label", "Control measures"),
        shiny::checkboxGroupInput(
          ns("interventions"),
          label = NULL,
          choices = stats::setNames(intervention_defs$id, intervention_defs$label),
          selected = character()
        ),
        shiny::checkboxInput(ns("tracing"), "Contact tracing", value = FALSE),
        shiny::checkboxInput(ns("build_testing"), "Build better testing", value = FALSE),
        shiny::div(class = "text-muted", shiny::htmlOutput(ns("measure_status")))
      ),

      # what clicking the map does
      shiny::div(
        class = "small",
        shiny::radioButtons(
          ns("click_mode"),
          label = "Clicking the map will",
          choices = c("Choose areas to test" = "choose", "Restrict an area" = "restrict"),
          selected = "choose"
        )
      )
    )
  )
}

mod_map_server <- function(
    id,
    boros,
    allocations_init,
    budget_info,
    observed_this_turn,
    observed_history = shiny::reactive(NULL),
    pop_tbl,
    app_ready = shiny::reactive(TRUE),
    intervention_defs = intervention_catalog(),
    first_report = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ready <- shiny::reactive({
      isTRUE(app_ready())
    })
    map_ready <- shiny::reactiveVal(FALSE)

    observeEvent(input$map_bounds, {
      if (!is.null(input$map_bounds)) {
        map_ready(TRUE)
      }
    })

    chosen <- shiny::reactiveVal(integer())
    restricted <- shiny::reactiveVal(integer())
    selected_interventions <- shiny::reactive({
      sel <- input$interventions
      if (is.null(sel)) character() else sel
    })

    # everything planned this turn apart from tests: interventions, local
    # restrictions, tracing and building testing
    other_cost <- function(restricted_now = restricted()) {
      budget <- budget_info()
      interventions <- compute_intervention_effects(selected_interventions(), intervention_defs)$cost
      local <- sum(budget$local_costs[restricted_now])
      tracing <- if (isTRUE(input$tracing)) budget$tracing_cost else 0
      build <- if (isTRUE(input$build_testing) && isTRUE(budget$testing$can_build)) budget$testing$build_cost else 0
      interventions + local + tracing + build
    }

    # boroughs that have had at least one positive result returned so far
    positive_boroughs <- shiny::reactive({
      hist <- observed_history()
      if (is.null(hist) || nrow(hist) == 0) {
        return(integer())
      }
      sort(unique(hist$borough_index[hist$positives > 0]))
    })

    # turn the testing plan into tests per borough ---------------------------
    plan <- shiny::reactive({
      budget <- budget_info()

      # test type (PCR if antigen does not exist yet)
      test <- input$test_type %||% "pcr"
      if (test == "ag" && !isTRUE(budget$testing$ag_available)) {
        test <- "pcr"
      }
      cost <- budget$costs[[test]]

      # which boroughs
      where <- input$where %||% "everywhere"
      targets <- switch(
        where,
        everywhere = seq_len(nrow(pop_tbl)),
        positives = positive_boroughs(),
        chosen = chosen()
      )

      # money: what the student asked for, cut down to what is left this turn
      asked <- input$spend %||% 0
      money <- max(0, min(asked, budget$budget - other_cost()))
      n_tests <- if (length(targets) == 0 || cost <= 0) 0 else floor(money / cost)

      # spread the tests over the chosen boroughs by population
      per_borough <- integer(nrow(pop_tbl))
      if (n_tests > 0) {
        per_borough[targets] <- spread_tests(n_tests, pop_tbl$pop[targets])
      }

      allocs <- allocations_init
      if (test == "ag") {
        allocs$n_ag <- as.integer(per_borough)
      } else {
        allocs$n_pcr <- as.integer(per_borough)
      }

      list(
        allocations = allocs,
        test = test,
        where = where,
        targets = targets,
        asked = asked,
        money = money,
        cut_down = money < asked - 1e-6
      )
    })

    # Compute borough centroids for allocation markers
    centroids <- sf::st_geometry(boros) |>
      sf::st_centroid(of_largest_polygon = TRUE) |>
      sf::st_coordinates()
    centroid_tbl <- tibble::tibble(
      borough_name = boros$borough_name,
      lng = centroids[, 1],
      lat = centroids[, 2]
    )

    bbox <- sf::st_bbox(boros)
    # a margin of 10% of the place's size (London or a whole country)
    pad_x <- 0.1 * (bbox[["xmax"]] - bbox[["xmin"]])
    pad_y <- 0.1 * (bbox[["ymax"]] - bbox[["ymin"]])
    bounds_list <- list(
      c(bbox[["ymin"]] - pad_y, bbox[["xmin"]] - pad_x),
      c(bbox[["ymax"]] + pad_y, bbox[["xmax"]] + pad_x)
    )
    reset_js <- sprintf(
      "function(btn, map){ map.fitBounds([[%.6f, %.6f],[%.6f, %.6f]]); }",
      bounds_list[[1]][1], bounds_list[[1]][2],
      bounds_list[[2]][1], bounds_list[[2]][2]
    )

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(
        boros,
        options = leaflet::leafletOptions(
          # keep the view on the place, at whatever zoom fits it
          maxBounds = bounds_list,
          # the mouse wheel scrolls the page; zoom with the +/- buttons
          scrollWheelZoom = FALSE
        )
      ) |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 0.8,
          color = "#ffffff",
          opacity = 0.8,
          fillOpacity = 0.9,
          fillColor = "#3d4f61",
          label = ~borough_name,
          group = "boroughs",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        ) |>
        leaflet::addEasyButton(
          leaflet::easyButton(
            icon = "fa-home",
            title = "Reset view",
            onClick = htmlwidgets::JS(reset_js)
          )
        ) |>
        # colour = share of tests positive among results back this turn
        # (the positivity legend is added with each update, so its scale can
        # follow this turn's results)
        leaflet::addControl(
          html = paste0(
            "<div style='font-size:11px;line-height:1.4'>",
            "<span style='display:inline-block;width:10px;height:10px;background:#3d4f61'></span> no results this report<br>",
            "<span style='display:inline-block;width:10px;height:10px;border:3px solid #ff4d4d'></span> restricted<br>",
            "<span style='display:inline-block;width:10px;height:10px;border:3px solid #4fc3f7'></span> chosen for testing<br>",
            "<span style='display:inline-block;width:10px;height:10px;background:#b56cf0;border:2px solid #e0b3ff'></span> first patient's area (lit until tested)",
            "<br><span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#ff4d4d;border:2px solid #fff'></span> first patient reported",
            "</div>"
          ),
          position = "bottomleft"
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    })

    # Map clicks: choose boroughs to test, or toggle a local restriction
    observeEvent(input$map_shape_click, {
      req(ready())
      click <- input$map_shape_click
      req(click$id)
      idx <- match(click$id, pop_tbl$borough_name)
      req(!is.na(idx))

      if (identical(input$click_mode, "restrict")) {
        current <- restricted()
        if (idx %in% current) {
          restricted(setdiff(current, idx))
          return()
        }
        trial <- c(current, idx)
        if (budget_info()$budget - other_cost(trial) < -1e-6) {
          shiny::showNotification("Not enough money left this turn to restrict another borough.", type = "error")
          return()
        }
        restricted(trial)
        return()
      }

      # choose mode: toggle the borough, and test only chosen boroughs
      current <- chosen()
      chosen(if (idx %in% current) setdiff(current, idx) else sort(c(current, idx)))
      if (!identical(input$where, "chosen")) {
        shiny::updateRadioButtons(session, "where", selected = "chosen")
      }
    })

    # measure labels show this place's prices (they scale with population)
    observe({
      req(ready())
      budget <- budget_info()
      defs <- budget$interventions
      shiny::updateCheckboxGroupInput(
        session, "interventions",
        choiceNames = sprintf("%s (%s/turn)", defs$label, format_money(defs$cost)),
        choiceValues = defs$id,
        selected = shiny::isolate(input$interventions)
      )
      shiny::updateCheckboxInput(session, "tracing",
                                 label = sprintf("Contact tracing (%s/turn)", format_money(budget$tracing_cost)))
      shiny::updateCheckboxInput(
        session, "build_testing",
        label = sprintf("Build better testing (%s once, %d turns, half capacity meanwhile)",
                        format_money(budget$testing$build_cost), budget$testing$build_turns)
      )
    })

    # what each control measure will do next turn (lag, fading, capacity)
    output$measure_status <- shiny::renderUI({
      req(ready())
      budget <- budget_info()
      lines <- character()

      for (id in selected_interventions()) {
        def <- intervention_defs[intervention_defs$id == id, ]
        next_streak <- get_streak(budget$streaks, id) + 1L
        strength <- intervention_strength(next_streak, def$lag, def$fatigue)
        lines <- c(lines, if (strength == 0) {
          sprintf("%s: no effect next turn (takes a turn to start).", def$label)
        } else {
          sprintf("%s: %d%% strength next turn.", def$label, round(100 * strength))
        })
      }

      if (length(restricted()) > 0) {
        lr <- teaching_params()$local_restrictions
        next_streaks <- budget$local_streaks[restricted()] + 1L
        strengths <- vapply(next_streaks, intervention_strength, numeric(1), lag = lr$lag, fatigue = lr$fatigue)
        lines <- c(lines, sprintf(
          "Local restrictions on %d borough(s): %s.",
          length(restricted()),
          if (all(strengths == 0)) "no effect next turn (takes a turn to start)" else
            paste0("up to ", round(100 * max(strengths)), "% strength next turn")
        ))
      }

      if (isTRUE(input$tracing)) {
        lines <- c(lines, sprintf("Contact tracing: follows up to %d positives a turn.", teaching_params()$tracing$capacity))
      }
      if (isTRUE(budget$testing$building)) {
        lines <- c(lines, sprintf("Building better testing: %d turn(s) left, capacity halved meanwhile.", budget$testing$build_left))
      }

      if (length(lines) == 0) {
        return(shiny::HTML("No control measures on."))
      }
      shiny::HTML(paste(lines, collapse = "<br>"))
    })

    # test choices show today's price and wait (antigen only once it exists)
    observe({
      req(ready())
      budget <- budget_info()
      testing <- budget$testing
      pcr_label <- sprintf("PCR %s, %d-turn wait", format_money(budget$costs[["pcr"]]), testing$delays$pcr)
      if (isTRUE(testing$ag_available)) {
        choices <- stats::setNames(c("ag", "pcr"), c(sprintf("Antigen %s, instant", format_money(budget$costs[["ag"]])), pcr_label))
        selected <- shiny::isolate(input$test_type) %||% "pcr"
      } else {
        choices <- stats::setNames("pcr", pcr_label)
        selected <- "pcr"
      }
      shiny::updateRadioButtons(session, "test_type", choices = choices, selected = selected)
    })

    # one line saying what this plan will do
    output$allocation_summary <- shiny::renderText({
      req(ready())
      p <- plan()
      budget <- budget_info()
      n_tests <- sum(p$allocations$n_ag + p$allocations$n_pcr)
      test_name <- if (p$test == "ag") "antigen" else "PCR"
      who_name <- if (identical(input$who, "random")) "a random sample of residents" else "people with symptoms"

      if (p$asked <= 0) {
        return("No tests planned yet: move the spend slider to start testing.")
      }

      if (length(p$targets) == 0) {
        where_txt <- if (p$where == "positives") "no borough has had a positive result yet" else "no boroughs chosen yet (click the map)"
        return(paste0("No tests this turn: ", where_txt, "."))
      }

      where_txt <- if (p$where == "everywhere") paste("all", nrow(pop_tbl), "areas") else paste(length(p$targets), "area(s)")
      txt <- paste0(
        format(n_tests, big.mark = ",", scientific = FALSE), " ", test_name, " tests of ", who_name,
        " in ", where_txt, " (", format_money(p$money), ")."
      )

      # symptomatic testing can only reach the people who come forward
      if (identical(input$who, "symptomatic") && !is.null(budget$pool_size)) {
        reachable <- sum(pmin(p$allocations$n_ag + p$allocations$n_pcr, budget$pool_size))
        if (reachable < n_tests) {
          txt <- paste0(
            txt, " Only about ", format(reachable, big.mark = ",", scientific = FALSE),
            " people come forward there, so only that many will be tested (and paid for)."
          )
        }
      }
      if (p$cut_down) {
        txt <- paste0(txt, " Cut down to fit the money left this turn.")
      }
      txt
    })

    observe({
      if (!ready()) {
        return()
      }
      latest <- observed_this_turn()
      total_tests <- sum(latest$n, na.rm = TRUE)
      positives <- sum(latest$positives, na.rm = TRUE)
      output$turn_positivity <- shiny::renderText({
        if (total_tests == 0) {
          ""
        } else {
          paste0("Last turn: ", round(100 * positives / total_tests, 1), "% positive")
        }
      })
    })

    # Update map with results, planned tests, chosen and restricted boroughs
    observe({
      if (!ready()) {
        shiny::invalidateLater(100, session)
        return()
      }
      if (!map_ready()) {
        shiny::invalidateLater(100, session)
        return()
      }
      res <- observed_this_turn()
      allocs <- plan()$allocations

      # Aggregate observed results
      agg <- res |>
        dplyr::group_by(borough_name) |>
        dplyr::summarise(
          positives = sum(positives),
          tests = sum(n),
          .groups = "drop"
        )

      values <- rep(NA_real_, nrow(boros))
      idx <- match(agg$borough_name, boros$borough_name)
      values[idx] <- agg$positives / agg$tests

      # Color palette: grey for no results, green to red for positivity
      # colour scale runs up to the highest positivity this turn (at least
      # 1%), so differences between areas show even when positivity is low
      top <- if (all(is.na(values))) 0.01 else max(0.01, max(values, na.rm = TRUE))
      # cool light blue (low) to deep red (high): reads well on the dark map
      # and stays distinct from the slate used for "no results"
      palette <- leaflet::colorNumeric(
        palette = c("#a9c5d6", "#f6d55c", "#f28e2b", "#e15759", "#9c1f2e"),
        domain = c(0, top),
        na.color = "#3d4f61"
      )

      # areas with no results this report stay dark slate
      fill_cols <- ifelse(is.na(values), "#3d4f61", palette(values))

      # Build labels including planned tests
      labels <- lapply(seq_len(nrow(boros)), function(i) {
        name <- boros$borough_name[i]
        val <- values[i]

        result_txt <- if (is.na(val)) {
          "<em>No results yet</em>"
        } else if (val == 0) {
          "<span style='color:green'>0% positive</span>"
        } else {
          sprintf("<span style='color:#d95f02'>%.1f%% positive</span>", 100 * val)
        }

        planned <- allocs$n_ag[i] + allocs$n_pcr[i]
        alloc_txt <- if (planned > 0) sprintf("<br><small>Planned: %d tests</small>", planned) else ""
        restrict_txt <- if (i %in% restricted()) "<br><small><b>Restricted</b></small>" else ""

        htmltools::HTML(paste0("<strong>", name, "</strong><br>", result_txt, alloc_txt, restrict_txt))
      })

      # outlines: red = restricted, blue = chosen for testing
      is_restricted <- seq_len(nrow(boros)) %in% restricted()
      is_chosen <- seq_len(nrow(boros)) %in% chosen() & identical(input$where, "chosen")
      outline_col <- ifelse(is_restricted, "#ff4d4d", ifelse(is_chosen, "#4fc3f7", "#ffffff"))
      outline_weight <- ifelse(is_restricted | is_chosen, 3.5, 0.8)

      # the first patient's area is lit: violet (outside the positivity
      # scale) until results come back from there, and a violet outline all
      # game unless one of the player's own outlines takes priority
      first <- first_report()
      if (!is.null(first) && !is.na(first$index)) {
        f <- first$index
        if (is.na(values[f])) {
          fill_cols[f] <- "#b56cf0"
        }
        if (!is_restricted[f] && !is_chosen[f]) {
          outline_col[f] <- "#e0b3ff"
          outline_weight[f] <- 3
        }
      }

      proxy <- leaflet::leafletProxy(ns("map"), data = boros) |>
        leaflet::removeControl("positivity_legend") |>
        leaflet::addLegend(
          layerId = "positivity_legend",
          position = "bottomright",
          pal = palette,
          values = c(0, top),
          title = "Tests positive<br>(results back<br>this report)",
          labFormat = leaflet::labelFormat(suffix = "%", digits = 1, transform = function(x) 100 * x),
          opacity = 0.8
        ) |>
        leaflet::clearGroup("boroughs") |>
        leaflet::clearGroup("alloc_markers") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = outline_weight,
          color = outline_col,
          opacity = 0.9,
          fillOpacity = 0.9,
          fillColor = fill_cols,
          label = labels,
          group = "boroughs",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )

      # (no circles for planned tests: they covered the map; chosen areas
      # have a blue outline instead)

      # where the first patient was reported (public, stays all game)
      proxy <- proxy |> leaflet::clearGroup("first_report")
      if (!is.null(first) && !is.na(first$index)) {
        spot <- centroid_tbl[first$index, ]
        proxy |>
          leaflet::addCircleMarkers(
            lng = spot$lng,
            lat = spot$lat,
            radius = 8,
            color = "#ffffff",
            weight = 2,
            fillColor = "#ff4d4d",
            fillOpacity = 1,
            label = first$label,
            group = "first_report"
          )
      }
    })

    list(
      allocations = shiny::reactive(plan()$allocations),
      who = shiny::reactive(input$who %||% "symptomatic"),
      # after each turn the plan carries over; building testing is a one-off
      # so untick it. Restrictions, interventions and tracing stay on.
      reset = function() {
        shiny::updateCheckboxInput(session, "build_testing", value = FALSE)
      },
      # new game: everything starts off and at zero; the player decides
      clear_interventions = function(fair_share = 0, max_spend = NULL) {
        shiny::updateCheckboxGroupInput(session, "interventions", selected = character())
        shiny::updateCheckboxInput(session, "tracing", value = FALSE)
        shiny::updateCheckboxInput(session, "build_testing", value = FALSE)
        shiny::updateRadioButtons(session, "who", selected = "symptomatic")
        shiny::updateRadioButtons(session, "where", selected = "everywhere")
        # the slider's range follows this place's per-turn cap (London: 200,000)
        if (!is.null(max_spend)) {
          step <- 10^floor(log10(max_spend / 40))
          shiny::updateSliderInput(
            session, "spend",
            max = floor(max_spend / step) * step, step = step,
            value = 0
          )
        } else {
          shiny::updateSliderInput(session, "spend", value = 0)
        }
        restricted(integer())
        chosen(integer())
      },
      interventions = selected_interventions,
      restricted = restricted,
      tracing = shiny::reactive(isTRUE(input$tracing)),
      build_testing = shiny::reactive(isTRUE(input$build_testing)),
      # everything planned this turn (tests and measures), for the top bar
      planned_total = shiny::reactive(other_cost() + plan()$money)
    )
  })
}
