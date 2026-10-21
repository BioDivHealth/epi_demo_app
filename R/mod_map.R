# Map module -----------------------------------------------------------------
#
# Two pieces of UI share one server:
#   mod_map_ui()      - the situation map (results, chosen and restricted areas)
#   mod_response_ui() - the response: who/where/which test/how much, control
#                       measures, and what clicking the map does
# The plan carries over from turn to turn until changed.

# Map layers -------------------------------------------------------------------
#
# Three public views of the latest report, one value per map area (in the
# map's order). Colours come in fixed steps that never rescale, so the map
# gets deeper and redder only as things really get worse.

map_palette <- c("#a9c5d6", "#d9e6a1", "#f6d55c", "#f5a742", "#f28e2b", "#e15759", "#b8333f", "#7a1426")

map_layers <- function(results, reports, pop_tbl, area_names) {
  n <- length(area_names)
  pop <- pop_tbl$pop[match(area_names, pop_tbl$borough_name)]

  # tests positive among results back this report
  tests <- numeric(n)
  positives <- numeric(n)
  if (!is.null(results) && nrow(results) > 0) {
    idx <- match(results$borough_name, area_names)
    tests <- as.numeric(tabulate_sum(idx, results$n, n))
    positives <- as.numeric(tabulate_sum(idx, results$positives, n))
  }
  positivity <- ifelse(tests > 0, positives / tests, NA_real_)

  # hospital admissions and deaths in the latest report, per 100,000 people
  admissions <- numeric(n)
  deaths <- numeric(n)
  if (!is.null(reports) && nrow(reports) > 0) {
    latest <- reports[reports$turn == max(reports$turn), , drop = FALSE]
    idx <- match(pop_tbl$borough_name[latest$borough_index], area_names)
    admissions <- as.numeric(tabulate_sum(idx, latest$admissions, n))
    deaths <- as.numeric(tabulate_sum(idx, latest$deaths, n))
  }
  per_100k <- function(x) ifelse(x > 0, 1e5 * x / pop, NA_real_)

  rate_breaks <- c(0.1, 0.3, 1, 3, 10, 30, 100)
  rate_labels <- c("under 0.1", "0.1 to 0.3", "0.3 to 1", "1 to 3", "3 to 10", "10 to 30", "30 to 100", "100 or more")
  list(
    positivity = list(
      values = positivity, tests = tests,
      breaks = c(0.001, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2),
      step_labels = c("under 0.1%", "0.1 to 0.5%", "0.5 to 1%", "1 to 2%", "2 to 5%", "5 to 10%", "10 to 20%", "20% or more"),
      none_label = "no results",
      title = "Tests positive<br>(results back<br>this report)"
    ),
    admissions = list(
      values = per_100k(admissions), counts = admissions,
      breaks = rate_breaks, step_labels = rate_labels, none_label = "none reported",
      title = "Hospital admissions<br>per 100,000<br>(this report)"
    ),
    deaths = list(
      values = per_100k(deaths), counts = deaths,
      breaks = rate_breaks, step_labels = rate_labels, none_label = "none reported",
      title = "Deaths<br>per 100,000<br>(this report)"
    )
  )
}

# sum `x` into `n` bins by index (NA indices dropped)
tabulate_sum <- function(idx, x, n) {
  out <- numeric(n)
  keep <- !is.na(idx)
  if (any(keep)) {
    sums <- rowsum(x[keep], idx[keep])
    out[as.integer(rownames(sums))] <- sums[, 1]
  }
  out
}

# colour for each value: its step on the fixed scale; NA = dark slate
map_colours <- function(values, breaks) {
  step <- findInterval(values, breaks) + 1L
  ifelse(is.na(values), "#3d4f61", map_palette[pmin(step, length(map_palette))])
}

mod_map_ui <- function(id, intervention_defs = intervention_catalog()) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex align-items-center gap-3",
      "Situation map",
      shiny::div(
        class = "small ms-auto map-layer-switch",
        shiny::radioButtons(
          ns("map_layer"), label = NULL, inline = TRUE,
          choices = c("Tests positive" = "positivity", "Hospital admissions" = "admissions", "Deaths" = "deaths"),
          selected = "positivity"
        )
      )
    ),
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
        # every real case a test finds is told to isolate
        shiny::radioButtons(
          ns("who"),
          label = "Test and isolate",
          choiceNames = list(
            shiny::HTML("People with symptoms <span class='text-muted'>(those who come forward ill; most have something else)</span>"),
            shiny::HTML("A random sample <span class='text-muted'>(anyone, ill or not)</span>")
          ),
          choiceValues = list("symptomatic", "random"),
          selected = "symptomatic"
        ),
        shiny::radioButtons(
          ns("where"),
          label = "Where",
          choices = c(
            "Everywhere" = "everywhere",
            "Where positives were found" = "positives",
            "Areas I pick on the map" = "chosen"
          ),
          selected = "everywhere"
        )
      ),

      # testing: which test and how much
      shiny::div(
        class = "small",
        # built by the server each report (prices, waits, antigen greyed
        # out until it exists)
        shiny::uiOutput(ns("test_type_ui")),
        shiny::sliderInput(
          ns("spend"),
          label = paste0("Spend on tests (", tp$currency, " per report)"),
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
        shiny::tags$label(class = "form-label", paste0("Measures (", tp$currency, " per report)")),
        # built by the server each report (prices; lockdown greyed out until
        # people would accept it)
        shiny::uiOutput(ns("interventions_ui")),
        shiny::checkboxInput(ns("tracing"), "Contact tracing", value = FALSE),
        shiny::checkboxInput(ns("build_testing"), "Improve PCR", value = FALSE),
        shiny::checkboxInput(ns("develop_rapid"), "Develop rapid tests", value = FALSE),
        shiny::div(class = "text-muted", shiny::htmlOutput(ns("measure_status")))
      ),

      # what clicking the map does
      shiny::div(
        class = "small",
        shiny::radioButtons(
          ns("click_mode"),
          label = "Map click",
          choices = c("Pick test areas" = "choose", "Restrict an area" = "restrict"),
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
    first_report = shiny::reactive(NULL),
    reports = shiny::reactive(NULL)) {
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
      # rapid tests: paid every report while being developed
      developing <- isTRUE(budget$testing$developing_rapid) ||
        (isTRUE(input$develop_rapid) && isTRUE(budget$testing$can_develop_rapid))
      build <- build + if (developing) budget$testing$rapid_cost else 0
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
            "<span style='display:inline-block;width:10px;height:10px;background:#b56cf0;border:2px solid #e0b3ff'></span> first report's area (lit until tested)",
            "<br><span style='display:inline-block;width:10px;height:10px;border-radius:50%;background:#ff4d4d;border:2px solid #fff'></span> first report",
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
      shiny::updateCheckboxInput(session, "tracing",
                                 label = sprintf("Contact tracing · %s", format_round(budget$tracing_cost)))
      testing <- budget$testing
      shiny::updateCheckboxInput(
        session, "build_testing",
        label = if (isTRUE(testing$can_build)) {
          sprintf("Improve PCR · %s once, takes %d reports (cheaper, more tests)",
                  format_round(testing$build_cost), testing$build_turns)
        } else if (isTRUE(testing$building)) "Improve PCR · under way" else "Improve PCR · labs at their best"
      )
      shiny::updateCheckboxInput(
        session, "develop_rapid",
        label = if (isTRUE(testing$can_develop_rapid)) {
          sprintf("Develop rapid tests · %s a report for %d reports (cheap home tests; lockdown works better)",
                  format_round(testing$rapid_cost), testing$rapid_turns)
        } else if (isTRUE(testing$developing_rapid)) {
          sprintf("Develop rapid tests · under way, %s a report", format_round(testing$rapid_cost))
        } else "Develop rapid tests · done"
      )
    })

    # London-wide measures with this place's prices; lockdown only once there
    # have been enough deaths or hospitals are filling up
    output$interventions_ui <- shiny::renderUI({
      req(ready())
      budget <- budget_info()
      defs <- budget$interventions
      allowed <- !identical(budget$lockdown_allowed, FALSE)
      trigger <- teaching_params()$lockdown_trigger

      names <- sprintf("%s · %s", defs$label, format_round(defs$cost))
      if (!allowed) {
        names[defs$id == "distancing"] <- sprintf(
          "%s · only after %d deaths or beds %d%% full", defs$label[defs$id == "distancing"],
          trigger$deaths, round(100 * trigger$bed_share)
        )
      }
      selected <- shiny::isolate(input$interventions) %||% character()
      if (!allowed) selected <- setdiff(selected, "distancing")

      boxes <- shiny::checkboxGroupInput(
        session$ns("interventions"), label = NULL,
        choiceNames = as.list(names), choiceValues = as.list(defs$id), selected = selected
      )
      if (!allowed) {
        boxes <- htmltools::tagQuery(boxes)$
          find("input")$filter(function(x, i) identical(x$attribs$value, "distancing"))$
          addAttrs(disabled = NA)$
          parent()$addAttrs(style = "opacity: 0.5;")$
          allTags()
      }
      boxes
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
          sprintf("%s starts working the report after next.", def$label)
        } else {
          sprintf("%s: %d%% strength.", def$label, round(100 * strength))
        })
      }

      if (length(restricted()) > 0) {
        lr <- teaching_params()$local_restrictions
        next_streaks <- budget$local_streaks[restricted()] + 1L
        strengths <- vapply(next_streaks, intervention_strength, numeric(1), lag = lr$lag, fatigue = lr$fatigue)
        lines <- c(lines, sprintf(
          "Restrictions on %d area%s: %s.",
          length(restricted()), if (length(restricted()) == 1) "" else "s",
          if (all(strengths == 0)) "start working the report after next" else
            paste0(round(100 * max(strengths)), "% strength")
        ))
      }

      if (isTRUE(input$tracing)) {
        lines <- c(lines, sprintf("Tracing: up to %d cases a report.", teaching_params()$tracing$capacity))
      }
      if (isTRUE(budget$testing$developing_rapid)) {
        lines <- c(lines, sprintf("Rapid tests: %d report%s to go.", budget$testing$rapid_left,
                                  if (budget$testing$rapid_left == 1) "" else "s"))
      }
      if (isTRUE(budget$testing$building)) {
        lines <- c(lines, sprintf("Building testing: %d report%s left, half capacity meanwhile.",
                                  budget$testing$build_left, if (budget$testing$build_left == 1) "" else "s"))
      }

      if (length(lines) == 0) {
        return(NULL)
      }
      shiny::HTML(paste(lines, collapse = "<br>"))
    })

    # test choices show today's price and wait; antigen is always listed but
    # greyed out until testing has been built up to a level that has it
    output$test_type_ui <- shiny::renderUI({
      req(ready())
      budget <- budget_info()
      testing <- budget$testing
      ag_available <- isTRUE(testing$ag_available)

      wait_days <- testing$delays$pcr * teaching_params()$report_days
      pcr_label <- sprintf("PCR · %s each · %s", budget$costs[["pcr"]],
                           if (wait_days == 0) "instant" else paste(wait_days, "days"))
      ag_label <- if (ag_available) {
        sprintf("Antigen · %s each · instant", budget$costs[["ag"]])
      } else {
        "Antigen · develop rapid tests first"
      }

      selected <- if (ag_available) shiny::isolate(input$test_type) %||% "pcr" else "pcr"
      buttons <- shiny::radioButtons(
        session$ns("test_type"),
        label = "Which test",
        choiceNames = list(ag_label, pcr_label),
        choiceValues = list("ag", "pcr"),
        selected = selected
      )
      if (!ag_available) {
        buttons <- htmltools::tagQuery(buttons)$
          find("input")$filter(function(x, i) identical(x$attribs$value, "ag"))$
          addAttrs(disabled = NA)$
          parent()$addAttrs(style = "opacity: 0.5;")$
          allTags()
      }
      buttons
    })

    # one line saying what this plan will do
    output$allocation_summary <- shiny::renderText({
      req(ready())
      p <- plan()
      budget <- budget_info()
      n_tests <- sum(p$allocations$n_ag + p$allocations$n_pcr)
      test_name <- if (p$test == "ag") "antigen" else "PCR"

      if (p$asked <= 0) {
        return("No tests planned.")
      }

      if (length(p$targets) == 0) {
        return(if (p$where == "positives") "No positives yet, so no tests." else "Pick areas on the map.")
      }

      where_txt <- if (p$where == "everywhere") paste("all", nrow(pop_tbl), "areas") else
        paste(length(p$targets), if (length(p$targets) == 1) "area" else "areas")
      txt <- paste0(format(n_tests, big.mark = ",", scientific = FALSE), " ", test_name, " tests in ", where_txt, ".")

      # symptomatic testing can only reach the people who come forward
      if (identical(input$who, "symptomatic") && !is.null(budget$pool_size)) {
        reachable <- sum(pmin(p$allocations$n_ag + p$allocations$n_pcr, budget$pool_size))
        if (reachable < n_tests) {
          txt <- paste0(txt, " Only about ", format(reachable, big.mark = ",", scientific = FALSE), " people come forward.")
        }
      }
      if (p$cut_down) {
        txt <- paste0(txt, " Cut to fit the money left.")
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
      layers <- map_layers(res, reports(), pop_tbl, boros$borough_name)
      layer <- layers[[input$map_layer %||% "positivity"]]

      # fixed steps, pale blue to deep red: the map only ever gets deeper as
      # things get worse, never rescaled. Nothing reported: dark slate.
      fill_cols <- map_colours(layer$values, layer$breaks)

      # hover: every layer's number for the area, plus the plan
      labels <- lapply(seq_len(nrow(boros)), function(i) {
        name <- boros$borough_name[i]
        pos <- layers$positivity$values[i]
        result_txt <- if (is.na(pos)) "No test results this report" else
          sprintf("%.1f%% of %s tests positive", 100 * pos, format(layers$positivity$tests[i], big.mark = ","))
        hosp_txt <- sprintf("%s admissions, %s deaths reported",
                            layers$admissions$counts[i], layers$deaths$counts[i])

        planned <- allocs$n_ag[i] + allocs$n_pcr[i]
        alloc_txt <- if (planned > 0) sprintf("<br><small>Planned: %d tests</small>", planned) else ""
        restrict_txt <- if (i %in% restricted()) "<br><small><b>Restricted</b></small>" else ""

        htmltools::HTML(paste0("<strong>", name, "</strong><br>", result_txt, "<br>", hosp_txt, alloc_txt, restrict_txt))
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
        if (identical(input$map_layer %||% "positivity", "positivity") && is.na(layer$values[f])) {
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
          colors = c("#3d4f61", map_palette),
          labels = c(layer$none_label, layer$step_labels),
          title = layer$title,
          opacity = 0.9
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
        shiny::updateCheckboxInput(session, "develop_rapid", value = FALSE)
      },
      # new game: everything starts off and at zero; the player decides
      clear_interventions = function(fair_share = 0, max_spend = NULL) {
        shiny::updateCheckboxGroupInput(session, "interventions", selected = character())
        shiny::updateCheckboxInput(session, "tracing", value = FALSE)
        shiny::updateCheckboxInput(session, "build_testing", value = FALSE)
        shiny::updateCheckboxInput(session, "develop_rapid", value = FALSE)
        shiny::updateRadioButtons(session, "who", selected = "symptomatic")
        shiny::updateRadioButtons(session, "where", selected = "everywhere")
        # the slider's range follows this place's per-report limit (London: 300,000)
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
      develop_rapid = shiny::reactive(isTRUE(input$develop_rapid)),
      # everything planned this turn (tests and measures), for the top bar
      planned_total = shiny::reactive(other_cost() + plan()$money)
    )
  })
}
