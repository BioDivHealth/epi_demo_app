# Map module -----------------------------------------------------------------

mod_map_ui <- function(id, intervention_defs = intervention_catalog()) {
  ns <- shiny::NS(id)
  intervention_choices <- stats::setNames(
    intervention_defs$id,
    sprintf("%s (£%s)", intervention_defs$label, intervention_defs$cost)
  )
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span("Click boroughs to allocate tests"),
      shiny::span(
        class = "badge bg-info",
        shiny::textOutput(ns("budget_badge"), inline = TRUE)
      )
    ),
    shiny::div(
      class = "mb-2 d-flex flex-wrap gap-3 align-items-center p-2 bg-light rounded",
      shiny::div(
        class = "d-flex gap-2 align-items-center",
        shiny::tags$label(class = "mb-0", "Test type:"),
        shiny::radioButtons(
          ns("test_type"),
          label = NULL,
        choices = c("Antigen (fast)" = "ag", "PCR (accurate)" = "pcr"),
          selected = "ag",
          inline = TRUE
        )
      ),
      shiny::div(
        class = "d-flex gap-2 align-items-center",
        shiny::tags$label(class = "mb-0 small", "Tests per click:"),
        shiny::sliderInput(
          ns("tests_per_click"),
          label = NULL,
          min = 1,
          max = 20,
          value = 5,
          step = 1,
          width = "120px",
          ticks = FALSE
        )
      ),
      shiny::actionButton(
        ns("mass_test"),
        "Mass Testing",
        class = "btn-outline-primary btn-sm"
      ),
      shiny::tags$span(class = "ms-auto small text-muted", shiny::textOutput(ns("allocation_summary"), inline = TRUE))
    ),
    leaflet::leafletOutput(ns("map"), height = "400px"),
    shiny::hr(class = "my-2"),
    shiny::fluidRow(
      shiny::column(
        width = 7,
        shiny::div(
          class = "d-flex flex-wrap gap-2",
          shiny::tags$label(class = "form-label small mb-0 me-2", "Interventions:"),
          shiny::checkboxGroupInput(
            ns("interventions"),
            label = NULL,
            choices = intervention_choices,
            selected = character(),
            inline = TRUE
          )
        )
      ),
      shiny::column(
        width = 5,
        shiny::div(
          class = "d-flex gap-2 justify-content-end",
          shiny::div(class = "small text-muted align-self-center", shiny::textOutput(ns("turn_positivity"), inline = TRUE)),
          shiny::actionButton(ns("clear_alloc"), "Clear", class = "btn-outline-secondary btn-sm"),
          shiny::actionButton(ns("advance"), "Advance Turn", class = "btn-primary")
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
    pop_tbl,
    app_ready = shiny::reactive(TRUE),
    run_mode = shiny::reactive("manual"),
    auto_running = shiny::reactive(FALSE),
    intervention_defs = intervention_catalog()) {
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

    allocations <- shiny::reactiveVal(allocations_init)
    selected_interventions <- shiny::reactive({
      sel <- input$interventions
      if (is.null(sel)) character() else sel
    })
    map_locked <- shiny::reactive({
      identical(run_mode(), "guided_auto") && isTRUE(auto_running())
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
    bounds_list <- list(
      c(bbox[["ymin"]] - 0.02, bbox[["xmin"]] - 0.02),
      c(bbox[["ymax"]] + 0.02, bbox[["xmax"]] + 0.02)
    )
    reset_js <- sprintf(
      "function(btn, map){ map.fitBounds([[%.6f, %.6f],[%.6f, %.6f]]); }",
      bounds_list[[1]][1], bounds_list[[1]][2],
      bounds_list[[2]][1], bounds_list[[2]][2]
    )

    observeEvent(input$clear_alloc, {
      if (isTRUE(map_locked())) {
        return()
      }
      allocations(allocations_init)
    })

    observeEvent(input$mass_test, {
      if (isTRUE(map_locked())) {
        return()
      }

      budget <- budget_info()
      cost_ag <- budget$costs[["ag"]]
      if (is.null(cost_ag)) {
        cost_ag <- 0
      }
      if (cost_ag <= 0) {
        shiny::showNotification("Antigen cost is zero or missing.", type = "error")
        return()
      }

      allocs <- allocations()
      spend_stats <- summarise_budget(allocs, budget$costs, budget$budget)
      intervention_cost <- compute_intervention_effects(selected_interventions(), intervention_defs)$cost
      remaining <- budget$budget - spend_stats$spend - intervention_cost

      if (!is.finite(remaining) || remaining < cost_ag) {
        shiny::showNotification("Not enough budget for mass testing.", type = "message")
        return()
      }

      total_tests <- floor(remaining / cost_ag)
      if (total_tests <= 0) {
        shiny::showNotification("Not enough budget for mass testing.", type = "message")
        return()
      }

      weights <- pop_tbl$pop
      if (length(weights) != nrow(allocs)) {
        shiny::showNotification("Population data mismatch.", type = "error")
        return()
      }

      weights <- weights / sum(weights)
      base_alloc <- floor(total_tests * weights)
      remainder <- total_tests - sum(base_alloc)

      if (remainder > 0) {
        fractional <- (total_tests * weights) - base_alloc
        extra_idx <- order(fractional, decreasing = TRUE)[seq_len(remainder)]
        base_alloc[extra_idx] <- base_alloc[extra_idx] + 1L
      }

      allocs$n_ag <- allocs$n_ag + as.integer(base_alloc)
      allocations(allocs)

      shiny::showNotification(
        paste0("Mass testing allocated ", total_tests, " antigen tests."),
        type = "message"
      )
    })

    output$map <- leaflet::renderLeaflet({
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
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]],
          lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]],
          lat2 = bbox[["ymax"]]
        )
    })

    # Handle map clicks - add tests based on selected type
    observeEvent(input$map_shape_click, {
      if (isTRUE(map_locked())) {
        return()
      }

      req(ready())
      click <- input$map_shape_click
      req(click$id)
      idx <- match(click$id, allocations()$borough_name)
      req(!is.na(idx))

      test_type <- input$test_type
      if (is.null(test_type)) test_type <- "ag"

      qty <- input$tests_per_click
      if (is.null(qty) || !is.finite(qty)) qty <- 5L
      qty <- as.integer(qty)
      current_allocs <- allocations()
      col_name <- if (test_type == "pcr") "n_pcr" else "n_ag"
      current_value <- current_allocs[[col_name]][idx]
      new_value <- current_value + qty

      trial_allocs <- current_allocs
      trial_allocs[[col_name]][idx] <- new_value

      budget <- budget_info()
      spend_stats <- summarise_budget(trial_allocs, budget$costs, budget$budget)
      intervention_cost <- compute_intervention_effects(selected_interventions(), intervention_defs)$cost

      if ((budget$budget - spend_stats$spend - intervention_cost) < -1e-6) {
        shiny::showNotification("Budget exceeded", type = "error")
        return()
      }

      allocations(trial_allocs)
    })

    output$budget_badge <- shiny::renderText({
      req(ready())
      allocs <- allocations()
      budget <- budget_info()
      spend_stats <- summarise_budget(allocs, budget$costs, budget$budget)
      intervention_cost <- compute_intervention_effects(selected_interventions(), intervention_defs)$cost
      total_spend <- spend_stats$spend + intervention_cost
      remaining <- budget$budget - total_spend
      paste0("£", round(remaining, 0), " left")
    })

    observe({
      req(ready())
      budget <- budget_info()
      cost_ag <- budget$costs[["ag"]]
      cost_pcr <- budget$costs[["pcr"]]
      if (is.null(cost_ag)) cost_ag <- 0
      if (is.null(cost_pcr)) cost_pcr <- 0
      shiny::updateRadioButtons(
        session,
        "test_type",
        choices = stats::setNames(
          c("ag", "pcr"),
          c(
            sprintf("Antigen (fast) £%s", cost_ag),
            sprintf("PCR (accurate) £%s", cost_pcr)
          )
        )
      )
    })

    output$allocation_summary <- shiny::renderText({
      req(ready())
      allocs <- allocations()
      total_ag <- sum(allocs$n_ag)
      total_pcr <- sum(allocs$n_pcr)
      if (total_ag + total_pcr == 0) {
        "No tests allocated"
      } else {
        paste0(total_ag, " Ag + ", total_pcr, " PCR tests")
      }
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

    # Update map with results and allocation markers
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
      allocs <- allocations()

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
      palette <- leaflet::colorNumeric(
        palette = "YlOrRd",
        domain = c(0, 1),
        na.color = "#e0e0e0"
      )

      fill_cols <- ifelse(is.na(values), "#e0e0e0", palette(values))

      # Build labels including allocations
      labels <- lapply(seq_len(nrow(boros)), function(i) {
        name <- boros$borough_name[i]
        val <- values[i]
        alloc_idx <- match(name, allocs$borough_name)

        result_txt <- if (is.na(val)) {
          "<em>No results yet</em>"
        } else if (val == 0) {
          "<span style='color:green'>0% positive</span>"
        } else {
          sprintf("<span style='color:#d95f02'>%.1f%% positive</span>", 100 * val)
        }

        alloc_txt <- ""
        if (!is.na(alloc_idx)) {
          ag <- allocs$n_ag[alloc_idx]
          pcr <- allocs$n_pcr[alloc_idx]
          if (ag + pcr > 0) {
            alloc_txt <- sprintf("<br><small>Allocated: %d Ag, %d PCR</small>", ag, pcr)
          }
        }

        htmltools::HTML(paste0("<strong>", name, "</strong><br>", result_txt, alloc_txt))
      })

      proxy <- leaflet::leafletProxy(ns("map"), data = boros) |>
        leaflet::clearGroup("boroughs") |>
        leaflet::clearGroup("alloc_markers") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = fill_cols,
          label = labels,
          group = "boroughs",
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )

      # Add allocation markers
      allocs_with_coords <- allocs |>
        dplyr::filter(n_ag > 0 | n_pcr > 0) |>
        dplyr::left_join(centroid_tbl, by = "borough_name")

      if (nrow(allocs_with_coords) > 0) {
        total_tests <- allocs_with_coords$n_ag + allocs_with_coords$n_pcr
        radius <- 4 + sqrt(total_tests)

        marker_labels <- lapply(seq_len(nrow(allocs_with_coords)), function(i) {
          htmltools::HTML(sprintf(
            "%s<br>Ag: %d, PCR: %d",
            allocs_with_coords$borough_name[i],
            allocs_with_coords$n_ag[i],
            allocs_with_coords$n_pcr[i]
          ))
        })

        proxy <- proxy |>
          leaflet::addCircleMarkers(
            lng = allocs_with_coords$lng,
            lat = allocs_with_coords$lat,
            radius = radius,
            color = "#2166ac",
            fillColor = "#2166ac",
            fillOpacity = 0.6,
            stroke = TRUE,
            weight = 2,
            label = marker_labels,
            group = "alloc_markers"
          )
      }
    })

    list(
      allocations = allocations,
      set_allocations = function(new_allocs) {
        if (is.null(new_allocs)) {
          return(invisible(NULL))
        }
        allocs_norm <- normalise_policy_allocations(new_allocs, pop_tbl)
        allocations(allocs_norm)
      },
      reset = function() {
        allocations(allocations_init)
      },
      clear_interventions = function() {
        shiny::updateCheckboxGroupInput(session, "interventions", selected = character())
      },
      interventions = selected_interventions,
      advance = shiny::eventReactive(input$advance, {
        if (isTRUE(map_locked())) {
          return(NULL)
        }
        input$advance
      }, ignoreNULL = TRUE)
    )
  })
}
