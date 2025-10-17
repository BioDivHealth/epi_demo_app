# Map module -----------------------------------------------------------------

mod_map_ui <- function(id, intervention_defs = intervention_catalog()) {
  ns <- shiny::NS(id)
  intervention_choices <- stats::setNames(
    intervention_defs$id,
    sprintf("%s (cost £%s)", intervention_defs$label, intervention_defs$cost)
  )
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Testing allocation"),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        leaflet::leafletOutput(ns("map"), height = 500)
      ),
      shiny::column(
        width = 4,
        shiny::radioButtons(
          ns("allocation_mode"),
          "Mode",
          choices = c("Add" = "add", "Remove" = "remove"),
          selected = "add"
        ),
        shiny::radioButtons(
          ns("allocation_type"),
          "Test type",
          choices = c("Antigen" = "ag", "PCR" = "pcr"),
          selected = "ag"
        ),
        shiny::numericInput(ns("allocation_amount"), "Quantity per click", value = 5, min = 1, step = 1),
        shiny::actionButton(ns("clear_alloc"), "Clear allocations"),
        shiny::div(class = "mt-3", shiny::strong("Remaining budget:")),
        shiny::textOutput(ns("budget_text")),
        shiny::div(class = "mt-4", shiny::strong("Interventions")),
        shiny::checkboxGroupInput(
          ns("interventions"),
          label = NULL,
          choices = intervention_choices,
          selected = character()
        ),
        shiny::helpText("Intervention cost applies in addition to testing spend."),
        shiny::textOutput(ns("intervention_cost_text")),
        shiny::div(class = "mt-3", shiny::strong("This turn positivity:")),
        shiny::textOutput(ns("turn_positivity"))
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
    intervention_defs = intervention_catalog()) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    allocations <- shiny::reactiveVal(allocations_init)
    selected_interventions <- shiny::reactive({
      sel <- input$interventions
      if (is.null(sel)) character() else sel
    })

    observeEvent(input$clear_alloc, {
      allocations(allocations_init)
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(boros) |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = "#cccccc",
          label = ~borough_name,
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )
    })

    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      req(click$id)
      idx <- match(click$id, allocations()$borough_name)
      req(!is.na(idx))

      mode <- input$allocation_mode
      type <- input$allocation_type
      qty <- max(1L, as.integer(input$allocation_amount))
      current_allocs <- allocations()

      col_name <- if (type == "ag") "n_ag" else "n_pcr"
      current_value <- current_allocs[[col_name]][idx]

      new_value <- current_value
      if (mode == "add") {
        new_value <- current_value + qty
      } else {
        new_value <- max(0L, current_value - qty)
      }

      trial_allocs <- current_allocs
      trial_allocs[[col_name]][idx] <- new_value

      budget <- budget_info()
      spend_stats <- summarise_budget(trial_allocs, budget$costs, budget$budget)
      intervention_cost <- compute_intervention_effects(selected_interventions(), intervention_defs)$cost

      if (mode == "add" && (budget$budget - spend_stats$spend - intervention_cost) < -1e-6) {
        shiny::showNotification("Budget exceeded", type = "error")
        return()
      }

      allocations(trial_allocs)
    })

    output$budget_text <- shiny::renderText({
      allocs <- allocations()
      budget <- budget_info()
      spend_stats <- summarise_budget(allocs, budget$costs, budget$budget)
      intervention_cost <- compute_intervention_effects(selected_interventions(), intervention_defs)$cost
      total_spend <- spend_stats$spend + intervention_cost
      remaining <- budget$budget - total_spend
      paste0(
        "£", format(round(remaining, 1), big.mark = ","),
        " remaining (spend £", format(round(total_spend, 1), big.mark = ","), ")"
      )
    })

    observe({
      latest <- observed_this_turn()
      total_tests <- sum(latest$n, na.rm = TRUE)
      positives <- sum(latest$positives, na.rm = TRUE)
      output$turn_positivity <- shiny::renderText({
        if (total_tests == 0) {
          "No results"
        } else {
          paste0(round(100 * positives / total_tests, 1), "%")
        }
      })
    })

    observe({
      res <- observed_this_turn()
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

      palette <- leaflet::colorNumeric(
        palette = "YlOrRd",
        domain = c(0, 1),
        na.color = "#cccccc"
      )

      fill_cols <- ifelse(is.na(values), "#cccccc", palette(values))

      leaflet::leafletProxy(ns("map"), data = boros) |>
        leaflet::clearShapes() |>
        leaflet::addPolygons(
          layerId = boros$borough_name,
          weight = 1,
          color = "#555555",
          fillOpacity = 0.7,
          fillColor = fill_cols,
          label = lapply(seq_len(nrow(boros)), function(i) {
            val <- values[i]
            txt <- if (is.na(val)) "No results" else paste0(round(100 * val, 1), "% positive")
            htmltools::HTML(paste0("<strong>", boros$borough_name[i], "</strong><br>", txt))
          }),
          highlight = leaflet::highlightOptions(weight = 2, bringToFront = TRUE)
        )
    })

    output$intervention_cost_text <- shiny::renderText({
      eff <- compute_intervention_effects(selected_interventions(), intervention_defs)
      if (eff$cost <= 0) {
        "No intervention cost this turn"
      } else {
        paste0("Intervention spend: £", format(round(eff$cost, 1), big.mark = ","))
      }
    })

    list(
      allocations = allocations,
      reset = function() {
        allocations(allocations_init)
      },
      clear_interventions = function() {
        shiny::updateCheckboxGroupInput(session, "interventions", selected = character())
      },
      interventions = selected_interventions
    )
  })
}
