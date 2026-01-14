# Controls module -------------------------------------------------------------

mod_controls_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    style = "height: 100%; overflow-y: auto;",
    bslib::card_header("Simulation Controls"),
    shiny::radioButtons(
      ns("scenario_mode"),
      label = "Scenario mode",
      choices = c("Default" = "default", "Custom" = "custom"),
      selected = "default",
      inline = TRUE
    ),
    shiny::helpText("Default uses baseline parameters; switch to Custom to adjust assumptions."),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'custom'", ns("scenario_mode")),
      shiny::sliderInput(ns("Rt"), "Rt (global)", min = 0.6, max = 2.2, value = 1.2, step = 0.05),
      shiny::sliderInput(ns("k"), "Overdispersion k", min = 0.1, max = 2, value = 0.7, step = 0.05),
      shiny::sliderInput(ns("imports_prob"), "Import probability", min = 0, max = 0.05, value = 0.01, step = 0.005),
      shiny::sliderInput(ns("lambda_import"), "Import size", min = 0, max = 3, value = 0.5, step = 0.1),
      bslib::card_header("Test performance"),
      shiny::sliderInput(ns("ag_se"), "Ag sensitivity", min = 0.5, max = 1, value = 0.85, step = 0.01),
      shiny::sliderInput(ns("ag_sp"), "Ag specificity", min = 0.8, max = 1, value = 0.97, step = 0.005),
      shiny::sliderInput(ns("pcr_se"), "PCR sensitivity", min = 0.7, max = 1, value = 0.95, step = 0.01),
      shiny::sliderInput(ns("pcr_sp"), "PCR specificity", min = 0.8, max = 1, value = 0.995, step = 0.001),
      bslib::card_header("Turnaround delays"),
      shiny::sliderInput(ns("ag_delay"), "Ag delay (turns)", min = 0, max = 1, value = 0, step = 1),
      shiny::sliderInput(ns("pcr_delay"), "PCR delay (turns)", min = 1, max = 3, value = 2, step = 1),
      bslib::card_header("Budget"),
      shiny::numericInput(ns("budget"), "Budget per turn", value = 200, min = 0, step = 10),
      shiny::numericInput(ns("cost_ag"), "Ag cost", value = 5, min = 0, step = 1),
      shiny::numericInput(ns("cost_pcr"), "PCR cost", value = 20, min = 0, step = 1),
      bslib::card_header("Starter intel"),
      shiny::selectInput(
        ns("starter_intel"),
        label = "Initial signal",
        choices = c(
          "None" = "none",
          "Alert (targeted PCR)" = "alert",
          "Wider screening" = "sweep"
        ),
        selected = "none"
      ),
    ),
    bslib::card_header("Simulation"),
    shiny::numericInput(ns("rng_seed"), "RNG seed", value = 2024, min = 1, step = 1),
    shiny::actionButton(ns("reseed"), "Reseed RNG"),
    shiny::actionButton(ns("end"), "End simulation"),
    shiny::actionButton(ns("reset"), "Reset"),
    width = NULL
  )
}

mod_controls_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    default_params <- list(
      epidemiology = list(
        Rt = 1.2,
        k = 0.7,
        imports_prob = 0.01,
        lambda_import = 0.5
      ),
      testing = list(
        sens = list(ag = 0.85, pcr = 0.95),
        spec = list(ag = 0.97, pcr = 0.995),
        delays = list(ag = 0, pcr = 2),
        prev_floor = 1e-6
      ),
      budget = list(
        budget = 200,
        costs = list(ag = 5, pcr = 20)
      ),
      starter_intel = "none"
    )

    observeEvent(input$scenario_mode, {
      if (input$scenario_mode == "default") {
        shiny::updateSliderInput(session, "Rt", value = default_params$epidemiology$Rt)
        shiny::updateSliderInput(session, "k", value = default_params$epidemiology$k)
        shiny::updateSliderInput(session, "imports_prob", value = default_params$epidemiology$imports_prob)
        shiny::updateSliderInput(session, "lambda_import", value = default_params$epidemiology$lambda_import)
        shiny::updateSliderInput(session, "ag_se", value = default_params$testing$sens$ag)
        shiny::updateSliderInput(session, "ag_sp", value = default_params$testing$spec$ag)
        shiny::updateSliderInput(session, "pcr_se", value = default_params$testing$sens$pcr)
        shiny::updateSliderInput(session, "pcr_sp", value = default_params$testing$spec$pcr)
        shiny::updateSliderInput(session, "ag_delay", value = default_params$testing$delays$ag)
        shiny::updateSliderInput(session, "pcr_delay", value = default_params$testing$delays$pcr)
        shiny::updateNumericInput(session, "budget", value = default_params$budget$budget)
        shiny::updateNumericInput(session, "cost_ag", value = default_params$budget$costs$ag)
        shiny::updateNumericInput(session, "cost_pcr", value = default_params$budget$costs$pcr)
        shiny::updateSelectInput(session, "starter_intel", selected = default_params$starter_intel)
      }
    }, ignoreNULL = FALSE)

    params_epidemic <- shiny::reactive({
      if (input$scenario_mode == "default") {
        default_params$epidemiology
      } else {
        list(
          Rt = input$Rt,
          k = input$k,
          imports_prob = input$imports_prob,
          lambda_import = input$lambda_import
        )
      }
    })

    params_testing <- shiny::reactive({
      if (input$scenario_mode == "default") {
        default_params$testing
      } else {
        list(
          sens = list(ag = input$ag_se, pcr = input$pcr_se),
          spec = list(ag = input$ag_sp, pcr = input$pcr_sp),
          delays = list(ag = input$ag_delay, pcr = input$pcr_delay),
          prev_floor = 1e-6
        )
      }
    })

    budget_info <- shiny::reactive({
      if (input$scenario_mode == "default") {
        default_params$budget
      } else {
        list(
          budget = input$budget,
          costs = list(ag = input$cost_ag, pcr = input$cost_pcr)
        )
      }
    })

    starter_intel <- shiny::reactive({
      if (input$scenario_mode == "default") {
        default_params$starter_intel
      } else {
        input$starter_intel
      }
    })

    reseed <- shiny::eventReactive(input$reseed, {
      input$rng_seed
    }, ignoreNULL = FALSE)

    list(
      epidemiology = params_epidemic,
      testing = params_testing,
      budget = budget_info,
      starter_intel = starter_intel,
      end = shiny::reactive(input$end),
      reset = shiny::reactive(input$reset),
      reseed = reseed
    )
  })
}
