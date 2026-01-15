# Controls module -------------------------------------------------------------

mod_controls_ui <- function(id) {

  ns <- shiny::NS(id)
  bslib::card(
    style = "height: 100%; overflow-y: auto;",
    bslib::card_header("Game Controls"),
    shiny::div(
      class = "mb-3",
      shiny::tags$p(
        class = "text-muted small",
        "Allocate tests on the map, then advance turns to see results."
      )
    ),
    shiny::hr(),
    shiny::div(
      class = "mb-3 controls-seed-block",
      shiny::tags$label(class = "form-label", "RNG Seed"),
      shiny::div(
        class = "d-flex gap-2 align-items-center",
        shiny::numericInput(ns("rng_seed"), label = NULL, value = 2024, min = 1, step = 1, width = "100%"),
        shiny::checkboxInput(ns("random_seed"), "Random", value = FALSE)
      )
    ),
    shiny::div(
      class = "mb-3",
      shiny::tags$label(class = "form-label", "Testing pool"),
      shiny::radioButtons(
        ns("testing_pool"),
        label = NULL,
        choices = c("Symptomatic" = "symptomatic", "Random" = "random"),
        selected = "symptomatic",
        inline = TRUE
      )
    ),
    shiny::actionButton(ns("reseed"), "New Game", class = "btn-outline-primary w-100 mb-2"),
    shiny::hr(),
    shiny::actionButton(ns("end"), "End & Reveal", class = "btn-success w-100 mb-2"),
    shiny::actionButton(ns("reset"), "Reset", class = "btn-outline-secondary w-100"),
    width = NULL
  )
}

mod_controls_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # Fixed parameters - students focus on testing strategy, not epidemiology
    fixed_params <- list(
      epidemiology = list(
        Rt = 1.3,
        k = 0.5,
        imports_prob = 0.01,
        lambda_import = 0.5
      ),
      testing = list(
        sens = list(ag = 0.80, pcr = 0.98),
        spec = list(ag = 0.995, pcr = 0.995),
        delays = list(ag = 0, pcr = 1),
        prev_floor = 1e-6
      ),
      budget = list(
        budget = 2000,
        costs = list(ag = 2, pcr = 10)
      ),
      starter_intel = "alert"
    )

    params_epidemic <- shiny::reactive({
      fixed_params$epidemiology
    })

    params_testing <- shiny::reactive({
      testing <- fixed_params$testing
      mode <- input$testing_pool
      if (is.null(mode)) {
        mode <- "symptomatic"
      }
      testing$sampling_mode <- mode
      testing$sampling_pool_frac <- 0.1
      testing
    })

    budget_info <- shiny::reactive({
      fixed_params$budget
    })

    starter_intel <- shiny::reactive({
      fixed_params$starter_intel
    })

    reseed <- shiny::eventReactive(input$reseed, {
      if (isTRUE(input$random_seed)) {
        sample.int(1e6, 1)
      } else {
        input$rng_seed
      }
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
