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
    shiny::uiOutput(ns("admin_controls")),
    shiny::actionButton(ns("reseed"), "New Game", class = "btn-outline-primary w-100 mb-2"),
    shiny::hr(),
    shiny::actionButton(ns("end"), "End & Reveal", class = "btn-success w-100 mb-2"),
    shiny::actionButton(ns("reset"), "Reset", class = "btn-outline-secondary w-100"),
    width = NULL
  )
}

mod_controls_server <- function(id, is_admin = NULL, shared_admin = NULL, reset_signal = NULL, borough_names = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(is_admin)) {
      is_admin <- function() FALSE
    }

    if (is.null(shared_admin)) {
      shared_admin <- shiny::reactiveValues(
        Rt = 1.3,
        k = 0.5,
        imports_prob = 0.01,
        lambda_import = 0.5,
        seed_borough = NA_integer_,
        seed_cases = 100L
      )
    }

    if (is.null(reset_signal)) {
      reset_signal <- shiny::reactiveVal(0L)
    }

    seed_choices <- c("Random" = "random")
    if (!is.null(borough_names) && length(borough_names) > 0) {
      seed_choices <- c(seed_choices, stats::setNames(seq_along(borough_names), borough_names))
    }

    # Fixed parameters - students focus on testing strategy, not epidemiology
    fixed_params <- list(
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

    admin_defaults <- list(
      Rt = 1.3,
      k = 0.5,
      imports_prob = 0.01,
      lambda_import = 0.5,
      seed_borough = NA_integer_,
      seed_cases = 100L
    )

    params_epidemic <- shiny::reactive({
      list(
        Rt = shared_admin$Rt,
        k = shared_admin$k,
        imports_prob = shared_admin$imports_prob,
        lambda_import = shared_admin$lambda_import
      )
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

    output$admin_controls <- shiny::renderUI({
      if (!is_admin()) {
        return(NULL)
      }

      seed_value <- shiny::isolate(shared_admin$seed_borough)
      if (is.null(seed_value) || is.na(seed_value)) {
        seed_selected <- "random"
      } else {
        seed_selected <- as.character(seed_value)
      }

      shiny::tagList(
        shiny::hr(),
        shiny::tags$h5("Admin controls"),
        shiny::tags$p(
          class = "text-muted small",
          "Changes apply to all sessions on the next turn."
        ),
        shiny::numericInput(
          ns("admin_rt"),
          label = "Rt",
          value = shiny::isolate(shared_admin$Rt),
          min = 0,
          step = 0.05
        ),
        shiny::numericInput(
          ns("admin_k"),
          label = "Overdispersion k",
          value = shiny::isolate(shared_admin$k),
          min = 0.01,
          step = 0.05
        ),
        shiny::numericInput(
          ns("admin_imports_prob"),
          label = "Imports probability",
          value = shiny::isolate(shared_admin$imports_prob),
          min = 0,
          max = 1,
          step = 0.01
        ),
        shiny::numericInput(
          ns("admin_lambda_import"),
          label = "Imports mean (lambda)",
          value = shiny::isolate(shared_admin$lambda_import),
          min = 0,
          step = 0.1
        ),
        shiny::selectInput(
          ns("admin_seed_borough"),
          label = "Initial seed borough",
          choices = seed_choices,
          selected = seed_selected
        ),
        shiny::numericInput(
          ns("admin_seed_cases"),
          label = "Initial seed cases",
          value = shiny::isolate(shared_admin$seed_cases),
          min = 0,
          step = 1
        ),
        shiny::actionButton(
          ns("admin_reset_all"),
          "Reset all sessions",
          class = "btn-outline-danger w-100"
        ),
        shiny::actionButton(
          ns("admin_reset_defaults"),
          "Reset to defaults",
          class = "btn-outline-secondary w-100 mt-2"
        )
      )
    })

    observeEvent(
      list(
        input$admin_rt,
        input$admin_k,
        input$admin_imports_prob,
        input$admin_lambda_import,
        input$admin_seed_borough,
        input$admin_seed_cases
      ),
      {
        shiny::req(is_admin())
        shiny::req(
          input$admin_rt,
          input$admin_k,
          input$admin_imports_prob,
          input$admin_lambda_import,
          input$admin_seed_borough,
          input$admin_seed_cases
        )

        shared_admin$Rt <- max(0, input$admin_rt)
        shared_admin$k <- max(0.01, input$admin_k)
        shared_admin$imports_prob <- min(1, max(0, input$admin_imports_prob))
        shared_admin$lambda_import <- max(0, input$admin_lambda_import)

        seed_choice <- input$admin_seed_borough
        if (is.null(seed_choice) || seed_choice == "random" || seed_choice == "") {
          shared_admin$seed_borough <- NA_integer_
        } else {
          seed_value <- suppressWarnings(as.integer(seed_choice))
          if (is.na(seed_value)) {
            seed_value <- NA_integer_
          }
          shared_admin$seed_borough <- seed_value
        }

        seed_cases <- suppressWarnings(as.integer(round(input$admin_seed_cases)))
        if (is.na(seed_cases) || seed_cases < 0L) {
          seed_cases <- 0L
        }
        shared_admin$seed_cases <- seed_cases
      },
      ignoreInit = TRUE
    )

    observeEvent(input$admin_reset_all, {
      shiny::req(is_admin())
      reset_signal(reset_signal() + 1L)
    })

    observeEvent(input$admin_reset_defaults, {
      shiny::req(is_admin())

      shared_admin$Rt <- admin_defaults$Rt
      shared_admin$k <- admin_defaults$k
      shared_admin$imports_prob <- admin_defaults$imports_prob
      shared_admin$lambda_import <- admin_defaults$lambda_import
      shared_admin$seed_borough <- admin_defaults$seed_borough
      shared_admin$seed_cases <- admin_defaults$seed_cases

      shiny::updateNumericInput(
        session,
        "admin_rt",
        value = admin_defaults$Rt
      )
      shiny::updateNumericInput(
        session,
        "admin_k",
        value = admin_defaults$k
      )
      shiny::updateNumericInput(
        session,
        "admin_imports_prob",
        value = admin_defaults$imports_prob
      )
      shiny::updateNumericInput(
        session,
        "admin_lambda_import",
        value = admin_defaults$lambda_import
      )
      shiny::updateSelectInput(
        session,
        "admin_seed_borough",
        selected = "random"
      )
      shiny::updateNumericInput(
        session,
        "admin_seed_cases",
        value = admin_defaults$seed_cases
      )
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
