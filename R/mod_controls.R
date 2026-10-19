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
        shiny::checkboxInput(ns("random_seed"), "Random", value = teaching_params()$random_seed_default)
      )
    ),
    shiny::uiOutput(ns("admin_controls")),

    shiny::hr(),


    width = NULL
  )
}

mod_controls_server <- function(id, is_admin = NULL, shared_admin = NULL, reset_signal = NULL, borough_names = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(is_admin)) {
      is_admin <- function() FALSE
    }

    scenario_defs <- scenario_catalog()
    default_scenario <- scenario_lookup("custom", scenario_defs)
    default_death_ifr <- 0.002
    default_death_delay <- 6L
    admin_defaults <- list(
      Rt = default_scenario$Rt[[1]],
      k = default_scenario$k[[1]],
      imports_prob = default_scenario$imports_prob[[1]],
      lambda_import = default_scenario$lambda_import[[1]],
      seed_borough = default_scenario$seed_borough[[1]],
      seed_cases = default_scenario$seed_cases[[1]],
      kernel_key = default_scenario$kernel_key[[1]],
      susceptible_frac = default_scenario$susceptible_frac[[1]],
      scenario_id = default_scenario$id[[1]],
      death_ifr = default_death_ifr,
      death_delay = default_death_delay
    )

    if (is.null(shared_admin)) {
      shared_admin <- shiny::reactiveValues(
        Rt = admin_defaults$Rt,
        k = admin_defaults$k,
        imports_prob = admin_defaults$imports_prob,
        lambda_import = admin_defaults$lambda_import,
        seed_borough = admin_defaults$seed_borough,
        seed_cases = admin_defaults$seed_cases,
        kernel_key = admin_defaults$kernel_key,
        susceptible_frac = admin_defaults$susceptible_frac,
        scenario_id = admin_defaults$scenario_id,
        death_ifr = admin_defaults$death_ifr,
        death_delay = admin_defaults$death_delay
      )
    }

    if (is.null(reset_signal)) {
      reset_signal <- shiny::reactiveVal(0L)
    }

    seed_choices <- c("Random" = "random")
    if (!is.null(borough_names) && length(borough_names) > 0) {
      seed_choices <- c(seed_choices, stats::setNames(seq_along(borough_names), borough_names))
    }
    scenario_labels <- scenario_choices(scenario_defs)

    seed_choice_from_value <- function(seed_value) {
      if (is.null(seed_value) || is.na(seed_value)) {
        "random"
      } else {
        as.character(seed_value)
      }
    }

    apply_scenario_defaults <- function(scenario_id) {
      scenario <- scenario_lookup(scenario_id, scenario_defs)
      death_ifr <- scenario$death_ifr[[1]]
      if (is.null(death_ifr) || is.na(death_ifr)) {
        death_ifr <- shared_admin$death_ifr
      }
      death_ifr <- min(1, max(0, death_ifr))
      death_delay <- scenario$death_delay[[1]]
      if (is.null(death_delay) || is.na(death_delay)) {
        death_delay <- shared_admin$death_delay
      }
      death_delay <- as.integer(round(death_delay))
      if (is.na(death_delay) || death_delay < 0L) {
        death_delay <- 0L
      }

      shared_admin$Rt <- scenario$Rt[[1]]
      shared_admin$k <- scenario$k[[1]]
      shared_admin$imports_prob <- scenario$imports_prob[[1]]
      shared_admin$lambda_import <- scenario$lambda_import[[1]]
      shared_admin$seed_borough <- scenario$seed_borough[[1]]
      shared_admin$seed_cases <- scenario$seed_cases[[1]]
      shared_admin$kernel_key <- scenario$kernel_key[[1]]
      shared_admin$susceptible_frac <- scenario$susceptible_frac[[1]]
      shared_admin$scenario_id <- scenario$id[[1]]
      shared_admin$death_ifr <- death_ifr
      shared_admin$death_delay <- death_delay

      shiny::updateNumericInput(
        session,
        "admin_rt",
        value = scenario$Rt[[1]]
      )
      shiny::updateNumericInput(
        session,
        "admin_k",
        value = scenario$k[[1]]
      )
      shiny::updateNumericInput(
        session,
        "admin_imports_prob",
        value = scenario$imports_prob[[1]]
      )
      shiny::updateNumericInput(
        session,
        "admin_lambda_import",
        value = scenario$lambda_import[[1]]
      )
      shiny::updateSelectInput(
        session,
        "admin_seed_borough",
        selected = seed_choice_from_value(scenario$seed_borough[[1]])
      )
      shiny::updateNumericInput(
        session,
        "admin_seed_cases",
        value = scenario$seed_cases[[1]]
      )
      shiny::updateNumericInput(
        session,
        "admin_death_ifr",
        value = death_ifr
      )
      shiny::updateNumericInput(
        session,
        "admin_death_delay",
        value = death_delay
      )
    }

    # Fixed parameters - students focus on testing strategy, not epidemiology.
    # Values come from teaching_params() so they can be tuned in one place.
    tp <- teaching_params()
    fixed_params <- list(
      testing = list(
        sens = tp$tests$sens,
        spec = tp$tests$spec,
        delays = tp$tests$delays,
        prev_floor = tp$tests$prev_floor
      ),
      budget = list(
        budget = tp$tests$budget,
        costs = tp$tests$costs
      ),
      starter_intel = "alert"
    )

    params_epidemic <- shiny::reactive({
      list(
        Rt = shared_admin$Rt,
        k = shared_admin$k,
        imports_prob = shared_admin$imports_prob,
        lambda_import = shared_admin$lambda_import
      )
    })

    params_mortality <- shiny::reactive({
      list(
        death_ifr = shared_admin$death_ifr,
        death_delay = shared_admin$death_delay
      )
    })

    params_testing <- shiny::reactive({
      testing <- fixed_params$testing
      # who to test is now chosen in the testing plan on the map card
      testing$sampling_mode <- "symptomatic"
      testing$sampling_pool_frac <- tp$symptomatic$pool_frac
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

      seed_selected <- seed_choice_from_value(shiny::isolate(shared_admin$seed_borough))
      scenario_selected <- shiny::isolate(shared_admin$scenario_id)
      if (is.null(scenario_selected) || is.na(scenario_selected) || scenario_selected == "") {
        scenario_selected <- "custom"
      }
      if (!(scenario_selected %in% scenario_defs$id)) {
        scenario_selected <- "custom"
      }

      shiny::tagList(
        shiny::hr(),
        shiny::tags$h5("Admin controls"),
        shiny::tags$p(
          class = "text-muted small",
          "Changes apply to all sessions on the next turn."
        ),
        shiny::selectInput(
          ns("admin_scenario"),
          label = "Scenario template",
          choices = scenario_labels,
          selected = scenario_selected
        ),
        shiny::uiOutput(ns("admin_scenario_note")),
        shiny::numericInput(
          ns("admin_rt"),
          label = "R0 (R if nobody were immune)",
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
        shiny::numericInput(
          ns("admin_death_ifr"),
          label = "Death IFR",
          value = shiny::isolate(shared_admin$death_ifr),
          min = 0,
          max = 1,
          step = 0.001
        ),
        shiny::numericInput(
          ns("admin_death_delay"),
          label = "Death delay (turns)",
          value = shiny::isolate(shared_admin$death_delay),
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

    output$admin_scenario_note <- shiny::renderUI({
      if (!is_admin()) {
        return(NULL)
      }
      scenario_id <- input$admin_scenario
      if (is.null(scenario_id) || scenario_id == "" || is.na(scenario_id)) {
        scenario_id <- "custom"
      }
      scenario <- scenario_lookup(scenario_id, scenario_defs)
      objective <- scenario$objective[[1]]
      if (is.null(objective) || identical(objective, "")) {
        return(NULL)
      }
      shiny::tags$p(class = "text-muted small", objective)
    })

    observeEvent(
      list(
        input$admin_rt,
        input$admin_k,
        input$admin_imports_prob,
        input$admin_lambda_import,
        input$admin_seed_borough,
        input$admin_seed_cases,
        input$admin_death_ifr,
        input$admin_death_delay
      ),
      {
        shiny::req(is_admin())
        shiny::req(
          input$admin_rt,
          input$admin_k,
          input$admin_imports_prob,
          input$admin_lambda_import,
          input$admin_seed_borough,
          input$admin_seed_cases,
          input$admin_death_ifr,
          input$admin_death_delay
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

        death_ifr <- suppressWarnings(as.numeric(input$admin_death_ifr))
        if (is.na(death_ifr) || !is.finite(death_ifr)) {
          death_ifr <- 0
        }
        shared_admin$death_ifr <- min(1, max(0, death_ifr))

        death_delay <- suppressWarnings(as.integer(round(input$admin_death_delay)))
        if (is.na(death_delay) || death_delay < 0L) {
          death_delay <- 0L
        }
        shared_admin$death_delay <- death_delay
      },
      ignoreInit = TRUE
    )

    observeEvent(input$admin_scenario, {
      shiny::req(is_admin())
      scenario_id <- input$admin_scenario
      if (is.null(scenario_id) || scenario_id == "" || is.na(scenario_id)) {
        scenario_id <- "custom"
      }
      if (identical(scenario_id, shiny::isolate(shared_admin$scenario_id))) {
        return()
      }
      apply_scenario_defaults(scenario_id)
    }, ignoreInit = TRUE)

    observeEvent(input$admin_reset_all, {
      shiny::req(is_admin())
      reset_signal(reset_signal() + 1L)
    })

    observeEvent(input$admin_reset_defaults, {
      shiny::req(is_admin())
      scenario_id <- input$admin_scenario
      if (is.null(scenario_id) || scenario_id == "" || is.na(scenario_id)) {
        scenario_id <- "custom"
      }
      apply_scenario_defaults(scenario_id)
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
      mortality = params_mortality,
      testing = params_testing,
      budget = budget_info,
      starter_intel = starter_intel,
      end = shiny::reactive(input$end),
      reset = shiny::reactive(input$reset),
      reseed = reseed
    )
  })
}
