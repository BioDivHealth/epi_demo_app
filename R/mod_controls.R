# Controls module -------------------------------------------------------------

mod_controls_testing_pool_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "map-selection-group map-selection-group-testing-pool",
    shiny::tags$label(class = "map-selection-label", "Testing pool"),
    shiny::div(
      class = "map-selection-options toggle-btn-group",
      shiny::radioButtons(
        ns("testing_pool"),
        label = NULL,
        choices = c("Symptomatic" = "symptomatic", "Random" = "random"),
        selected = "random",
        inline = TRUE
      )
    )
  )
}

mod_controls_header_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "header-game-controls",
    shiny::div(
      class = "header-seed-controls",
      shiny::tags$label(
        class = "header-seed-label text-muted small mb-0",
        shiny::tags$strong("Seed")
      ),
      shiny::numericInput(
        ns("rng_seed"),
        label = NULL,
        value = 2024,
        min = 1,
        step = 1,
        width = "48px"
      ),
      shiny::checkboxInput(ns("random_seed"), "Random", value = FALSE)
    ),
    shiny::actionButton(ns("reseed"), "New Game", class = "btn-outline-primary btn-sm"),
    shiny::actionButton(ns("end"), "End & Reveal", class = "btn-success btn-sm"),
    shiny::actionButton(ns("reset"), "Reset", class = "btn-outline-secondary btn-sm")
  )
}

mod_controls_run_mode_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "map-selection-group map-selection-group-run-mode",
    shiny::div(
      class = "map-selection-options toggle-btn-group-vertical",
      shiny::radioButtons(
        ns("run_mode"),
        label = NULL,
        choices = c("Guided auto" = "guided_auto", "Manual" = "manual"),
        selected = "guided_auto",
        inline = FALSE
      )
    )
  )
}

mod_controls_borough_policy_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "map-selection-group map-selection-group-borough-policy",
    shiny::tags$label(class = "map-selection-label", "Borough policy"),
    shiny::div(
      class = "map-selection-options toggle-btn-group",
      shiny::radioButtons(
        ns("allocation_policy"),
        label = NULL,
        choices = c(
          "Manual" = "manual",
          "Random" = "random_population",
          "Symptomatic" = "symptomatic_proxy"
        ),
        selected = "symptomatic_proxy",
        inline = TRUE
      )
    )
  )
}

mod_controls_run_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(
    class = "map-run-controls",
    shiny::conditionalPanel(
      condition = "input.run_mode === 'guided_auto'",
      ns = ns,
      shiny::div(
        class = "map-run-guided",
        shiny::div(
          class = "map-run-field map-run-field-turns",
          shiny::sliderInput(
            ns("auto_turns"),
            label = "Auto turns",
            min = 1,
            max = 50,
            value = 12,
            step = 1,
            ticks = FALSE,
            width = "170px"
          )
        ),
        shiny::div(
          class = "map-run-field map-run-field-speed",
          shiny::selectInput(
            ns("auto_speed"),
            label = "Auto speed",
            choices = c("Slow" = "slow", "Normal" = "normal", "Fast" = "fast"),
            selected = "normal",
            width = "140px"
          )
        )
      ),
      shiny::div(
        class = "map-run-actions",
        shiny::actionButton(ns("start_run"), "Start run", class = "btn-primary btn-sm"),
        shiny::actionButton(ns("pause_run"), "Pause", class = "btn-outline-secondary btn-sm"),
        shiny::actionButton(ns("run_batch"), "Run batch", class = "btn-outline-primary btn-sm"),
        shiny::actionButton(ns("stop_run"), "Stop", class = "btn-outline-danger btn-sm")
      )
    )
  )
}

mod_controls_ui <- function(id) {

  ns <- shiny::NS(id)
  bslib::card(
    style = "height: 100%; overflow-y: auto;",
    bslib::card_header("Admin Controls"),
    shiny::uiOutput(ns("admin_controls")),
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
      mode <- input$testing_pool
      if (is.null(mode)) {
        mode <- "random"
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

      seed_selected <- seed_choice_from_value(shiny::isolate(shared_admin$seed_borough))
      scenario_selected <- shiny::isolate(shared_admin$scenario_id)
      if (is.null(scenario_selected) || is.na(scenario_selected) || scenario_selected == "") {
        scenario_selected <- "custom"
      }
      if (!(scenario_selected %in% scenario_defs$id)) {
        scenario_selected <- "custom"
      }

      shiny::tagList(
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

    # Force a visible default on first render for the shared map control.
    session$onFlushed(function() {
      shiny::updateRadioButtons(
        session,
        "testing_pool",
        selected = "random"
      )
    }, once = TRUE)

    reseed <- shiny::eventReactive(input$reseed, {
      seed_value <- suppressWarnings(as.integer(input$rng_seed)[1])
      if (length(seed_value) != 1L || is.na(seed_value) || seed_value < 1L) {
        seed_value <- 2024L
      }

      if (isTRUE(input$random_seed)) {
        sample.int(1e6, 1)
      } else {
        seed_value
      }
    }, ignoreNULL = FALSE)

    run_mode <- shiny::reactive({
      mode <- input$run_mode
      if (is.null(mode) || !mode %in% c("guided_auto", "manual")) {
        return("guided_auto")
      }
      mode
    })

    autorun <- shiny::reactive({
      turns <- suppressWarnings(as.integer(input$auto_turns))
      if (is.na(turns)) {
        turns <- 12L
      }
      turns <- max(1L, min(50L, turns))

      speed <- input$auto_speed
      if (is.null(speed) || !speed %in% c("slow", "normal", "fast")) {
        speed <- "normal"
      }
      speed_lookup <- c(slow = 1200L, normal = 700L, fast = 250L)

      policy_id <- input$allocation_policy
      if (is.null(policy_id) || !policy_id %in% c("manual", "random_population", "symptomatic_proxy")) {
        policy_id <- "symptomatic_proxy"
      }

      list(
        turns = turns,
        speed = speed,
        speed_ms = as.integer(speed_lookup[[speed]]),
        policy_id = policy_id
      )
    })

    baseline_mode <- shiny::reactive("no_action")

    list(
      epidemiology = params_epidemic,
      mortality = params_mortality,
      testing = params_testing,
      budget = budget_info,
      starter_intel = starter_intel,
      run_mode = run_mode,
      autorun = autorun,
      baseline_mode = baseline_mode,
      start_run = shiny::reactive(input$start_run),
      pause_run = shiny::reactive(input$pause_run),
      run_batch = shiny::reactive(input$run_batch),
      stop_run = shiny::reactive(input$stop_run),
      end = shiny::reactive(input$end),
      reset = shiny::reactive(input$reset),
      reseed = reseed
    )
  })
}
