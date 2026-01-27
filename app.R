library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tibble)
library(EpiEstim)
library(shinymanager)

quiet_load_support <- function(app_dir = ".") {
  previous <- getOption("shiny.autoload.r")
  on.exit(options(shiny.autoload.r = previous), add = TRUE)

  options(shiny.autoload.r = TRUE)
  withCallingHandlers(
    shiny::loadSupport(appDir = app_dir),
    warning = function(w) {
      msg <- conditionMessage(w)
      if (grepl("Loading R/ subdirectory", msg, fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

quiet_load_support()

if (!exists("load_inputs")) {
  r_dir <- "R"
  if (dir.exists(r_dir)) {
    r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
    for (file in r_files) {
      source(file, local = FALSE)
    }
  }
}

inputs <- load_inputs()

auth_disabled <- tolower(Sys.getenv("EPILONDON_AUTH_DISABLE", "false")) %in%
  c("true", "1", "yes")

auth_credentials_path <- Sys.getenv(
  "EPILONDON_AUTH_FILE",
  file.path("config", "credentials.csv")
)

load_credentials <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf(
      "Auth credentials file not found at '%s'. Create it before running the app.",
      path
    ))
  }

  creds <- utils::read.csv(path, stringsAsFactors = FALSE)
  required <- c("user", "password")
  missing <- setdiff(required, names(creds))
  if (length(missing) > 0) {
    stop(sprintf(
      "Auth credentials missing required columns: %s",
      paste(missing, collapse = ", ")
    ))
  }

  if (!"admin" %in% names(creds)) {
    creds$admin <- FALSE
  }
  creds$admin <- as.logical(creds$admin)
  creds
}

credentials <- NULL
if (!auth_disabled) {
  credentials <- load_credentials(auth_credentials_path)
}

get_auth_user_info <- function(auth) {
  info <- auth$user_info
  if (is.function(info)) {
    info <- info()
  }
  if (is.null(info)) {
    return(NULL)
  }
  if (is.data.frame(info) && nrow(info) < 1L) {
    return(NULL)
  }
  info
}

get_admin_users <- function(creds) {
  if (is.null(creds) || !("user" %in% names(creds))) {
    return(character())
  }
  admins <- rep(FALSE, nrow(creds))
  if ("admin" %in% names(creds)) {
    admins <- as.logical(creds$admin)
    admins[is.na(admins)] <- FALSE
  }
  creds$user[admins]
}

admin_users <- get_admin_users(credentials)

shared_admin <- reactiveValues(
  Rt = 1.3,
  k = 0.5,
  imports_prob = 0.01,
  lambda_import = 0.5,
  seed_borough = NA_integer_,
  seed_cases = 100L,
  kernel_key = "default",
  susceptible_frac = 1,
  scenario_id = "custom",
  death_ifr = 0.002,
  death_delay = 6L
)

shared_reset_signal <- reactiveVal(0L)

empty_results <- tibble(
  borough_index = integer(),
  borough_name = character(),
  type = character(),
  n = integer(),
  positives = integer(),
  turn_sampled = integer(),
  turn_available = integer()
)

allocations_template <- tibble(
  borough_index = seq_len(nrow(inputs$pop_tbl)),
  borough_name = inputs$pop_tbl$borough_name,
  n_ag = integer(nrow(inputs$pop_tbl)),
  n_pcr = integer(nrow(inputs$pop_tbl))
)

intervention_defs <- intervention_catalog()

app_ui <- page_fluid(
  theme = bs_theme(bootswatch = "sandstone"),
  title = "Outbreak London",
  tags$head(
    tags$style(HTML("
      .main-header { margin-bottom: 1rem; }
      .main-header h3 { margin: 0; color: #5a5a5a; }
      .turn-badge { font-size: 1.1rem; font-weight: 600; }
      .scenario-label { font-size: 0.85rem; color: #6c757d; }
      #controls-rng_seed { width: 100%; min-width: 75px; font-size: 11px; }
      .controls-seed-block {
        margin-bottom: 12px;
        width: 100%;
        height: fit-content;
      }
      .controls-seed-block .checkbox {
        vertical-align: middle;
        text-align: left;
        margin-bottom: 0;
        height: fit-content;
      }
      .controls-seed-block .checkbox label {
        overflow: visible;
        height: fit-content;
        text-align: center;
        vertical-align: middle;
      }
      .controls-seed-block .checkbox span {
        font-size: 14px;
        color: rgba(62, 63, 58, 1);
      }
    "))
  ),
  div(
    class = "main-header d-flex justify-content-between align-items-center",
    div(
      h3("Outbreak London"),
      div(class = "scenario-label", textOutput("scenario_display"))
    ),
    div(
      class = "turn-badge badge bg-secondary",
      textOutput("turn_display", inline = TRUE)
    )
  ),
  uiOutput("main_layout"),
  div(
    class = "text-muted small mt-2",
    "Data: GLA/ONS borough boundaries. Click boroughs to allocate tests, then advance turns."
  )
)

ui <- if (auth_disabled) app_ui else secure_app(app_ui, enable_admin = FALSE)

server <- function(input, output, session) {
  if (auth_disabled) {
    is_admin <- reactive(FALSE)
  } else {
    auth <- secure_server(check_credentials = check_credentials(credentials))
    is_admin <- reactive({
      user <- auth$user
      if (is.function(user)) {
        user <- user()
      }
      if (is.null(user) || is.na(user) || identical(user, "")) {
        return(FALSE)
      }
      user %in% admin_users
    })
  }

  output$main_layout <- renderUI({
    if (is_admin()) {
      bslib::layout_sidebar(
        bslib::layout_columns(
          col_widths = c(8, 4),
          fill = FALSE,
          mod_map_ui("map", intervention_defs = intervention_defs),
          mod_dashboard_ui("dashboard")
        ),
        sidebar = bslib::sidebar(
          mod_controls_ui("controls"),
          width = "320px",
          open = "desktop"
        ),
        fill = FALSE
      )
    } else {
      bslib::layout_sidebar(
        bslib::layout_columns(
          col_widths = c(7, 5),
          fill = FALSE,
          mod_map_ui("map", intervention_defs = intervention_defs),
          mod_dashboard_ui("dashboard")
        ),
        sidebar = bslib::sidebar(
          mod_controls_ui("controls"),
          width = "280px",
          open = "desktop"
        ),
        fill = FALSE
      )
    }
  })

  turn <- reactiveVal(0L)
  total_spend <- reactiveVal(0)
  spend_current <- reactiveVal(list(total = 0, last_turn = 0, remaining = 0, within_budget = TRUE, interventions = "None"))
  app_ready <- reactiveVal(FALSE)

  active_kernel <- reactive({
    key <- shared_admin$kernel_key
    if (is.null(key) || key == "" || identical(key, "default")) {
      return(inputs$K)
    }
    if (identical(key, "measles") && !is.null(inputs$K_measles)) {
      return(inputs$K_measles)
    }
    inputs$K
  })

  effective_pop_tbl <- reactive({
    frac <- shared_admin$susceptible_frac
    if (is.null(frac) || !is.finite(frac)) {
      frac <- 1
    }
    frac <- max(0, frac)
    pop_tbl <- inputs$pop_tbl
    pop_tbl$pop <- pmax(pop_tbl$pop * frac, 1)
    pop_tbl
  })

  output$turn_display <- renderText({
    paste("Turn", turn())
  })

  output$scenario_display <- renderText({
    scenario_id <- shared_admin$scenario_id
    if (is.null(scenario_id) || is.na(scenario_id) || scenario_id == "custom") {
      return("")
    }
    scenario <- scenario_lookup(scenario_id)
    label <- scenario$label[[1]]
    if (is.null(label) || identical(label, "")) {
      return("")
    }
    paste("Scenario:", label)
  })

  run_log <- reactiveVal(tibble())
  run_report <- reactiveVal(NULL)

  observation_state <- reactiveVal(init_observation_state())
  available_now <- reactiveVal(empty_results)

  turn_logs <- reactiveValues(
    truth = tibble(),
    observations = tibble(),
    actions = tibble()
  )
  replay_unlocked <- reactiveVal(FALSE)
  empty_logs <- init_turn_logs(inputs$pop_tbl)
  replay_snapshot <- reactiveVal(list(
    truth = empty_logs$truth,
    observations = empty_logs$observations,
    actions = empty_logs$actions,
    truth_history = list()
  ))
  replay_open_id <- reactiveVal(0L)

  truth_state <- reactiveVal(NULL)
  truth_history <- reactiveVal(list())

  controls <- mod_controls_server(
    id = "controls",
    is_admin = is_admin,
    shared_admin = shared_admin,
    reset_signal = shared_reset_signal,
    borough_names = inputs$pop_tbl$borough_name
  )

  allocations_module <- mod_map_server(
    id = "map",
    boros = inputs$boros,
    allocations_init = allocations_template,
    budget_info = controls$budget,
    observed_this_turn = reactive(available_now()),
    pop_tbl = inputs$pop_tbl,
    app_ready = app_ready,
    intervention_defs = intervention_defs
  )

  dashboard_module <- mod_dashboard_server(
    id = "dashboard",
    observed_history = reactive(observation_state()$history),
    queue_state = reactive(observation_state()$queue),
    spend_info = spend_current,
    available_now = reactive(available_now()),
    reported_deaths = reactive({
      rl <- run_log()
      if (is.null(rl) || nrow(rl) == 0 || !"reported_deaths" %in% names(rl)) {
        return(0)
      }
      latest_turn <- max(rl$turn, na.rm = TRUE)
      vals <- rl$reported_deaths[rl$turn == latest_turn]
      if (length(vals) == 0 || all(is.na(vals))) {
        0
      } else {
        vals[[1]]
      }
    }),
    app_ready = app_ready
  )

  logs_snapshot <- reactive({
    list(
      truth = turn_logs$truth,
      observations = turn_logs$observations,
      actions = turn_logs$actions,
      truth_history = truth_history()
    )
  })

  output$run_summary_table <- renderTable({
    report <- run_report()
    req(report)
    report$turn_summary
  }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 2)

  initialise_truth <- function(seed = NULL) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    params_epi <- controls$epidemiology()
    budget <- controls$budget()

    n_boros <- nrow(inputs$pop_tbl)
    seed_borough <- shared_admin$seed_borough
    if (is.null(seed_borough) || is.na(seed_borough) ||
        seed_borough < 1L || seed_borough > n_boros) {
      seed_borough <- sample.int(n_boros, 1L)
    }
    seed_cases <- as.integer(shared_admin$seed_cases)
    if (length(seed_cases) != 1L || is.na(seed_cases) || seed_cases < 0L) {
      seed_cases <- 100L
    }
    init <- init_truth(
      n_boroughs = n_boros,
      seed_boroughs = seed_borough,
      seed_cases = seed_cases
    )

    blank_logs <- init_turn_logs(inputs$pop_tbl)
    turn_logs$truth <- blank_logs$truth
    turn_logs$observations <- blank_logs$observations
    turn_logs$actions <- blank_logs$actions
    replay_unlocked(FALSE)
    replay_snapshot(list(
      truth = blank_logs$truth,
      observations = blank_logs$observations,
      actions = blank_logs$actions,
      truth_history = list()
    ))

    truth_state(init)
    truth_history(init$history)
    base_observation <- init_observation_state()
    starter_option <- controls$starter_intel()
    testing_params <- controls$testing()
    starter_alloc <- starter_intel_allocations(starter_option, truth_state = init, pop_tbl = inputs$pop_tbl)

    starter_obs <- NULL
    if (sum(starter_alloc$n_ag) + sum(starter_alloc$n_pcr) > 0) {
      starter_obs <- observe_tests(
        state = base_observation,
        turn = 0,
        allocations = starter_alloc,
        truth_incidence = init$incidence,
        pop_tbl = effective_pop_tbl(),
        params = testing_params
      )
      base_observation <- list(queue = starter_obs$queue, history = starter_obs$history)
      current_available <- starter_obs$available
    } else {
      current_available <- empty_results
    }

    turn_logs$truth <- append_truth_log(
      truth_log = turn_logs$truth,
      turn = init$turn,
      incidence = init$incidence,
      cumulative = init$cumulative,
      deaths = init$deaths,
      cumulative_deaths = init$cumulative_deaths,
      imports = integer(length(init$incidence)),
      Rt = params_epi$Rt,
      pop_tbl = inputs$pop_tbl
    )

    starter_spend_stats <- summarise_budget(starter_alloc, list(pcr = 0, ag = 0), budget$budget)
    turn_logs$actions <- append_action_log(
      action_log = turn_logs$actions,
      turn = 0L,
      allocations = starter_alloc,
      pop_tbl = inputs$pop_tbl,
      costs = list(pcr = 0, ag = 0),
      meta = list(
        allocation_spend = starter_spend_stats$spend,
        intervention_cost = 0,
        total_turn_spend = starter_spend_stats$spend,
        cumulative_spend = 0,
        budget_remaining = budget$budget,
        interventions_active = "None"
      )
    )

    turn_logs$observations <- append_observation_log(
      observation_log = turn_logs$observations,
      available_results = current_available,
      turn_reported = 0L,
      pop_tbl = inputs$pop_tbl
    )

    observation_state(base_observation)
    available_now(current_available)
    total_spend(0)
    spend_current(list(
      total = 0,
      last_turn = 0,
      remaining = budget$budget,
      within_budget = TRUE,
      interventions = "None"
    ))
    turn(0L)
    allocations_module$reset()
    allocations_module$clear_interventions()

    starter_note <- starter_intel_label(starter_option)
    starter_queue <- if (!is.null(starter_obs)) sum(starter_obs$queue$n) else 0
    run_log(tibble(
      turn = 0L,
      truth_total = sum(init$incidence),
      tests_ag = sum(starter_alloc$n_ag),
      tests_pcr = sum(starter_alloc$n_pcr),
      allocation_spend = 0,
      intervention_cost = 0,
      total_turn_spend = 0,
      cumulative_spend = 0,
      observed_tests = sum(current_available$n),
      observed_positives = sum(current_available$positives),
      reported_deaths = sum(init$deaths),
      active_interventions = "None",
      notes = starter_note,
      queue_pending = starter_queue
    ))

    run_report(NULL)
  }

  observeEvent(shared_reset_signal(), {
    initialise_truth(seed = controls$reseed())
    showNotification("Admin reset triggered", type = "message")
  }, ignoreInit = TRUE)

  observe({
    # keep spend panel current when budget sliders change between turns
    budget <- controls$budget()
    current_info <- spend_current()
    spend_current(list(
      total = current_info$total,
      last_turn = current_info$last_turn,
      remaining = budget$budget - current_info$last_turn,
      within_budget = current_info$last_turn <= budget$budget + 1e-6,
      interventions = if (is.null(current_info$interventions)) "None" else current_info$interventions
    ))
  })

  observeEvent(controls$reseed(), {
    already_initialised <- !is.null(isolate(truth_state()))
    if (!already_initialised) {
      app_ready(FALSE)
    }
    initialise_truth(seed = controls$reseed())
    if (!already_initialised) {
      app_ready(TRUE)
    }
    if (already_initialised) {
      showNotification("Simulation reseeded", type = "message")
    }
  }, ignoreNULL = FALSE)

  observeEvent(controls$reset(), {
    initialise_truth(seed = controls$reseed())
  })

  observeEvent(allocations_module$advance(), {
    req(truth_state())

    current_turn <- turn()
    allocs <- allocations_module$allocations()
    budget <- controls$budget()
    params_epi <- controls$epidemiology()
    params_mort <- controls$mortality()
    testing_params <- controls$testing()
    active_ids <- allocations_module$interventions()
    effects <- compute_intervention_effects(active_ids, intervention_defs)
    intervention_label <- if (length(effects$labels) == 0) "None" else paste(effects$labels, collapse = ", ")

    params_epi$Rt <- params_epi$Rt * effects$Rt_multiplier
    params_epi$imports_prob <- min(1, max(0, params_epi$imports_prob * effects$imports_multiplier))

    testing_params$delays$ag <- max(0, testing_params$delays$ag + effects$delay_adjust$ag)
    testing_params$delays$pcr <- max(0, testing_params$delays$pcr + effects$delay_adjust$pcr)

    spend_stats <- summarise_budget(allocs, budget$costs, budget$budget)
    intervention_cost <- effects$cost
    turn_spend <- spend_stats$spend + intervention_cost
    remaining_budget <- budget$budget - turn_spend

    if (remaining_budget < -1e-6) {
      showNotification("Adjust allocations/interventions to meet budget", type = "error")
      return()
    }

    new_total_spend <- total_spend() + turn_spend
    total_spend(new_total_spend)
    spend_current(list(
      total = new_total_spend,
      last_turn = turn_spend,
      remaining = remaining_budget,
      within_budget = remaining_budget >= -1e-6,
      interventions = intervention_label
    ))

    turn_logs$actions <- append_action_log(
      action_log = turn_logs$actions,
      turn = current_turn,
      allocations = allocs,
      pop_tbl = inputs$pop_tbl,
      costs = budget$costs,
      meta = list(
        allocation_spend = spend_stats$spend,
        intervention_cost = intervention_cost,
        total_turn_spend = turn_spend,
        cumulative_spend = new_total_spend,
        budget_remaining = remaining_budget,
        interventions_active = intervention_label
      )
    )

    obs <- observe_tests(
      state = observation_state(),
      turn = turn(),
      allocations = allocs,
      truth_incidence = truth_state()$incidence,
      pop_tbl = effective_pop_tbl(),
      params = testing_params
    )

    observation_state(list(queue = obs$queue, history = obs$history))
    available_now(obs$available)

    current_deaths <- 0
    if (!is.null(truth_state()$deaths)) {
      current_deaths <- sum(truth_state()$deaths)
    }

    turn_logs$observations <- append_observation_log(
      observation_log = turn_logs$observations,
      available_results = obs$available,
      turn_reported = current_turn,
      pop_tbl = inputs$pop_tbl
    )

    next_truth <- step_truth(
      state = truth_state(),
      K = active_kernel(),
      Rt_global = params_epi$Rt,
      k_overdisp = params_epi$k,
      imports_prob = params_epi$imports_prob,
      lambda_import = params_epi$lambda_import,
      pop = effective_pop_tbl()$pop,
      death_ifr = params_mort$death_ifr,
      death_delay = params_mort$death_delay
    )

    truth_state(next_truth)
    truth_history(next_truth$history)

    turn_logs$truth <- append_truth_log(
      truth_log = turn_logs$truth,
      turn = next_truth$turn,
      incidence = next_truth$incidence,
      cumulative = next_truth$cumulative,
      deaths = next_truth$deaths,
      cumulative_deaths = next_truth$cumulative_deaths,
      imports = next_truth$last_imports,
      Rt = params_epi$Rt,
      pop_tbl = inputs$pop_tbl
    )

    entry_cur <- tibble(
      turn = current_turn,
      tests_ag = sum(allocs$n_ag),
      tests_pcr = sum(allocs$n_pcr),
      allocation_spend = spend_stats$spend,
      intervention_cost = intervention_cost,
      total_turn_spend = turn_spend,
      cumulative_spend = new_total_spend,
      observed_tests = sum(obs$available$n),
      observed_positives = sum(obs$available$positives),
      reported_deaths = current_deaths,
      active_interventions = intervention_label,
      notes = "",
      queue_pending = sum(obs$queue$n)
    )

    rl <- run_log()
    if (nrow(rl) == 0 || !(current_turn %in% rl$turn)) {
      run_log(dplyr::bind_rows(rl, entry_cur))
    } else {
      rl <- dplyr::rows_update(rl, entry_cur, by = "turn")
      run_log(rl)
    }

    next_turn <- current_turn + 1L
    turn(next_turn)
    allocations_module$reset()

    # Show turn summary notification
    n_results <- sum(obs$available$n)
    n_positives <- sum(obs$available$positives)
    n_pending <- sum(obs$queue$n)

    summary_msg <- if (n_results > 0) {
      sprintf(
        "Turn %d complete: %d results returned (%d positive). %d tests pending.",
        current_turn, n_results, n_positives, n_pending
      )
    } else {
      sprintf(
        "Turn %d complete: No results yet. %d tests pending.",
        current_turn, n_pending
      )
    }

    showNotification(
      summary_msg,
      type = if (n_positives > 0) "warning" else "message",
      duration = 5
    )
  })

  observeEvent(controls$end(), {
    req(truth_state())
    report <- build_run_report(
      truth_history = truth_history(),
      observed_history = observation_state()$history,
      pop_tbl = effective_pop_tbl(),
      total_spend = total_spend(),
      run_log = run_log(),
      truth_log = turn_logs$truth
    )

    run_report(report)
    replay_snapshot(logs_snapshot())
    replay_unlocked(TRUE)

    metrics <- report$metrics

    # Calculate detection coverage
    total_true <- sum(sapply(truth_history(), sum))
    total_observed <- sum(observation_state()$history$positives)
    detection_rate <- if (total_true > 0) round(100 * total_observed / total_true, 1) else NA

    format_rt <- function(mean_rt, lower_rt, upper_rt) {
      if (is.na(mean_rt) || is.na(lower_rt) || is.na(upper_rt)) {
        "Rt unavailable"
      } else {
        sprintf("%.2f (%.2f–%.2f)", mean_rt, lower_rt, upper_rt)
      }
    }

    true_prev_pct <- if (is.na(metrics$true_prevalence_rate)) {
      "N/A"
    } else {
      sprintf("%.2f%%", 100 * metrics$true_prevalence_rate)
    }

    observed_rt_txt <- format_rt(
      metrics$rt_observed_mean,
      metrics$rt_observed_lower,
      metrics$rt_observed_upper
    )
    truth_rt_txt <- format_rt(
      metrics$rt_truth_mean,
      metrics$rt_truth_lower,
      metrics$rt_truth_upper
    )

    showModal(modalDialog(
      title = tags$span(
        tags$i(class = "fa fa-eye me-2"),
        "The Truth Revealed"
      ),
      fluidRow(
        column(6,
          tags$div(
            class = "card bg-light mb-3",
            tags$div(class = "card-header", tags$strong("Your Performance")),
            tags$div(
              class = "card-body",
              tags$p(
                tags$strong("Time to first detection: "),
                ifelse(is.na(metrics$time_to_detection), "Never detected!", paste("Turn", metrics$time_to_detection))
              ),
              tags$p(
                tags$strong("Peak observed: "),
                metrics$peak_observed_positives, " positives"
              ),
              tags$p(
                tags$strong("Total spend: "),
                paste0("£", round(metrics$total_spend, 0))
              ),
              tags$p(
                tags$strong("Detection rate: "),
                ifelse(is.na(detection_rate), "N/A", paste0(detection_rate, "% of true cases"))
              ),
              tags$p(
                tags$strong("Estimated Rt (observed): "),
                observed_rt_txt
              )
            )
          )
        ),
        column(6,
          tags$div(
            class = "card bg-warning-subtle mb-3",
            tags$div(class = "card-header", tags$strong("Hidden Truth")),
            tags$div(
              class = "card-body",
              tags$p(
                tags$strong("Peak true incidence: "),
                metrics$peak_true_incidence, " cases"
              ),
              tags$p(
                tags$strong("Calibration gap: "),
                sprintf("%.3f RMSE", metrics$calibration_gap)
              ),
              tags$p(
                tags$strong("True cumulative prevalence (final turn): "),
                true_prev_pct
              ),
              tags$p(
                tags$strong("Rt from truth incidence: "),
                truth_rt_txt
              ),
              tags$p(
                class = "text-muted small",
                "The calibration gap measures how far your inferred prevalence was from the truth."
              )
            )
          )
        )
      ),
      tags$hr(),
      tags$h5("Turn-by-Turn Comparison"),
      tags$div(
        style = "max-height: 300px; overflow-y: auto;",
        tableOutput("run_summary_table")
      ),
      tags$hr(),
      tags$div(
        class = "text-center",
        actionButton("show_replay", "View Replay", class = "btn-primary btn-lg")
      ),
      footer = modalButton("Close"),
      size = "xl",
      easyClose = TRUE
    ))
  })

  # Handle replay button
  observeEvent(input$show_replay, {
    removeModal()
    showModal(modalDialog(
      title = "Turn-by-Turn Replay",
      mod_replay_ui("replay"),
      footer = modalButton("Close"),
      size = "xl",
      easyClose = TRUE
    ))
    replay_open_id(replay_open_id() + 1L)
  })

  # Initialize replay server (works when modal opens)
  mod_replay_server(
    id = "replay",
    logs = reactive(replay_snapshot()),
    boros = inputs$boros,
    pop_tbl = inputs$pop_tbl,
    ended = reactive(replay_unlocked()),
    open_signal = reactive(replay_open_id())
  )
}

shinyApp(ui, server)
