library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tibble)
library(EpiEstim)
library(shinymanager)

options(shiny.usews = TRUE)

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
  title = "Outbreaks in London: simulation",
  tags$head(
    tags$style(HTML("
      .main-header { margin-bottom: 1rem; }
      .top-bar {
        background: #ffffff; border-bottom: 1px solid #dee2e6;
        padding: 0.5rem 0.75rem; margin: 0 -0.75rem 0.75rem -0.75rem; z-index: 1000;
      }
      .top-bar h4 { margin: 0; color: #3e3f3a; }
      .top-stat { font-size: 0.9rem; line-height: 1.2; }
      .top-stat .label { display: block; font-size: 0.7rem; color: #6c757d; text-transform: uppercase; }
      .top-stat .value { font-weight: 600; }
      /* no basemap tiles (the old tile service now needs an API key):
         boroughs are drawn on a plain light background */
      .leaflet-container { background: #eef1f4; }
      .main-header h3 { margin: 0; color: #5a5a5a; }
      .header-actions { display: flex; align-items: center; gap: 0.5rem; }
      .help-button { white-space: nowrap; }
      .turn-badge { font-size: 1.1rem; font-weight: 600; }
      .scenario-label { font-size: 0.85rem; color: #6c757d; }
      #map-map {
        height: 55vh !important;
        min-height: 320px;
      }
      .bslib-sidebar-toggle,
      .bslib-sidebar-toggle-btn,
      .bslib-sidebar-trigger {
        display: none !important;
      }
      .mfb-component--br,
      .mfb-component--br * {
        display: none !important;
      }
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
    ,
    tags$script(HTML("(
      function() {
        var key = 'epilondon_help_seen';
        function navType() {
          var entries = performance.getEntriesByType && performance.getEntriesByType('navigation');
          if (entries && entries.length && entries[0].type) {
            return entries[0].type;
          }
          if (performance.navigation && typeof performance.navigation.type !== 'undefined') {
            return performance.navigation.type === 1 ? 'reload' : 'navigate';
          }
          return 'navigate';
        }
        var type = navType();
        var seen = false;
        try { seen = localStorage.getItem(key) === '1'; } catch (e) { seen = false; }
        var shouldShow = !seen;
        if (shouldShow) {
          try { localStorage.setItem(key, '1'); } catch (e) {}
        }
        var send = function() {
          if (!shouldShow) {
            return false;
          }
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('help_auto', Date.now(), {priority: 'event'});
            return true;
          }
          return false;
        };
        var tries = 0;
        var retry = function() {
          tries += 1;
          if (send()) {
            return;
          }
          if (tries < 40) {
            setTimeout(retry, 250);
          }
        };
        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', retry);
        } else {
          retry();
        }
        document.addEventListener('shiny:connected', retry);
      }
    )();"))
  ),
  # top bar: always visible, holds where you are and the main buttons
  div(
    class = "top-bar sticky-top d-flex flex-wrap align-items-center gap-3",
    div(
      h4("Outbreaks in London"),
      div(class = "scenario-label", textOutput("scenario_display", inline = TRUE))
    ),
    div(class = "top-stat", span(class = "label", "Turn"), span(class = "value", textOutput("turn_display", inline = TRUE))),
    div(class = "top-stat", span(class = "label", "Money"), span(class = "value", textOutput("money_display", inline = TRUE))),
    div(class = "top-stat", span(class = "label", "Testing and hospitals"), span(class = "value", textOutput("system_display", inline = TRUE))),
    div(
      class = "ms-auto d-flex flex-wrap gap-2",
      actionButton("advance_turn", "Advance turn", class = "btn-primary"),
      actionButton("controls-end", "End & reveal", class = "btn-success"),
      actionButton("controls-reseed", "New outbreak", class = "btn-outline-secondary btn-sm"),
      actionButton("controls-reset", "Replay this outbreak", class = "btn-outline-secondary btn-sm"),
      actionButton("help", "Help", class = "btn-outline-secondary btn-sm help-button")
    )
  ),
  uiOutput("main_layout"),
  div(
    class = "text-muted small mt-2",
    "Data: GLA/ONS borough boundaries."
  )
)

ui <- if (auth_disabled) app_ui else secure_app(app_ui, enable_admin = FALSE)

server <- function(input, output, session) {
  auth <- NULL
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

  help_shown <- reactiveVal(FALSE)
  help_pending <- reactiveVal(FALSE)

  get_auth_user <- function(auth) {
    if (is.null(auth)) {
      return(NULL)
    }
    user <- auth$user
    if (is.function(user)) {
      user <- user()
    }
    user
  }

  auth_ready <- reactive({
    if (auth_disabled) {
      return(TRUE)
    }
    user <- get_auth_user(auth)
    !(is.null(user) || is.na(user) || identical(user, ""))
  })

  show_help_modal <- function() {
    scenario_id <- isolate(shared_admin$scenario_id)
    showModal(modalDialog(
      title = "Welcome and guidance",
      help_modal_body(scenario_id = scenario_id, show_scenario = isolate(is_admin())),
      footer = modalButton("Close"),
      size = "l",
      easyClose = TRUE
    ))
  }

  show_help_once <- function() {
    if (isTRUE(help_shown())) {
      return()
    }
    show_help_modal()
    help_shown(TRUE)
    help_pending(FALSE)
  }

  observeEvent(list(input$help_auto, auth_ready()), {
    if (is.null(input$help_auto)) {
      return()
    }
    if (isTRUE(auth_ready())) {
      show_help_once()
    } else {
      help_pending(TRUE)
    }
  }, ignoreInit = FALSE)

  observeEvent(auth_ready(), {
    if (isTRUE(auth_ready()) && isTRUE(help_pending())) {
      show_help_once()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$help, {
    show_help_modal()
    help_shown(TRUE)
    help_pending(FALSE)
  })

  output$main_layout <- renderUI({
    # map and plan on the left, results on the right; stacked on narrow screens
    main <- bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = c(12, 12), lg = c(7, 5)),
      fill = FALSE,
      mod_map_ui("map", intervention_defs = intervention_defs),
      mod_dashboard_ui("dashboard")
    )

    # only admins get the sidebar (seed and scenario settings)
    if (is_admin()) {
      bslib::layout_sidebar(
        main,
        sidebar = bslib::sidebar(mod_controls_ui("controls"), width = "300px", open = "desktop"),
        fill = FALSE
      )
    } else {
      main
    }
  })

  turn <- reactiveVal(0L)
  total_spend <- reactiveVal(0)
  spend_current <- reactiveVal(list(total = 0, last_turn = 0, remaining = 0, within_budget = TRUE, interventions = "None"))
  app_ready <- reactiveVal(FALSE)

  output$turn_display <- renderText({
    paste(turn(), "of", teaching_params()$game_turns)
  })

  # money left this turn (after the current plan) and in total
  output$money_display <- renderText({
    req(game_state())
    budget <- turn_budget_info()
    planned <- allocations_module$planned_total()
    paste0(
      "£", format(round(budget$budget - planned), big.mark = ",", scientific = FALSE), " left this turn · £",
      format(round(budget$pot_left), big.mark = ",", scientific = FALSE), " left in total"
    )
  })

  # what testing, hospitals and labs publish
  output$system_display <- renderText({
    req(game_state())
    budget <- turn_budget_info()
    view <- player_view(game_state(), current_config())
    paste0(
      budget$testing$label, if (isTRUE(budget$testing$building)) " (upgrading)" else "",
      " · beds ", round(100 * view$bed_pressure), "% full",
      " · lab backlog ", format(round(view$lab_backlog), big.mark = ",", scientific = FALSE)
    )
  })

  output$scenario_display <- renderText({
    # students should work out what kind of outbreak this is from the data
    if (!isTRUE(is_admin())) {
      return("")
    }
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

  # the whole game (hidden truth, test queues, money, levels) lives in one
  # engine state; see R/game_engine.R
  game_state <- reactiveVal(NULL)

  # the seed of the current outbreak, so it can be replayed exactly
  game_seed <- reactiveVal(NULL)

  # what happened last turn, shown on the dashboard (instead of pop-ups)
  turn_log <- reactiveVal(character())

  # hidden testing biases for this game (see teaching_params())
  game_biases <- reactiveVal(NULL)

  # where the starter alert pointed, and where the outbreak really started
  starter_alert_info <- reactiveVal(NULL)

  # settings for the current game, from the shared admin values and the
  # chosen scenario's disease biology
  current_config <- reactive({
    scenario <- scenario_lookup(shared_admin$scenario_id)

    seed_cases <- as.integer(shared_admin$seed_cases)
    if (length(seed_cases) != 1L || is.na(seed_cases) || seed_cases < 0L) {
      seed_cases <- 100L
    }
    seed_borough <- shared_admin$seed_borough
    if (is.null(seed_borough) || is.na(seed_borough) ||
        seed_borough < 1L || seed_borough > nrow(inputs$pop_tbl)) {
      seed_borough <- NA_integer_
    }

    game_config(
      inputs = inputs,
      epi = list(
        Rt = shared_admin$Rt,
        k = shared_admin$k,
        imports_prob = shared_admin$imports_prob,
        lambda_import = shared_admin$lambda_import,
        seed_borough = seed_borough,
        seed_cases = seed_cases
      ),
      mort = list(
        death_ifr = shared_admin$death_ifr,
        death_delay = shared_admin$death_delay
      ),
      kernel_key = shared_admin$kernel_key,
      susceptible_frac = shared_admin$susceptible_frac,
      test_level = scenario$test_level[[1]],
      bio = list(
        asymptomatic = scenario$asymptomatic[[1]],
        presymptomatic = scenario$presymptomatic[[1]],
        delay_decay = scenario$delay_decay[[1]],
        hosp_frac = scenario$hosp_frac[[1]]
      )
    )
  })

  # copy the engine state into the reactives the modules and logs read
  sync_from_state <- function(state) {
    game_state(state)
    truth_state(state$truth)
    truth_history(state$truth$history)
    observation_state(state$obs)
    available_now(state$available)
    game_biases(state$biases)
    total_spend(state$spend_total)
    turn(state$turn)
  }

  # money, costs and testing level for this turn (what the map needs)
  turn_budget_info <- reactive({
    state <- game_state()
    req(state)
    config <- current_config()
    view <- player_view(state, config)
    list(
      budget = view$budget,
      pot_left = view$budget_left,
      costs = view$costs,
      local_costs = view$local_costs,
      tracing_cost = view$tracing_cost,
      testing = view$testing,
      allowance = view$allowance,
      streaks = view$streaks,
      local_streaks = view$local_streaks,
      pool_size = view$pool_size
    )
  })

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
    budget_info = turn_budget_info,
    observed_this_turn = reactive(available_now()),
    observed_history = reactive(observation_state()$history),
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
    reports = reactive({
      state <- game_state()
      if (is.null(state)) NULL else state$reports
    }),
    turn_log = turn_log,
    system_status = reactive({
      state <- game_state()
      if (is.null(state)) return(NULL)
      view <- player_view(state, current_config())
      list(bed_pressure = view$bed_pressure, lab_backlog = view$lab_backlog)
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
    config <- current_config()

    starter_option <- controls$starter_intel()
    state <- new_game(
      config,
      seed = seed,
      starter = starter_option,
      sampling_mode = "symptomatic"
    )
    init <- state$truth
    starter_alloc <- state$starter_alloc
    game_seed(state$seed)

    # first log entry: the early alert
    alerted <- which(starter_alloc$n_ag + starter_alloc$n_pcr > 0)
    turn_log(if (length(alerted) == 1) {
      sprintf(
        "An early alert sent %d free tests to %s. Results come back as they are processed.",
        sum(starter_alloc$n_ag + starter_alloc$n_pcr), inputs$pop_tbl$borough_name[alerted]
      )
    } else {
      "New outbreak. No alerts yet."
    })

    # remember where the alert pointed, for the reveal
    starter_alert_info(NULL)
    if (!is.null(state$alert)) {
      starter_alert_info(list(
        alerted = inputs$pop_tbl$borough_name[state$alert$alerted],
        true_start = inputs$pop_tbl$borough_name[state$alert$true_start]
      ))
    }

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

    sync_from_state(state)
    current_available <- state$available
    params_epi <- config$epi
    pot <- config$tp$budget_total

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

    starter_spend_stats <- summarise_budget(starter_alloc, list(pcr = 0, ag = 0), pot)
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
        budget_remaining = pot,
        interventions_active = "None"
      )
    )

    turn_logs$observations <- append_observation_log(
      observation_log = turn_logs$observations,
      available_results = current_available,
      turn_reported = 0L,
      pop_tbl = inputs$pop_tbl
    )

    spend_current(list(
      total = 0,
      last_turn = 0,
      remaining = pot,
      within_budget = TRUE,
      interventions = "None"
    ))
    allocations_module$reset()
    allocations_module$clear_interventions(fair_share = player_view(state, config)$allowance)

    starter_note <- starter_intel_label(starter_option)
    starter_queue <- sum(state$obs$queue$n)
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
      reported_deaths = 0,
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

  observeEvent(controls$reseed(), {
    already_initialised <- !is.null(isolate(game_state()))
    if (!already_initialised) {
      app_ready(FALSE)
    }
    initialise_truth(seed = controls$reseed())
    if (!already_initialised) {
      app_ready(TRUE)
    }
    if (already_initialised) {
      showNotification("New outbreak started", type = "message")
    }
  }, ignoreNULL = FALSE)

  # replay: the same hidden outbreak again, to try a different strategy
  observeEvent(controls$reset(), {
    initialise_truth(seed = game_seed())
    showNotification("Replaying the same outbreak", type = "message")
  })

  observeEvent(input$advance_turn, {
    state <- game_state()
    req(state)

    # the game lasts a fixed number of turns
    if (state$turn >= teaching_params()$game_turns) {
      showNotification("That was the last turn. Press End & reveal to see what really happened.", type = "message")
      return()
    }
    config <- current_config()
    current_turn <- state$turn

    # everything the player chose this turn
    actions <- list(
      allocations = allocations_module$allocations(),
      sampling_mode = allocations_module$who(),
      interventions = allocations_module$interventions(),
      local_restrictions = allocations_module$restricted(),
      tracing = allocations_module$tracing(),
      build_testing = allocations_module$build_testing()
    )

    result <- play_turn(state, actions, config)
    if (!result$ok) {
      showNotification("Adjust allocations/interventions to meet budget", type = "error")
      return()
    }

    new_state <- result$state
    sync_from_state(new_state)
    allocs <- result$allocations
    obs <- list(available = result$available, queue = result$queue)

    # one line describing every action taken this turn, for the logs/replay
    action_parts <- character()
    if (!identical(result$intervention_label, "None")) {
      action_parts <- c(action_parts, result$intervention_label)
    }
    if (length(result$restricted) > 0) {
      action_parts <- c(action_parts, paste0(
        "Restricted: ", paste(inputs$pop_tbl$borough_name[result$restricted], collapse = ", ")
      ))
    }
    if (isTRUE(actions$tracing)) {
      action_parts <- c(action_parts, "Contact tracing")
    }
    if (result$build_cost > 0) {
      action_parts <- c(action_parts, "Started building testing")
    }
    intervention_label <- if (length(action_parts) == 0) "None" else paste(action_parts, collapse = "; ")

    other_cost <- result$intervention_cost + result$local_cost + result$tracing_cost + result$build_cost
    pot_left <- config$tp$budget_total - new_state$spend_total

    spend_current(list(
      total = new_state$spend_total,
      last_turn = result$turn_spend,
      remaining = pot_left,
      within_budget = TRUE,
      interventions = intervention_label
    ))

    turn_logs$actions <- append_action_log(
      action_log = turn_logs$actions,
      turn = current_turn,
      allocations = allocs,
      pop_tbl = inputs$pop_tbl,
      costs = test_setup(state, config)$costs,
      meta = list(
        allocation_spend = result$allocation_spend,
        intervention_cost = other_cost,
        total_turn_spend = result$turn_spend,
        cumulative_spend = new_state$spend_total,
        budget_remaining = pot_left,
        interventions_active = intervention_label
      )
    )

    turn_logs$observations <- append_observation_log(
      observation_log = turn_logs$observations,
      available_results = obs$available,
      turn_reported = current_turn,
      pop_tbl = inputs$pop_tbl
    )

    next_truth <- new_state$truth
    turn_logs$truth <- append_truth_log(
      truth_log = turn_logs$truth,
      turn = next_truth$turn,
      incidence = next_truth$incidence,
      cumulative = next_truth$cumulative,
      deaths = next_truth$deaths,
      cumulative_deaths = next_truth$cumulative_deaths,
      imports = next_truth$last_imports,
      Rt = result$Rt_used,
      pop_tbl = inputs$pop_tbl
    )

    entry_cur <- tibble(
      turn = current_turn,
      tests_ag = sum(allocs$n_ag),
      tests_pcr = sum(allocs$n_pcr),
      allocation_spend = result$allocation_spend,
      intervention_cost = other_cost,
      total_turn_spend = result$turn_spend,
      cumulative_spend = new_state$spend_total,
      observed_tests = sum(obs$available$n),
      observed_positives = sum(obs$available$positives),
      reported_deaths = sum(new_state$reports$deaths),
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

    allocations_module$reset()

    # what happened this turn, kept on the dashboard until the next turn
    n_results <- sum(obs$available$n)
    n_positives <- sum(obs$available$positives)
    n_pending <- sum(obs$queue$n)
    new_reports <- new_state$reports[new_state$reports$turn == new_state$turn, ]
    log <- c(
      if (n_results > 0) {
        sprintf("%s test results came back: %s positive (%.1f%%).",
                format(n_results, big.mark = ","), format(n_positives, big.mark = ","),
                100 * n_positives / n_results)
      } else {
        "No test results came back this turn."
      },
      if (n_pending > 0) sprintf("%s tests still waiting for results.", format(n_pending, big.mark = ",")),
      sprintf("Hospitals reported %s new admissions and %s deaths.",
              format(sum(new_reports$admissions), big.mark = ","), format(sum(new_reports$deaths), big.mark = ","))
    )

    # tell the player if fewer tests ran than they asked for
    asked <- sum(actions$allocations$n_ag + actions$allocations$n_pcr)
    ran <- sum(allocs$n_ag + allocs$n_pcr)
    if (ran < asked) {
      log <- c(log, sprintf(
        "Only %s of the %s planned tests ran: not enough people came forward, or there were not enough kits.",
        format(ran, big.mark = ","), format(asked, big.mark = ",")
      ))
    }
    if (result$pcr_extra_delay > 0) {
      log <- c(log, sprintf("Labs are behind: this turn's PCR results will be %d turn(s) late.", result$pcr_extra_delay))
    }
    if (new_state$turn >= teaching_params()$game_turns) {
      log <- c(log, "That was the last turn. Press End & reveal.")
    }
    turn_log(c(sprintf("Turn %d:", current_turn), log))
  })

  # Debrief card: the hidden things that were distorting what students saw
  hidden_distortions_ui <- function() {
    biases <- game_biases()
    if (is.null(biases)) {
      return(NULL)
    }

    care <- tibble(
      borough = inputs$pop_tbl$borough_name,
      care_seeking = biases$care_seeking
    ) |>
      arrange(care_seeking)
    least <- paste(head(care$borough, 3), collapse = ", ")
    most <- paste(rev(tail(care$borough, 3)), collapse = ", ")

    alert <- starter_alert_info()
    alert_txt <- if (is.null(alert)) {
      "No starter alert this game."
    } else if (identical(alert$alerted, alert$true_start)) {
      sprintf("The starter alert was right: the outbreak began in %s.", alert$true_start)
    } else {
      sprintf(
        "The starter alert was a false alarm: it pointed at %s, but the outbreak began in %s.",
        alert$alerted, alert$true_start
      )
    }

    # hospitals: how long they were over capacity and what that cost
    hospital <- game_state()$hospital
    beds <- teaching_params()$hospital$beds
    turns_full <- sum(hospital$occupancy > beds)
    hospital_txt <- tags$p(
      tags$strong("Hospitals: "),
      if (turns_full == 0) {
        sprintf("never ran out of beds (peak %d%% full).", round(100 * max(c(0, hospital$occupancy)) / beds))
      } else {
        sprintf(
          "ran out of beds for %d turn(s); about %s extra people died because there was no bed for them.",
          turns_full, format(sum(hospital$extra_deaths), big.mark = ",", scientific = FALSE)
        )
      }
    )

    tp <- teaching_params()
    tags$div(
      class = "card border-info mb-3",
      tags$div(class = "card-header", tags$strong("What was distorting your data")),
      tags$div(
        class = "card-body small",
        tags$p(alert_txt),
        tags$p(
          tags$strong("Who comes forward for a test (symptomatic pool): "),
          "least likely in ", least, "; most likely in ", most, ". ",
          "Infections there were under- or over-represented in your results."
        ),
        tags$p(
          tags$strong("Other respiratory illness: "),
          sprintf(
            "a wave of non-target illness peaked around turn %d, swelling the symptomatic pool and pulling positivity down even if cases were rising.",
            biases$wave_peak
          )
        ),
        hospital_txt,
        tags$p(
          tags$strong("Antigen false positives: "),
          sprintf(
            "antigen specificity was %.1f%%, so about %d in every 1,000 uninfected people tested came back positive.",
            100 * tp$tests$spec$ag, round(1000 * (1 - tp$tests$spec$ag))
          )
        ),
        tags$p(
          class = "text-muted",
          "Care-seeking and the illness wave only affected symptomatic testing; random testing was unbiased but found fewer cases."
        )
      )
    )
  }

  observeEvent(controls$end(), {
    req(truth_state())
    report <- build_run_report(
      truth_history = truth_history(),
      observed_history = observation_state()$history,
      pop_tbl = inputs$pop_tbl,
      total_spend = total_spend(),
      run_log = run_log(),
      truth_log = turn_logs$truth
    )

    run_report(report)
    replay_snapshot(logs_snapshot())
    replay_unlocked(TRUE)

    metrics <- report$metrics

    # real cases found by the player's own tests (not the starter alert, not
    # false positives)
    found_pct <- if (is.na(metrics$share_of_infections_found)) NA else round(100 * metrics$share_of_infections_found, 1)
    false_pct <- if (is.na(metrics$false_positives) || metrics$own_positives == 0) NA else
      round(100 * metrics$false_positives / metrics$own_positives)

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
        "True Data Revealed"
      ),
      fluidRow(
        column(6,
          tags$div(
            class = "card bg-light mb-3",
            tags$div(class = "card-header", tags$strong("Your Results")),
            tags$div(
              class = "card-body",
              tags$p(
                tags$strong("First real case found by your testing: "),
                ifelse(is.na(metrics$time_to_detection), "never", paste("turn", metrics$time_to_detection))
              ),
              tags$p(
                tags$strong("Peak observed: "),
                metrics$peak_observed_positives, " positives"
              ),
              tags$p(
                tags$strong("Total spend: "),
                paste0("£", format(round(metrics$total_spend, 0), big.mark = ",", scientific = FALSE))
              ),
              tags$p(
                tags$strong("Real cases your tests found: "),
                format(metrics$real_cases_found, big.mark = ",", scientific = FALSE),
                ifelse(is.na(found_pct), "", paste0(" (", found_pct, "% of all infections)"))
              ),
              tags$p(
                tags$strong("False positives: "),
                ifelse(
                  is.na(false_pct), "none",
                  paste0(format(metrics$false_positives, big.mark = ",", scientific = FALSE),
                         " of your ", format(metrics$own_positives, big.mark = ",", scientific = FALSE),
                         " positives (", false_pct, "%)")
                )
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
            tags$div(class = "card-header", tags$strong("True Data")),
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
      hidden_distortions_ui(),
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
    replay_snapshot(logs_snapshot())
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
