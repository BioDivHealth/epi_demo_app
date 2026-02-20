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
        var shouldShow = (!seen) || type === 'reload';
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
    div(
      class = "main-header d-flex justify-content-between align-items-center",
      div(
        h3("Outbreaks in London: simulation"),
        div(class = "scenario-label", textOutput("scenario_display"))
      ),
      div(
        class = "header-actions",
        actionButton("help", "Help", class = "btn btn-outline-secondary btn-sm help-button"),
        div(
          class = "turn-badge badge bg-secondary",
          textOutput("turn_display", inline = TRUE)
        )
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
      help_modal_body(scenario_id = scenario_id),
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
  auto_running <- reactiveVal(FALSE)
  auto_turns_requested <- reactiveVal(0L)
  auto_turns_completed <- reactiveVal(0L)
  auto_stop_reason <- reactiveVal(NULL)

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
  dual_report <- reactiveVal(NULL)
  baseline_branch <- reactiveVal(NULL)
  master_seed <- reactiveVal(2024L)

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
    run_mode = controls$run_mode,
    auto_running = auto_running,
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
    comparison_report = reactive(dual_report()),
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
    report_dual <- dual_report()
    if (!is.null(report_dual) && !is.null(report_dual$turn_comparison)) {
      return(report_dual$turn_comparison)
    }

    report <- run_report()
    req(report)
    report$turn_summary
  }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 2)

  initialise_truth <- function(seed = NULL) {
    seed_value <- suppressWarnings(as.integer(seed)[1])
    if (length(seed_value) != 1L || is.na(seed_value) || seed_value < 1L) {
      seed_value <- sample.int(1e6, 1)
    }
    set.seed(seed_value)
    master_seed(seed_value)

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
      pop_tbl = inputs$pop_tbl,
      branch = "player"
    )

    starter_spend_stats <- summarise_budget(starter_alloc, list(pcr = 0, ag = 0), budget$budget)
    turn_logs$actions <- append_action_log(
      action_log = turn_logs$actions,
      turn = 0L,
      allocations = starter_alloc,
      pop_tbl = inputs$pop_tbl,
      costs = list(pcr = 0, ag = 0),
      branch = "player",
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
      pop_tbl = inputs$pop_tbl,
      branch = "player"
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
      reported_deaths = sum(init$cumulative_deaths),
      active_interventions = "None",
      notes = starter_note,
      queue_pending = starter_queue
    ))

    baseline_logs <- init_turn_logs(inputs$pop_tbl)
    baseline_logs$truth <- append_truth_log(
      truth_log = baseline_logs$truth,
      turn = init$turn,
      incidence = init$incidence,
      cumulative = init$cumulative,
      deaths = init$deaths,
      cumulative_deaths = init$cumulative_deaths,
      imports = integer(length(init$incidence)),
      Rt = params_epi$Rt,
      pop_tbl = inputs$pop_tbl,
      branch = "baseline"
    )

    baseline_branch(new_branch_state(
      name = "baseline",
      turn = init$turn,
      truth_state = clone_branch_object(init),
      truth_history = clone_branch_object(init$history),
      observation_state = init_observation_state(),
      available_now = empty_results,
      run_log = tibble(
        turn = 0L,
        truth_total = sum(init$incidence),
        tests_ag = 0L,
        tests_pcr = 0L,
        allocation_spend = 0,
        intervention_cost = 0,
        total_turn_spend = 0,
        cumulative_spend = 0,
        observed_tests = 0L,
        observed_positives = 0L,
        reported_deaths = sum(init$cumulative_deaths),
        active_interventions = "None",
        notes = "",
        queue_pending = 0L
      ),
      turn_logs = baseline_logs,
      total_spend = 0,
      spend_current = list(
        total = 0,
        last_turn = 0,
        remaining = budget$budget,
        within_budget = TRUE,
        interventions = "None"
      )
    ))

    run_report(NULL)
    dual_report(NULL)
  }

  reset_auto_state <- function(reason = NULL) {
    auto_running(FALSE)
    auto_turns_requested(0L)
    auto_turns_completed(0L)
    auto_stop_reason(reason)
  }

  derive_turn_seed <- function(master_seed_value, turn_index) {
    master_seed_value <- suppressWarnings(as.integer(master_seed_value))
    turn_index <- suppressWarnings(as.integer(turn_index))

    if (is.na(master_seed_value) || master_seed_value < 1L) {
      master_seed_value <- 1L
    }
    if (is.na(turn_index) || turn_index < 0L) {
      turn_index <- 0L
    }

    as.integer((master_seed_value * 1009L + turn_index * 9176L) %% .Machine$integer.max)
  }

  build_turn_request <- function(
      allocs,
      active_ids,
      current_turn,
      branch_name = "player",
      branch_truth_state = truth_state(),
      branch_observation_state = observation_state(),
      branch_total_spend = total_spend(),
      branch_run_log = run_log(),
      branch_turn_logs = list(
        truth = turn_logs$truth,
        observations = turn_logs$observations,
        actions = turn_logs$actions
      ),
      truth_seed = NULL) {
    list(
      current_turn = current_turn,
      allocs = allocs,
      budget = controls$budget(),
      params_epi = controls$epidemiology(),
      params_mort = controls$mortality(),
      testing_params = controls$testing(),
      active_ids = active_ids,
      intervention_defs = intervention_defs,
      total_spend = branch_total_spend,
      truth_state = branch_truth_state,
      observation_state = branch_observation_state,
      active_kernel = active_kernel(),
      effective_pop_tbl = effective_pop_tbl(),
      pop_tbl = inputs$pop_tbl,
      run_log = branch_run_log,
      turn_logs = branch_turn_logs,
      branch = branch_name,
      truth_seed = truth_seed
    )
  }

  apply_turn_result <- function(result, reset_allocations = TRUE) {
    total_spend(result$total_spend)
    spend_current(result$spend_current)

    observation_state(result$observation_state)
    available_now(result$available_now)

    truth_state(result$truth_state)
    truth_history(result$truth_history)

    turn_logs$actions <- result$turn_logs$actions
    turn_logs$observations <- result$turn_logs$observations
    turn_logs$truth <- result$turn_logs$truth

    run_log(result$run_log)
    turn(result$next_turn)
    if (isTRUE(reset_allocations)) {
      allocations_module$reset()
    }

    showNotification(
      result$notification$message,
      type = result$notification$type,
      duration = result$notification$duration
    )
  }

  step_dual_once <- function(allocs, active_ids, source = c("manual", "auto")) {
    source <- match.arg(source)
    req(truth_state())
    baseline <- baseline_branch()
    req(!is.null(baseline))

    turn_seed <- derive_turn_seed(master_seed(), turn())

    player_request <- build_turn_request(
      allocs = allocs,
      active_ids = active_ids,
      current_turn = turn(),
      branch_name = "player",
      truth_seed = turn_seed
    )

    baseline_allocs <- build_baseline_allocations(
      mode = controls$baseline_mode(),
      request = list(
        pop_tbl = effective_pop_tbl(),
        budget = controls$budget(),
        intervention_cost = 0,
        current_turn = turn(),
        observation_state = baseline$observation_state,
        available_now = baseline$available_now
      )
    )

    baseline_request <- build_turn_request(
      allocs = baseline_allocs,
      active_ids = character(),
      current_turn = turn(),
      branch_name = "baseline",
      branch_truth_state = baseline$truth_state,
      branch_observation_state = baseline$observation_state,
      branch_total_spend = baseline$total_spend,
      branch_run_log = baseline$run_log,
      branch_turn_logs = baseline$turn_logs,
      truth_seed = turn_seed
    )

    player_result <- run_single_turn(player_request)
    baseline_result <- run_single_turn(baseline_request)

    budget_failed <- identical(player_result$status, "budget_exceeded") ||
      identical(baseline_result$status, "budget_exceeded")
    if (isTRUE(budget_failed)) {
      error_message <- if (identical(player_result$status, "budget_exceeded")) {
        player_result$error_message
      } else {
        baseline_result$error_message
      }

      showNotification(error_message, type = "error")
      if (identical(source, "auto")) {
        auto_running(FALSE)
        auto_stop_reason("budget_exceeded")
      }
      return(FALSE)
    }

    baseline_branch(apply_branch_turn_result(baseline, baseline_result))

    apply_turn_result(
      player_result,
      reset_allocations = !identical(source, "auto")
    )
    TRUE
  }

  build_policy_request <- function(policy_id, allocs_manual, active_ids, current_turn) {
    intervention_cost <- compute_intervention_effects(
      active_ids,
      intervention_defs,
      turn_budget = controls$budget()$budget
    )$cost
    list(
      policy_id = policy_id,
      allocs_manual = allocs_manual,
      pop_tbl = effective_pop_tbl(),
      budget = controls$budget(),
      intervention_cost = intervention_cost,
      current_turn = current_turn,
      observation_state = observation_state(),
      available_now = available_now()
    )
  }

  initialise_or_resume_autorun <- function() {
    config_turns <- suppressWarnings(as.integer(controls$autorun()$turns))
    if (is.na(config_turns) || config_turns < 1L) {
      config_turns <- 1L
    }
    config_turns <- min(50L, config_turns)

    should_reset <- auto_turns_requested() <= 0L ||
      auto_turns_completed() >= auto_turns_requested() ||
      identical(auto_stop_reason(), "user_stop") ||
      identical(auto_stop_reason(), "completed") ||
      config_turns != auto_turns_requested()

    if (should_reset) {
      auto_turns_requested(config_turns)
      auto_turns_completed(0L)
    }

    auto_stop_reason(NULL)
  }

  run_auto_iteration <- function() {
    current_turn <- turn()
    active_ids <- allocations_module$interventions()
    manual_allocs <- allocations_module$allocations()
    policy_id <- controls$autorun()$policy_id

    policy_request <- build_policy_request(
      policy_id = policy_id,
      allocs_manual = manual_allocs,
      active_ids = active_ids,
      current_turn = current_turn
    )
    allocs_policy <- build_policy_allocations(policy_request)
    allocations_module$set_allocations(allocs_policy)

    step_ok <- step_dual_once(
      allocs = allocs_policy,
      active_ids = active_ids,
      source = "auto"
    )
    if (!isTRUE(step_ok)) {
      return(FALSE)
    }

    auto_turns_completed(auto_turns_completed() + 1L)
    TRUE
  }

  observeEvent(shared_reset_signal(), {
    reset_auto_state(reason = "reset_or_reseed")
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

    reset_auto_state(reason = "reset_or_reseed")
    initialise_truth(seed = controls$reseed())

    if (!already_initialised) {
      app_ready(TRUE)
    }
    if (already_initialised) {
      showNotification("Simulation reseeded", type = "message")
    }
  }, ignoreNULL = FALSE)

  observeEvent(controls$reset(), {
    reset_auto_state(reason = "reset_or_reseed")
    initialise_truth(seed = controls$reseed())
  })

  observeEvent(allocations_module$advance(), {
    req(truth_state())
    req(!isTRUE(auto_running()))

    step_dual_once(
      allocs = allocations_module$allocations(),
      active_ids = allocations_module$interventions(),
      source = "manual"
    )
  })

  observeEvent(controls$start_run(), {
    req(truth_state())
    req(identical(controls$run_mode(), "guided_auto"))

    initialise_or_resume_autorun()
    auto_running(TRUE)
  })

  observeEvent(controls$pause_run(), {
    req(identical(controls$run_mode(), "guided_auto"))
    auto_running(FALSE)
  })

  observeEvent(controls$stop_run(), {
    req(identical(controls$run_mode(), "guided_auto"))
    auto_running(FALSE)
    auto_stop_reason("user_stop")
  })

  observeEvent(controls$run_mode(), {
    if (identical(controls$run_mode(), "guided_auto")) {
      return()
    }
    if (isTRUE(auto_running())) {
      auto_running(FALSE)
    }
    if (auto_turns_requested() > 0L && auto_turns_completed() < auto_turns_requested()) {
      auto_stop_reason("user_stop")
    }
  }, ignoreInit = TRUE)

  observeEvent(controls$run_batch(), {
    req(truth_state())
    req(identical(controls$run_mode(), "guided_auto"))

    initialise_or_resume_autorun()
    auto_running(FALSE)

    remaining_steps <- auto_turns_requested() - auto_turns_completed()
    if (!is.finite(remaining_steps) || remaining_steps <= 0L) {
      auto_stop_reason("completed")
      return()
    }

    for (step_idx in seq_len(as.integer(remaining_steps))) {
      step_ok <- run_auto_iteration()
      if (!isTRUE(step_ok)) {
        break
      }

      if (auto_turns_completed() >= auto_turns_requested()) {
        auto_stop_reason("completed")
        break
      }
    }
  })

  observe({
    if (!isTRUE(auto_running())) {
      return()
    }

    if (!identical(controls$run_mode(), "guided_auto")) {
      auto_running(FALSE)
      auto_stop_reason("user_stop")
      return()
    }

    speed_ms <- controls$autorun()$speed_ms
    if (is.null(speed_ms) || !is.finite(speed_ms)) {
      speed_ms <- 700L
    }
    shiny::invalidateLater(as.integer(speed_ms), session)

    isolate({
      if (!isTRUE(auto_running())) {
        return()
      }

      if (auto_turns_requested() <= 0L || auto_turns_completed() >= auto_turns_requested()) {
        auto_running(FALSE)
        auto_stop_reason("completed")
        return()
      }

      step_ok <- run_auto_iteration()
      if (!isTRUE(step_ok)) {
        return()
      }

      if (auto_turns_completed() >= auto_turns_requested()) {
        auto_running(FALSE)
        auto_stop_reason("completed")
      }
    })
  })

  observeEvent(controls$end(), {
    req(truth_state())

    if (isTRUE(auto_running())) {
      auto_running(FALSE)
    }
    if (auto_turns_requested() > 0L && auto_turns_completed() < auto_turns_requested()) {
      auto_stop_reason("user_stop")
    }

    baseline <- baseline_branch()
    req(!is.null(baseline))

    player_branch <- new_branch_state(
      name = "player",
      turn = turn(),
      truth_state = truth_state(),
      truth_history = truth_history(),
      observation_state = observation_state(),
      available_now = available_now(),
      run_log = run_log(),
      turn_logs = list(
        truth = turn_logs$truth,
        observations = turn_logs$observations,
        actions = turn_logs$actions
      ),
      total_spend = total_spend(),
      spend_current = spend_current()
    )

    report_dual <- build_dual_branch_report(
      player_branch = player_branch,
      baseline_branch = baseline,
      pop_tbl = effective_pop_tbl()
    )

    report <- report_dual$player
    run_report(report)
    dual_report(report_dual)
    replay_snapshot(logs_snapshot())
    replay_unlocked(TRUE)

    metrics <- report$metrics
    comparison <- report_dual$comparison

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
      fluidRow(
        column(12,
          tags$div(
            class = "card bg-info-subtle mb-3",
            tags$div(class = "card-header", tags$strong("Player vs Baseline")),
            tags$div(
              class = "card-body",
              tags$p(
                tags$strong("Cases averted: "),
                as.integer(round(comparison$cases_averted))
              ),
              tags$p(
                tags$strong("Deaths averted: "),
                as.integer(round(comparison$deaths_averted))
              ),
              tags$p(
                tags$strong("Peak delay: "),
                if (is.na(comparison$peak_delay_turns)) "N/A" else paste0(comparison$peak_delay_turns, " turns")
              ),
              tags$p(
                tags$strong("Spend delta (player - baseline): "),
                if (is.na(comparison$spend_delta)) "N/A" else paste0("£", round(comparison$spend_delta, 0))
              )
            )
          )
        )
      ),
      tags$hr(),
      tags$h5("Turn-by-Turn Branch Comparison"),
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
