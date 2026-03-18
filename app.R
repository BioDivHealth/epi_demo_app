library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tibble)
library(EpiEstim)
library(shinymanager)
library(shinyjs)

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

credentials <- NULL
if (!auth_disabled && file.exists(auth_credentials_path)) {
  credentials <- load_credentials(auth_credentials_path)
} else if (!auth_disabled) {
  warning(sprintf(
    "Admin credentials file not found at '%s'; starting in game-only mode.",
    auth_credentials_path
  ), call. = FALSE)
}

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
  show_scenario = FALSE,
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
  turn_available = integer(),
  source = character()
)

allocations_template <- tibble(
  borough_index = seq_len(nrow(inputs$pop_tbl)),
  borough_name = inputs$pop_tbl$borough_name,
  n_ag = integer(nrow(inputs$pop_tbl)),
  n_pcr = integer(nrow(inputs$pop_tbl))
)

intervention_defs <- intervention_catalog()
style_asset_version <- as.integer(file.info(file.path("www", "style.css"))$mtime)
if (!is.finite(style_asset_version) || is.na(style_asset_version)) {
  style_asset_version <- as.integer(Sys.time())
}

app_ui <- page_fluid(
  theme = bs_theme(bootswatch = "sandstone"),
  title = "Outbreaks in London: simulation",
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = paste0("style.css?v=", style_asset_version)),
    tags$script(src = "help-auto.js"),
    tags$script(src = "auth-enter-fix.js")
  ),
  div(
    class = "main-header d-flex justify-content-between align-items-center",
    div(
      class = "main-header-title",
      h3("Outbreaks in London: simulation"),
      uiOutput("scenario_display")
    ),
    div(
      class = "header-actions",
      mod_controls_header_ui(
        "controls",
        lifecycle_extra = tagList(
          uiOutput("session_actions"),
          actionButton("help", "Help", class = "btn btn-outline-secondary btn-sm help-button")
        )
      )
    )
  ),
  uiOutput("main_layout"),
  div(
    class = "text-muted small mt-2",
    "Data: GLA/ONS borough boundaries. Click boroughs to allocate tests, then advance turns."
  )
)

ui <- app_ui

server <- function(input, output, session) {
  turn_progress_notification_id <- "turn-progress-toast"
  admin_session <- reactiveValues(
    active = FALSE,
    user = NULL
  )
  admin_login_error <- reactiveVal(NULL)

  is_admin <- reactive({
    if (auth_disabled) {
      return(FALSE)
    }
    isTRUE(admin_session$active)
  })

  help_shown <- reactiveVal(FALSE)
  help_pending <- reactiveVal(FALSE)

  auth_ready <- reactive({
    TRUE
  })

  show_help_modal <- function() {
    scenario_id <- isolate(shared_admin$scenario_id)
    show_scenario <- isTRUE(isolate(shared_admin$show_scenario))
    showModal(modalDialog(
      title = "Welcome and guidance",
      help_modal_body(
        scenario_id = scenario_id,
        show_scenario = show_scenario
      ),
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

  show_admin_login_modal <- function() {
    admin_login_error(NULL)
    showModal(modalDialog(
      title = "Admin sign in",
      div(
        class = "admin-login-note",
        "Sign in here only to unlock admin controls."
      ),
      textInput("admin_login_user", "Username"),
      passwordInput("admin_login_password", "Password"),
      uiOutput("admin_login_feedback"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("admin_login_submit", "Sign in", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  output$session_actions <- renderUI({
    if (is_admin()) {
      return(div(
        class = "session-auth-controls",
        span(class = "session-role-pill session-role-pill-admin", "Admin mode"),
        span(class = "session-role-user text-muted small", admin_session$user),
        actionButton(
          "admin_logout",
          "Leave admin mode",
          class = "btn btn-outline-secondary btn-sm"
        )
      ))
    }

    if (auth_disabled || is.null(credentials)) {
      return(div(
        class = "session-auth-controls",
        span(class = "session-role-pill", "Game mode")
      ))
    }

    div(
      class = "session-auth-controls",
      span(class = "session-role-pill", "Game mode"),
      actionButton(
        "admin_login",
        "Admin sign in",
        class = "btn btn-outline-dark btn-sm"
      )
    )
  })

  output$admin_login_feedback <- renderUI({
    message <- admin_login_error()
    if (is.null(message) || identical(message, "")) {
      return(NULL)
    }
    div(class = "text-danger small", message)
  })

  observeEvent(input$admin_login, {
    show_admin_login_modal()
  })

  observeEvent(input$admin_login_submit, {
    result <- check_admin_credentials(
      user = input$admin_login_user,
      password = input$admin_login_password,
      creds = credentials
    )

    if (isTRUE(result$ok)) {
      admin_session$active <- TRUE
      admin_session$user <- result$user
      admin_login_error(NULL)
      removeModal()
      showNotification("Admin controls unlocked for this session.", type = "message")
      return()
    }

    admin_login_error(admin_auth_failure_message(result$reason))
  })

  observeEvent(input$admin_logout, {
    admin_session$active <- FALSE
    admin_session$user <- NULL
    showNotification("Returned to game mode.", type = "message")
  })

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
    content <- bslib::layout_columns(
      col_widths = if (is_admin()) c(8, 4) else c(7, 5),
      fill = FALSE,
      fillable = FALSE,
      mod_map_ui(
        "map",
        intervention_defs = intervention_defs,
        run_controls_ui = mod_controls_run_ui("controls"),
        run_mode_ui = mod_controls_run_mode_ui("controls"),
        testing_pool_ui = mod_controls_testing_pool_ui("controls"),
        borough_policy_ui = mod_controls_borough_policy_ui("controls")
      ),
      mod_dashboard_ui(
        "dashboard",
        turn_ui = div(
          class = "turn-badge badge bg-secondary",
          textOutput("turn_display", inline = TRUE)
        )
      )
    )
    if (is_admin()) {
      bslib::layout_sidebar(
        content,
        sidebar = bslib::sidebar(
          mod_controls_ui("controls"),
          width = "280px",
          open = "desktop"
        ),
        fill = FALSE
      )
    } else {
      content
    }
  })

  turn <- reactiveVal(0L)
  total_spend <- reactiveVal(0)
  spend_current <- reactiveVal(list(total = 0, last_turn = 0, remaining = 0, within_budget = TRUE, interventions = "None"))
  intervention_state <- reactiveVal(init_intervention_state(intervention_defs))
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

  output$scenario_display <- renderUI({
    scenario_display <- student_scenario_display(
      scenario_id = shared_admin$scenario_id,
      show_scenario = shared_admin$show_scenario
    )
    if (!isTRUE(scenario_display$visible)) {
      return(NULL)
    }
    div(class = "scenario-label", scenario_display$header_text)
  })

  run_log <- reactiveVal(tibble())
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
  replay_snapshot <- reactiveVal(build_replay_snapshot(NULL, NULL))
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
    intervention_state_info = reactive(intervention_state()),
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
    current_turn = turn,
    total_boroughs = nrow(inputs$pop_tbl),
    planned_turn_mix = reactive(allocations_module$planned_turn_mix()),
    comparison_report = reactive(dual_report()),
    run_status = reactive({
      mode <- controls$run_mode()
      list(
        mode = mode,
        running = isTRUE(auto_running()),
        turns_completed = auto_turns_completed(),
        turns_requested = auto_turns_requested(),
        stop_reason = auto_stop_reason(),
        revealed = isTRUE(replay_unlocked())
      )
    }),
    app_ready = app_ready
  )

  output$endgame_prevalence_plot <- renderPlot({
    req(isTRUE(replay_unlocked()))
    snapshot <- replay_snapshot()
    player_logs <- replay_get_branch_logs(snapshot, branch = "player")
    player_truth_log <- replay_resolve_truth_log(
      log_data = snapshot,
      borough_names = inputs$boros$borough_name,
      branch = "player"
    )
    baseline_truth_log <- replay_resolve_truth_log(
      log_data = snapshot,
      borough_names = inputs$boros$borough_name,
      branch = "baseline"
    )
    common_turns <- sort(unique(c(
      suppressWarnings(as.integer(player_truth_log$turn)),
      suppressWarnings(as.integer(player_logs$observations$turn_reported)),
      suppressWarnings(as.integer(baseline_truth_log$turn)),
      suppressWarnings(as.integer(player_logs$actions$turn))
    )))
    common_turns <- common_turns[is.finite(common_turns)]

    prevalence_plot <- replay_plot_prevalence(
      truth_log = player_truth_log,
      obs_log = player_logs$observations,
      pop_tbl = inputs$pop_tbl,
      window = 3L,
      baseline_truth_log = baseline_truth_log
    )
    cumulative_plot <- replay_plot_cumulative(
      truth_log = player_truth_log,
      obs_log = player_logs$observations,
      pop_tbl = inputs$pop_tbl,
      window = 3L,
      baseline_truth_log = baseline_truth_log,
      cumulative_series = c("observed", "player_truth", "baseline_truth"),
      show_legend = FALSE
    )
    timeline_plot <- replay_plot_intervention_timeline(
      run_log = player_logs$run_log,
      action_log = player_logs$actions,
      intervention_defs = intervention_defs,
      turns = common_turns
    )
    testing_plot <- replay_plot_testing(
      action_log = player_logs$actions,
      turns = common_turns
    )

    plot_list <- list(
      prevalence = prevalence_plot,
      cumulative = cumulative_plot,
      interventions = timeline_plot,
      testing = testing_plot
    )

    if (length(common_turns) > 0) {
      x_limits <- range(common_turns, na.rm = TRUE)
      plot_list <- list(
        prevalence = if (is.null(plot_list$prevalence)) NULL else {
          plot_list$prevalence + ggplot2::coord_cartesian(xlim = x_limits)
        },
        cumulative = if (is.null(plot_list$cumulative)) NULL else {
          plot_list$cumulative + ggplot2::coord_cartesian(xlim = x_limits, clip = "off")
        },
        interventions = if (is.null(plot_list$interventions)) NULL else {
          plot_list$interventions + ggplot2::coord_cartesian(xlim = x_limits)
        },
        testing = if (is.null(plot_list$testing)) NULL else {
          plot_list$testing + ggplot2::coord_cartesian(xlim = x_limits)
        }
      )
    }

    available_names <- names(plot_list)[!vapply(plot_list, is.null, logical(1))]
    if (length(available_names) == 0) {
      return(invisible(NULL))
    }
    if (length(available_names) == 1) {
      print(plot_list[[available_names[[1]]]])
      return(invisible(NULL))
    }

    grobs <- lapply(plot_list[available_names], ggplot2::ggplotGrob)
    shared_widths <- Reduce(
      grid::unit.pmax,
      lapply(grobs, function(grob_obj) grob_obj$widths)
    )
    grobs <- lapply(grobs, function(grob_obj) {
      grob_obj$widths <- shared_widths
      grob_obj
    })

    panel_height_map <- c(
      prevalence = 3.5,
      cumulative = 2.5,
      interventions = 1.4,
      testing = 2.0
    )
    panel_heights <- panel_height_map[available_names]

    grid::grid.newpage()
    grid::pushViewport(grid::viewport(
      layout = grid::grid.layout(
        nrow = length(grobs),
        ncol = 1,
        heights = grid::unit(panel_heights, "null")
      )
    ))
    for (i in seq_along(grobs)) {
      grid::grid.draw(grid::editGrob(
        grobs[[i]],
        vp = grid::viewport(layout.pos.row = i, layout.pos.col = 1)
      ))
    }
    grid::popViewport()
  }, res = 96)

  current_player_branch <- function() {
    new_branch_state(
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
      spend_current = spend_current(),
      intervention_state = intervention_state()
    )
  }

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
    replay_snapshot(build_replay_snapshot(NULL, NULL))

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
        params = testing_params,
        source = "starter_intel"
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
        interventions_active = "None",
        intervention_statuses = "None"
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
    intervention_state(init_intervention_state(intervention_defs))
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
      intervention_statuses = "None",
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
        intervention_statuses = "None",
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
      ),
      intervention_state = init_intervention_state(intervention_defs)
    ))

    dual_report(NULL)
  }

  reset_auto_state <- function(reason = NULL) {
    auto_running(FALSE)
    auto_turns_requested(0L)
    auto_turns_completed(0L)
    auto_stop_reason(reason)
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
      branch_intervention_state = intervention_state(),
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
      intervention_state = branch_intervention_state,
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
    intervention_state(result$intervention_state)

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
      duration = result$notification$duration,
      id = turn_progress_notification_id,
      closeButton = FALSE
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
      branch_intervention_state = baseline$intervention_state,
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
      selected = active_ids,
      catalog = intervention_defs,
      state = intervention_state(),
      observation_history = observation_state()$history,
      current_turn = current_turn,
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

  resolve_player_turn_allocations <- function(current_turn, active_ids) {
    manual_allocs <- allocations_module$allocations()
    policy_request <- build_policy_request(
      policy_id = controls$autorun()$policy_id,
      allocs_manual = manual_allocs,
      active_ids = active_ids,
      current_turn = current_turn
    )

    allocs <- build_run_mode_allocations(
      run_mode = controls$run_mode(),
      policy_request = policy_request
    )

    if (identical(controls$run_mode(), "guided_auto")) {
      allocations_module$set_allocations(allocs)
    }

    allocs
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
    allocs_policy <- resolve_player_turn_allocations(
      current_turn = current_turn,
      active_ids = active_ids
    )

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
    removeNotification(turn_progress_notification_id)
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

    removeNotification(turn_progress_notification_id)
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
    removeNotification(turn_progress_notification_id)
    reset_auto_state(reason = "reset_or_reseed")
    initialise_truth(seed = controls$reseed())
  })

  observeEvent(allocations_module$advance(), {
    req(truth_state())
    req(!isTRUE(auto_running()))

    active_ids <- allocations_module$interventions()
    allocs <- resolve_player_turn_allocations(
      current_turn = turn(),
      active_ids = active_ids
    )
    step_source <- if (identical(controls$run_mode(), "guided_auto")) "auto" else "manual"

    step_dual_once(
      allocs = allocs,
      active_ids = active_ids,
      source = step_source
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

  observe({
    running <- isTRUE(auto_running())
    # transport buttons
    shinyjs::toggleState("controls-start_run", condition = !running)
    shinyjs::toggleState("controls-run_batch", condition = !running)
    shinyjs::toggleState("controls-pause_run", condition = running)
    shinyjs::toggleState("controls-stop_run", condition = running)
    # map action buttons
    shinyjs::toggleState("map-advance", condition = !running)
    shinyjs::toggleState("map-clear_alloc", condition = !running)
    shinyjs::toggleState("map-mass_test", condition = !running)
    # strategy inputs
    shinyjs::toggleState("map-interventions", condition = !running)
    shinyjs::toggleState("map-test_type", condition = !running)
    shinyjs::toggleState("map-tests_per_click", condition = !running)
    shinyjs::toggleState("controls-testing_pool", condition = !running)
    shinyjs::toggleState("controls-allocation_policy", condition = !running)
    # controls strip overlay
    shinyjs::toggleClass(id = "map-controls_strip", class = "strip-locked", condition = running)
  })

  output[["controls-turn_progress"]] <- shiny::renderUI({
    completed <- auto_turns_completed()
    requested <- auto_turns_requested()
    running <- isTRUE(auto_running())
    if (!running || requested <= 0L) {
      return(NULL)
    }
    pct <- round(100 * min(completed, requested) / requested)
    shiny::div(
      class = "map-run-progress",
      shiny::div(
        class = "map-run-progress-label",
        paste0("Turn ", completed, " of ", requested)
      ),
      shiny::div(
        class = "progress map-run-progress-bar",
        shiny::div(
          class = "progress-bar",
          role = "progressbar",
          style = paste0("width: ", pct, "%"),
          `aria-valuenow` = pct,
          `aria-valuemin` = "0",
          `aria-valuemax` = "100"
        )
      )
    )
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

    player_branch <- current_player_branch()

    report_dual <- build_dual_branch_report(
      player_branch = player_branch,
      baseline_branch = baseline,
      pop_tbl = effective_pop_tbl()
    )

    dual_report(report_dual)
    replay_snapshot(build_replay_snapshot(player_branch, baseline, report_dual))
    replay_unlocked(TRUE)

    metrics <- report_dual$player$metrics
    comparison <- report_dual$comparison

    total_true <- sum(sapply(truth_history(), sum))
    total_observed <- sum(observation_state()$history$positives)
    detection_rate <- if (total_true > 0) round(100 * total_observed / total_true, 1) else NA

    showModal(build_endgame_modal(metrics, comparison, detection_rate))
  })

  observeEvent(input$show_replay, {
    removeModal()
    if (isTRUE(replay_unlocked())) {
      replay_snapshot(build_replay_snapshot(
        player_branch = current_player_branch(),
        baseline_branch = baseline_branch(),
        comparison_report = dual_report()
      ))
    }
    showModal(modalDialog(
      title = "Turn-by-Turn Replay",
      mod_replay_ui("replay"),
      footer = modalButton("Close"),
      size = "xl",
      easyClose = TRUE
    ))
    replay_open_id(replay_open_id() + 1L)
  })

  mod_replay_server(
    id = "replay",
    logs = reactive(replay_snapshot()),
    boros = inputs$boros,
    pop_tbl = inputs$pop_tbl,
    ended = reactive(replay_unlocked()),
    open_signal = reactive(replay_open_id()),
    intervention_defs = intervention_defs
  )
}

shinyApp(ui, server)
