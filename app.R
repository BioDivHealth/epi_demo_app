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

# Library of other places (countries and cities); London is the default
place_index <- if (dir.exists(file.path("locations", "library"))) read_place_index() else NULL

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
  death_delay = 4L
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


intervention_defs <- intervention_catalog()

app_ui <- page_fluid(
  theme = bs_theme(bootswatch = "sandstone"),
  title = "Outbreaks in London: simulation",
  tags$head(
    tags$style(HTML("
      .main-header { margin-bottom: 1rem; }
      .top-bar {
        background: var(--bs-body-bg); border-bottom: 1px solid var(--bs-border-color);
        padding: 0.5rem 0.75rem; margin: 0 -0.75rem 0.75rem -0.75rem; z-index: 1000;
      }
      .top-bar h4 { margin: 0; color: var(--bs-emphasis-color); }
      .top-stat { font-size: 0.9rem; line-height: 1.2; }
      .top-stat .label { display: block; font-size: 0.7rem; color: var(--bs-secondary-color); text-transform: uppercase; }
      .top-stat .value { font-weight: 600; }
      /* no basemap tiles (the old tile service now needs an API key):
         boroughs are drawn on a plain light background */
      .leaflet-container, .leaflet-container.leaflet-touch { background: #1b2631 !important; }
      /* the page scrolls, not the cards inside it */
      .bslib-card, .bslib-card .card-body { overflow: visible !important; }
      .main-header h3 { margin: 0; color: #5a5a5a; }
      .header-actions { display: flex; align-items: center; gap: 0.5rem; }
      .help-button { white-space: nowrap; }
      .turn-badge { font-size: 1.1rem; font-weight: 600; }
      .scenario-label { font-size: 0.85rem; color: var(--bs-secondary-color); }
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
    # tell the server whether the browser is in dark mode (charts follow it)
    tags$script(HTML("(
      function() {
        var mq = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)');
        function isDark() { return !!(mq && mq.matches); }
        // the page itself switches (Bootstrap colour modes)...
        function applyTheme() {
          document.documentElement.setAttribute('data-bs-theme', isDark() ? 'dark' : 'light');
        }
        // ...and the server is told, so the charts switch with it
        function sendToServer() {
          var connected = window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected && Shiny.shinyapp.isConnected();
          if (connected) {
            Shiny.setInputValue('dark_mode', isDark());
            return true;
          }
          return false;
        }
        applyTheme();
        var tries = 0;
        (function waitForShiny() {
          if (!sendToServer() && tries++ < 80) { setTimeout(waitForShiny, 250); }
        })();
        if (mq && mq.addEventListener) {
          mq.addEventListener('change', function() { applyTheme(); sendToServer(); });
        }
      }
    )();")),
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
      h4(textOutput("place_title", inline = TRUE)),
      div(class = "scenario-label", textOutput("scenario_display", inline = TRUE))
    ),
    div(class = "top-stat", span(class = "label", "Date"), span(class = "value", textOutput("turn_display", inline = TRUE))),
    div(class = "top-stat", span(class = "label", "Budget"), span(class = "value", textOutput("money_display", inline = TRUE))),
    div(class = "top-stat", span(class = "label", "Testing and hospitals"), span(class = "value", textOutput("system_display", inline = TRUE))),
    div(
      class = "ms-auto d-flex flex-wrap gap-2",
      actionButton("advance_turn", "Advance to next report", class = "btn-primary"),
      actionButton("controls-end", "End response & review", class = "btn-success"),
      actionButton("controls-reseed", "New outbreak", class = "btn-outline-secondary btn-sm"),
      actionButton("controls-reset", "Replay this outbreak", class = "btn-outline-secondary btn-sm"),
      actionButton("choose_place", "Change location", class = "btn-outline-secondary btn-sm"),
      actionButton("help", "Briefing", class = "btn-outline-secondary btn-sm help-button")
    )
  ),
  uiOutput("main_layout"),
  div(
    class = "text-muted small mt-2",
    textOutput("data_credit", inline = TRUE)
  )
)

ui <- if (auth_disabled) app_ui else secure_app(app_ui, enable_admin = FALSE)

server <- function(input, output, session) {
  # Which place this session plays: ?place=<id> in the address (set by the
  # "Change location" window), London otherwise. Loaded once per session, so a
  # new place starts a fresh page and a fresh outbreak.
  place_meta <- NULL
  query <- shiny::parseQueryString(shiny::isolate(session$clientData$url_search))
  place_id <- query$place
  if (!is.null(place_id) && !is.null(place_index) && place_id %in% place_index$id &&
      !isTRUE(place_index$use_builtin[place_index$id == place_id])) {
    place <- load_place(place_id, place_index)
    place_meta <- place$meta
    inputs <- place$inputs
    # measles spreads more locally: steeper distance decay
    inputs$K_measles <- load_place(place_id, place_index, gravity = list(alpha = 1, beta = 3))$inputs$K
  }

  # allocation table in this place's area order
  allocations_template <- tibble(
    borough_index = seq_len(nrow(inputs$pop_tbl)),
    borough_name = inputs$pop_tbl$borough_name,
    n_ag = integer(nrow(inputs$pop_tbl)),
    n_pcr = integer(nrow(inputs$pop_tbl))
  )

  output$place_title <- renderText({
    # cities by their own name ("Lagos"), countries by the country name
    place_name <- if (is.null(place_meta)) {
      "London"
    } else if (identical(place_meta$type[[1]], "city")) {
      sub(" [(].*$", "", place_meta$label[[1]])
    } else {
      place_meta$country[[1]]
    }
    paste("Outbreaks in", place_name)
  })
  output$data_credit <- renderText({
    if (is.null(place_meta)) {
      "Data: GLA/ONS borough boundaries and populations."
    } else {
      paste0(
        place_meta$label[[1]], ": ", nrow(inputs$pop_tbl), " areas. Boundaries: geoBoundaries (",
        place_meta$boundary_license[[1]] %||% "see docs/DATA_SOURCES.md",
        "); population: GHS-POP 2020 (EC JRC, CC BY 4.0)."
      )
    }
  })

  # "Change location": the picker in a window, then reload with ?place=<id>
  picked_place <- if (!is.null(place_index)) mod_location_picker_server("picker") else NULL

  observeEvent(input$choose_place, {
    req(picked_place)
    showModal(modalDialog(
      title = "Choose where the outbreak happens",
      mod_location_picker_ui("picker", default_query = if (is.null(place_meta)) "London" else place_meta$country[[1]]),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("play_place", "Start a new outbreak here", class = "btn-primary")
      ),
      size = "xl",
      easyClose = TRUE
    ))
  })

  observeEvent(input$play_place, {
    req(picked_place)
    chosen <- picked_place()$meta
    target <- if (isTRUE(chosen$use_builtin[[1]])) "?" else paste0("?place=", chosen$id[[1]])
    updateQueryString(target, mode = "push")
    session$reload()
  })

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

  # Situation report shown when each new outbreak starts
  show_sitrep <- function(patient_area, report_date, first_event = "presented with") {
    place_name <- if (is.null(place_meta)) {
      "London"
    } else if (identical(place_meta$type[[1]], "city")) {
      sub(" [(].*$", "", place_meta$label[[1]])
    } else {
      place_meta$country[[1]]
    }
    showModal(modalDialog(
      title = paste("Situation report:", format_date(report_date)),
      tags$p(
        "On ", tags$strong(format_date(report_date)), ", a patient ", first_event, " an unidentified illness in ",
        tags$strong(patient_area), ". We know nothing about where they caught it or where it came from."
      ),
      tags$p(
        "You are the chief epidemiologist for ", tags$strong(place_name),
        " and have been tasked with controlling it."
      ),
      tags$p(tags$strong("What will you do?")),
      tags$p(sprintf(
        "You have %d days, until %s, to bring it under control. You will get a surveillance report every %d days, and you have one budget for the whole response.",
        response_days(current_config()), format_date(response_end_date(current_config())),
        current_config()$bio$gen_days
      )),
      footer = tagList(
        actionButton("sitrep_help", "Read the briefing", class = "btn-outline-secondary"),
        modalButton("Open the response room")
      ),
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

  # (no automatic help pop-up: each new outbreak opens with a situation
  # report instead, which links to the briefing)
  observeEvent(input$sitrep_help, {
    show_help_modal()
  })

  observeEvent(input$help, {
    show_help_modal()
    help_shown(TRUE)
    help_pending(FALSE)
  })

  output$main_layout <- renderUI({
    # problem (map) and measurement (surveillance) side by side on top, the
    # response underneath; stacked on narrow screens
    main <- tagList(
      bslib::layout_columns(
        col_widths = bslib::breakpoints(sm = c(12, 12), lg = c(7, 5)),
        fill = FALSE,
        mod_map_ui("map", intervention_defs = intervention_defs),
        mod_dashboard_ui("dashboard")
      ),
      mod_response_ui("map", intervention_defs = intervention_defs)
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
    config <- current_config()
    paste0(
      "Day ", turn_day(turn(), config), " of ", response_days(config), " · ",
      format_date(turn_date(turn(), config))
    )
  })

  # money left this turn (after the current plan) and in total
  output$money_display <- renderText({
    req(game_state())
    budget <- turn_budget_info()
    planned <- allocations_module$planned_total()
    paste0(
      format_money(budget$budget - planned), " left this turn · ",
      format_money(budget$pot_left), " left in total"
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
    if (!is.null(place_meta)) {
      # the admin picks seed boroughs by London index; elsewhere start at random
      seed_borough <- NA_integer_
    }
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
        hosp_frac = scenario$hosp_frac[[1]],
        gen_days = scenario$gen_days[[1]],
        hosp_delay = scenario$hosp_delay[[1]]
      )
    )
  })

  # copy the engine state into the reactives the modules and logs read
  sync_from_state <- function(state) {
    game_state(state)
    truth_state(state$truth)
    truth_history(utils::tail(state$truth$history, -(state$truth_offset %||% 0)))
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
      interventions = view$interventions,
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
    intervention_defs = intervention_defs,
    # only the public first report: the area and what happened there
    first_report = reactive({
      info <- starter_alert_info()
      if (is.null(info)) return(NULL)
      list(
        index = match(info$alerted, inputs$pop_tbl$borough_name),
        label = sprintf("First patient %s here, %s", info$first_event, info$report_date_txt)
      )
    })
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
      list(
        bed_pressure = view$bed_pressure,
        lab_backlog = view$lab_backlog,
        pot_share = view$budget_left / current_config()$tp$budget_total
      )
    }),
    date_label = function(turns) format(turn_date(turns, current_config()), "%d %b"),
    dark_mode = reactive(isTRUE(input$dark_mode)),
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
    state <- new_game(config, seed = seed, sampling_mode = "symptomatic")
    init <- state$truth
    starter_alloc <- state$starter_alloc
    game_seed(state$seed)

    # the first patient, and (hidden until the review) where and when it began
    patient_area <- inputs$pop_tbl$borough_name[state$alert$alerted]
    report_date <- as.Date(config$tp$start_date)
    starter_alert_info(list(
      alerted = patient_area,
      true_start = inputs$pop_tbl$borough_name[state$alert$true_start],
      hidden_days = state$alert$hidden_turns * config$bio$gen_days,
      began = report_date - state$alert$hidden_turns * config$bio$gen_days,
      infected_by_report = state$start_infections,
      first_event = if (identical(state$alert$trigger, "death")) "died" else "reported",
      report_date_txt = format_date(report_date)
    ))

    first_event <- if (identical(state$alert$trigger, "death")) "died of" else "presented with"
    turn_log(sprintf(
      "%s: a patient %s an unidentified illness in %s. Nothing else is known yet.",
      format_date(report_date), first_event, patient_area
    ))

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
      turn = 0L,
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
    allocations_module$clear_interventions(
      fair_share = player_view(state, config)$allowance,
      max_spend = config$tp$tests$budget
    )

    starter_note <- paste("First patient reported in", patient_area)
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

    # situation report: the brief for this outbreak
    show_sitrep(patient_area, report_date, first_event)
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
      showNotification("Time is up. Press End response & review to see what really happened.", type = "message")
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
      turn = new_state$turn,
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
      log <- c(log, sprintf("Time is up (day %d). Press End response & review.", response_days(config)))
    }
    turn_log(c(sprintf(
      "Report for %s (day %d):",
      format_date(turn_date(new_state$turn, config)), turn_day(new_state$turn, config)
    ), log))
  })

  # Review chart: real new infections (black) against confirmed cases (red),
  # from the true start, with the unseen period shaded
  output$review_truth_plot <- renderPlot({
    req(run_report())
    state <- game_state()
    config <- current_config()
    offset <- state$truth_offset %||% 0

    real <- tibble(
      turn = seq_along(state$truth$history) - 1L - offset,
      cases = vapply(state$truth$history, sum, numeric(1)),
      series = "Real new infections"
    )

    hist <- state$obs$history
    last_turn <- max(real$turn)
    seen <- numeric(last_turn + 1)
    if (nrow(hist) > 0) {
      by_turn <- tapply(hist$positives, hist$turn_available, sum)
      keep <- as.integer(names(by_turn)) <= last_turn
      seen[as.integer(names(by_turn))[keep] + 1] <- by_turn[keep]
    }
    observed <- tibble(turn = 0:last_turn, cases = seen, series = "Confirmed cases you saw")

    both <- bind_rows(real, observed)
    both$date <- turn_date(both$turn, config)
    both$series <- factor(both$series, levels = c("Real new infections", "Confirmed cases you saw"))
    report_date <- as.Date(config$tp$start_date)
    true_start <- min(both$date)
    col <- chart_colours(isTRUE(input$dark_mode))

    ggplot2::ggplot(both, ggplot2::aes(x = date, y = cases, colour = series)) +
      ggplot2::annotate("rect", xmin = true_start, xmax = report_date, ymin = -Inf, ymax = Inf,
                        fill = col$shade, alpha = 0.6) +
      ggplot2::annotate("text", x = true_start, y = Inf, label = "Unseen spread",
                        hjust = -0.05, vjust = 1.5, size = 3.5, colour = col$muted) +
      ggplot2::geom_vline(xintercept = report_date, linetype = "dashed", colour = col$muted) +
      ggplot2::annotate("text", x = report_date, y = Inf, label = "First patient reported",
                        hjust = -0.05, vjust = 3.2, size = 3.5, colour = col$muted) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::scale_colour_manual(values = c("Real new infections" = col$real, "Confirmed cases you saw" = "#e8443a")) +
      ggplot2::scale_y_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        breaks = c(0, 1, 10, 100, 1000, 1e4, 1e5, 1e6, 1e7),
        labels = scales::label_comma()
      ) +
      ggplot2::labs(x = NULL, y = "Cases per report", colour = NULL) +
      chart_theme(isTRUE(input$dark_mode), base_size = 12) +
      ggplot2::theme(legend.position = "top")
  }, bg = "transparent")

  # Review card: what the disease was, and where and when it really began
  disease_card_ui <- function() {
    config <- current_config()
    scenario <- scenario_lookup(shared_admin$scenario_id)
    alert <- starter_alert_info()
    bio <- config$bio
    pct <- function(x) paste0(round(100 * x), "%")

    properties <- tibble(
      Property = c(
        "Each case would infect (nobody immune)", "Each case infected at the start",
        "Infections with no symptoms", "Spread before symptoms",
        "Infected people needing hospital", "Infected people who die",
        "Days per generation of infection"
      ),
      Value = c(
        format(config$epi$Rt), sprintf("%.2f", config$epi$Rt * config$susceptible_frac),
        pct(bio$asymptomatic), pct(bio$presymptomatic),
        pct(bio$hosp_frac), sprintf("%.1f%%", 100 * config$mort$death_ifr),
        format(bio$gen_days)
      )
    )

    began_txt <- if (is.null(alert)) "" else sprintf(
      paste(
        "The true start date was %s, with one infected person in %s.",
        "By the time the first patient was reported (%s, in %s, %d days later),",
        "%s people had been infected."
      ),
      format_date(alert$began), alert$true_start,
      format_date(as.Date(config$tp$start_date)), alert$alerted, alert$hidden_days,
      format(alert$infected_by_report, big.mark = ",")
    )

    tags$div(
      class = "card border-dark mb-3",
      tags$div(class = "card-header", tags$strong(paste("The disease:", scenario$label[[1]]))),
      tags$div(
        class = "card-body small",
        tags$p(scenario$blurb[[1]]),
        tags$p(tags$strong(began_txt)),
        tags$table(
          class = "table table-sm mb-0",
          tags$tbody(lapply(seq_len(nrow(properties)), function(i) {
            tags$tr(tags$td(properties$Property[i]), tags$td(tags$strong(properties$Value[i])))
          }))
        )
      )
    )
  }

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
      ""
    } else if (identical(alert$alerted, alert$true_start)) {
      sprintf(
        "The first patient was reported in %s, where the outbreak had begun %d days earlier.",
        alert$true_start, alert$hidden_days
      )
    } else {
      sprintf(
        "The first patient was reported in %s, but the outbreak had begun in %s %d days earlier.",
        alert$alerted, alert$true_start, alert$hidden_days
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
      title = "After-action review: what really happened",
      disease_card_ui(),
      tags$h5("What you saw against what was really happening"),
      plotOutput("review_truth_plot", height = "320px"),
      tags$p(
        class = "text-muted small",
        "Black: real new infections each report (including the unseen start). Red: confirmed cases you saw.",
        "The scale is logarithmic, so each gridline is ten times the one below."
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
                format_money(metrics$total_spend)
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
    open_signal = reactive(replay_open_id()),
    dark_mode = reactive(isTRUE(input$dark_mode))
  )
}

shinyApp(ui, server)
