library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(tibble)
library(EpiEstim)

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

inputs <- load_inputs()

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

ui <- page_fluid(
  theme = bs_theme(bootswatch = "sandstone"),
  title = "Outbreak London (boroughs)",
  fluidRow(
    column(3, mod_controls_ui("controls")),
    column(5, mod_map_ui("map", intervention_defs = intervention_defs)),
    column(4, mod_dashboard_ui("dashboard"))
  ),
  fluidRow(
    column(12, mod_replay_ui("replay"))
  ),
  card(
    card_body(
      HTML("Data layers are precomputed from GLA/ONS sources; loaded from ./data/*.rds."),
      class = "small text-muted"
    )
  )
)

server <- function(input, output, session) {
  turn <- reactiveVal(0L)
  total_spend <- reactiveVal(0)
  spend_current <- reactiveVal(list(total = 0, last_turn = 0, remaining = 0, within_budget = TRUE, interventions = "None"))

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

  truth_state <- reactiveVal(NULL)
  truth_history <- reactiveVal(list())

  controls <- mod_controls_server("controls")

  allocations_module <- mod_map_server(
    id = "map",
    boros = inputs$boros,
    allocations_init = allocations_template,
    budget_info = controls$budget,
    observed_this_turn = reactive(available_now()),
    intervention_defs = intervention_defs
  )

  dashboard_module <- mod_dashboard_server(
    id = "dashboard",
    observed_history = reactive(observation_state()$history),
    queue_state = reactive(observation_state()$queue),
    spend_info = spend_current,
    available_now = reactive(available_now())
  )

  logs_snapshot <- reactive({
    list(
      truth = turn_logs$truth,
      observations = turn_logs$observations,
      actions = turn_logs$actions
    )
  })

  mod_replay_server(
    id = "replay",
    logs = logs_snapshot,
    boros = inputs$boros,
    ended = reactive(replay_unlocked())
  )

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

    init <- init_truth(
      n_boroughs = nrow(inputs$pop_tbl),
      seed_boroughs = 1L,
      seed_cases = 20L
    )

    blank_logs <- init_turn_logs(inputs$pop_tbl)
    turn_logs$truth <- blank_logs$truth
    turn_logs$observations <- blank_logs$observations
    turn_logs$actions <- blank_logs$actions
    replay_unlocked(FALSE)

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
        pop_tbl = inputs$pop_tbl,
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
      active_interventions = "None",
      notes = starter_note,
      queue_pending = starter_queue
    ))

    run_report(NULL)
  }

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
    initialise_truth(seed = controls$reseed())
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
      pop_tbl = inputs$pop_tbl,
      params = testing_params
    )

    observation_state(list(queue = obs$queue, history = obs$history))
    available_now(obs$available)

    turn_logs$observations <- append_observation_log(
      observation_log = turn_logs$observations,
      available_results = obs$available,
      turn_reported = current_turn,
      pop_tbl = inputs$pop_tbl
    )

    next_truth <- step_truth(
      state = truth_state(),
      K = inputs$K,
      Rt_global = params_epi$Rt,
      k_overdisp = params_epi$k,
      imports_prob = params_epi$imports_prob,
      lambda_import = params_epi$lambda_import
    )

    truth_state(next_truth)
    truth_history(next_truth$history)

    turn_logs$truth <- append_truth_log(
      truth_log = turn_logs$truth,
      turn = next_truth$turn,
      incidence = next_truth$incidence,
      cumulative = next_truth$cumulative,
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
  })

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

    metrics <- report$metrics
    metric_items <- tagList(
      tags$li(sprintf(
        "Time to first detection: %s",
        ifelse(is.na(metrics$time_to_detection), "Not detected", metrics$time_to_detection)
      )),
      tags$li(sprintf("Peak true incidence: %s", metrics$peak_true_incidence)),
      tags$li(sprintf("Peak observed positives: %s", metrics$peak_observed_positives)),
      tags$li(sprintf(
        "Percent positive: %s",
        ifelse(is.na(metrics$percent_positive), "NA", sprintf("%.1f%%", metrics$percent_positive))
      )),
      tags$li(sprintf("Total spend: £%s", round(metrics$total_spend, 1))),
      tags$li(sprintf("Calibration gap (RMSE): %.3f", metrics$calibration_gap))
    )

    showModal(modalDialog(
      title = "Run summary",
      tags$h4("Key metrics"),
      tags$ul(metric_items),
      tags$hr(),
      tags$h4("Turn-by-turn detail"),
      tableOutput("run_summary_table"),
      easyClose = TRUE,
      size = "l"
    ))

    replay_unlocked(TRUE)
  })
}

shinyApp(ui, server)
