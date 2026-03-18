library(testthat)

if (!exists("replay_branch_names")) {
  source(testthat::test_path("..", "..", "R", "util_replay.R"), local = FALSE)
}

make_branch_payload <- function() {
  player_truth <- tibble::tibble(
    branch = "player",
    turn = c(0L, 0L),
    borough_index = c(1L, 2L),
    gss_code = c("G1", "G2"),
    borough_name = c("A", "B"),
    incidence = c(3L, 1L),
    imports = c(0L, 0L),
    Rt = c(1.2, 1.2)
  )

  baseline_truth <- tibble::tibble(
    branch = "baseline",
    turn = c(0L, 0L, 1L, 1L),
    borough_index = c(1L, 2L, 1L, 2L),
    gss_code = c("G1", "G2", "G1", "G2"),
    borough_name = c("A", "B", "A", "B"),
    incidence = c(2L, 1L, 2L, 2L),
    imports = c(0L, 0L, 0L, 0L),
    Rt = c(1.1, 1.1, 1.1, 1.1)
  )

  obs_player <- tibble::tibble(
    branch = "player",
    turn_reported = 1L,
    turn_sampled = 0L,
    turn_available = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_returned = 10L,
    positives = 3L,
    positivity = 0.3
  )

  obs_baseline <- tibble::tibble(
    branch = "baseline",
    turn_reported = 1L,
    turn_sampled = 0L,
    turn_available = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_returned = 8L,
    positives = 1L,
    positivity = 0.125
  )

  actions_player <- tibble::tibble(
    branch = "player",
    turn = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_allocated = 10L,
    spend = 20,
    cost_per_test = 2,
    allocation_spend = 20,
    intervention_cost = 0,
    total_turn_spend = 20,
    cumulative_spend = 20,
    budget_remaining = 1980,
    interventions_active = "None"
  )
  run_log_player <- tibble::tibble(
    turn = c(0L, 1L),
    active_interventions = c("None", "Social distancing"),
    intervention_cost = c(0, 50)
  )

  actions_baseline <- tibble::tibble(
    branch = "baseline",
    turn = 1L,
    borough_index = 1L,
    gss_code = "G1",
    borough_name = "A",
    test_type = "ag",
    n_allocated = 5L,
    spend = 10,
    cost_per_test = 2,
    allocation_spend = 10,
    intervention_cost = 0,
    total_turn_spend = 10,
    cumulative_spend = 10,
    budget_remaining = 1990,
    interventions_active = "None"
  )
  run_log_baseline <- tibble::tibble(
    turn = c(0L, 1L),
    active_interventions = c("None", "None"),
    intervention_cost = c(0, 0)
  )

  list(
    branches = list(
      player = list(
        truth = player_truth,
        observations = obs_player,
        actions = actions_player,
        run_log = run_log_player,
        truth_history = list(c(3L, 1L), c(4L, 2L))
      ),
      baseline = list(
        truth = baseline_truth,
        observations = obs_baseline,
        actions = actions_baseline,
        run_log = run_log_baseline,
        truth_history = list(c(2L, 1L), c(2L, 2L))
      )
    )
  )
}

make_legacy_payload <- function() {
  list(
    truth = tibble::tibble(
      turn = c(0L, 0L),
      borough_index = c(1L, 2L),
      gss_code = c("G1", "G2"),
      borough_name = c("A", "B"),
      incidence = c(1L, 0L),
      imports = c(0L, 0L),
      Rt = c(1.2, 1.2)
    ),
    observations = tibble::tibble(
      turn_reported = 1L,
      turn_sampled = 0L,
      turn_available = 1L,
      borough_index = 1L,
      gss_code = "G1",
      borough_name = "A",
      test_type = "ag",
      n_returned = 10L,
      positives = 2L,
      positivity = 0.2
    ),
    actions = tibble::tibble(
      turn = 1L,
      borough_index = 1L,
      gss_code = "G1",
      borough_name = "A",
      test_type = "ag",
      n_allocated = 10L,
      spend = 20,
      cost_per_test = 2,
      allocation_spend = 20,
      intervention_cost = 0,
      total_turn_spend = 20,
      cumulative_spend = 20,
      budget_remaining = 1980,
      interventions_active = "None"
    ),
    truth_history = list(c(1L, 0L), c(2L, 1L))
  )
}

test_that("replay branch names detect branch payload and legacy payload", {
  expect_equal(replay_branch_names(make_branch_payload()), c("player", "baseline"))
  expect_equal(replay_branch_names(make_legacy_payload()), c("player"))
})

test_that("replay_get_branch_logs returns selected branch and falls back for missing branch", {
  payload <- make_branch_payload()

  baseline_logs <- replay_get_branch_logs(payload, branch = "baseline")
  expect_equal(nrow(baseline_logs$truth), 4)
  expect_equal(sum(baseline_logs$truth$incidence), 7)

  fallback_logs <- replay_get_branch_logs(payload, branch = "nonexistent")
  expect_equal(nrow(fallback_logs$truth), 2)
  expect_equal(sum(fallback_logs$truth$incidence), 4)

  legacy_logs <- replay_get_branch_logs(make_legacy_payload(), branch = "player")
  expect_true(is.list(legacy_logs$truth_history))
  expect_equal(nrow(legacy_logs$observations), 1)
  expect_true("run_log" %in% names(baseline_logs))
  expect_equal(nrow(baseline_logs$run_log), 2)
  expect_equal(nrow(legacy_logs$run_log), 0)
})

test_that("replay_filter_branch filters when branch column exists and no-ops otherwise", {
  with_branch <- tibble::tibble(branch = c("player", "baseline"), value = c(1, 2))
  without_branch <- tibble::tibble(value = c(1, 2))

  filtered <- replay_filter_branch(with_branch, "player")
  expect_equal(nrow(filtered), 1)
  expect_equal(filtered$value[[1]], 1)

  unfiltered <- replay_filter_branch(without_branch, "baseline")
  expect_equal(nrow(unfiltered), 2)
})

test_that("replay_resolve_truth_turns and replay_resolve_truth_log are branch-specific", {
  payload <- make_branch_payload()
  borough_names <- c("A", "B")

  turns_player <- replay_resolve_truth_turns(payload, branch = "player")
  turns_baseline <- replay_resolve_truth_turns(payload, branch = "baseline")

  expect_equal(turns_player, c(0L, 1L))
  expect_equal(turns_baseline, c(0L, 1L))

  truth_player <- replay_resolve_truth_log(payload, borough_names, branch = "player")
  truth_baseline <- replay_resolve_truth_log(payload, borough_names, branch = "baseline")

  expect_equal(sum(truth_player$incidence), 10)
  expect_equal(sum(truth_baseline$incidence), 7)
})

test_that("replay observation/action summaries respect branch selection", {
  payload <- make_branch_payload()
  merged_obs <- dplyr::bind_rows(
    payload$branches$player$observations,
    payload$branches$baseline$observations
  )
  merged_actions <- dplyr::bind_rows(
    payload$branches$player$actions,
    payload$branches$baseline$actions
  )

  obs_player <- replay_summarise_observed(merged_obs, target_turn = 1L, branch = "player")
  obs_baseline <- replay_summarise_observed(merged_obs, target_turn = 1L, branch = "baseline")

  expect_equal(sum(obs_player$tests), 10)
  expect_equal(sum(obs_baseline$tests), 8)

  actions_player <- replay_summarise_actions(merged_actions, target_turn = 1L, branch = "player")
  actions_baseline <- replay_summarise_actions(merged_actions, target_turn = 1L, branch = "baseline")

  expect_equal(sum(actions_player$tests_ag), 10)
  expect_equal(sum(actions_baseline$tests_ag), 5)
})

test_that("intervention timeline builds full turn grid from run log labels", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs")
  )
  run_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    active_interventions = c("None", "Social distancing, Targeted quarantine", "Rapid labs")
  )

  timeline <- replay_build_intervention_timeline(
    run_log = run_log,
    turns = 0:2,
    intervention_defs = defs
  )

  expect_equal(nrow(timeline), 3L * nrow(defs))
  active_t1 <- timeline |>
    dplyr::filter(turn == 1L, active) |>
    dplyr::pull(intervention_label) |>
    sort()
  expect_equal(active_t1, sort(c("Social distancing", "Targeted quarantine")))
})

test_that("intervention timeline falls back to action log labels when run log missing", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 0L, 1L),
    interventions_active = c("None", "None", "Rapid labs")
  )

  timeline <- replay_build_intervention_timeline(
    run_log = tibble::tibble(),
    turns = 0:1,
    intervention_defs = defs,
    action_log = action_log
  )

  rapid_active_t1 <- timeline |>
    dplyr::filter(turn == 1L, intervention_id == "rapid") |>
    dplyr::pull(active)
  expect_true(isTRUE(rapid_active_t1))
})

test_that("intervention timeline plot returns ggplot and includes fallback-derived active state", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine", "rapid"),
    label = c("Social distancing", "Targeted quarantine", "Rapid labs")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 1L),
    interventions_active = c("None", "Rapid labs")
  )

  plot_obj <- replay_plot_intervention_timeline(
    run_log = tibble::tibble(),
    action_log = action_log,
    intervention_defs = defs,
    turns = 0:1
  )

  expect_s3_class(plot_obj, "ggplot")
  rapid_state <- plot_obj$data |>
    dplyr::filter(turn == 1L, intervention_id == "rapid") |>
    dplyr::pull(active)
  expect_true(isTRUE(rapid_state))
  expect_equal(sort(unique(plot_obj$data$strip_state)), c("Active", "Inactive"))
})

test_that("intervention timeline plot returns NULL when no turns are available", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine"),
    label = c("Social distancing", "Targeted quarantine")
  )

  expect_null(replay_plot_intervention_timeline(
    run_log = tibble::tibble(),
    action_log = tibble::tibble(),
    intervention_defs = defs,
    turns = integer()
  ))
})

test_that("turn comparison normalization returns stable columns and types", {
  raw_comparison <- tibble::tibble(
    turn = c("0", "1"),
    player_truth_total = c("10", "12"),
    baseline_truth_total = c(15, 18)
  )

  normalized <- replay_normalize_turn_comparison(raw_comparison)

  expect_equal(
    names(normalized),
    c(
      "turn",
      "player_truth_total",
      "baseline_truth_total",
      "player_deaths_total",
      "baseline_deaths_total",
      "player_observed_positives",
      "baseline_observed_positives",
      "player_total_turn_spend",
      "baseline_total_turn_spend"
    )
  )
  expect_type(normalized$turn, "integer")
  expect_type(normalized$player_truth_total, "double")
  expect_true(all(is.na(normalized$player_deaths_total)))
})

test_that("intervention markers detect starts and mix changes from run log", {
  defs <- tibble::tibble(
    id = c("distancing", "rapid", "community"),
    label = c("Social distancing", "Rapid labs", "Community case-finding")
  )
  run_log <- tibble::tibble(
    turn = c(0L, 1L, 2L, 3L),
    active_interventions = c(
      "None",
      "Social distancing",
      "Social distancing, Rapid labs",
      "Rapid labs"
    )
  )

  markers <- replay_extract_intervention_markers(
    run_log = run_log,
    action_log = tibble::tibble(),
    intervention_defs = defs
  )

  expect_equal(markers$turn, c(1L, 2L, 3L))
  expect_equal(
    markers$marker_label,
    c("Intervention started", "Intervention started", "Intervention mix changed")
  )
  expect_match(markers$marker_detail[[1]], "Social distancing activated")
  expect_match(markers$marker_detail[[2]], "Rapid labs activated")
  expect_match(markers$marker_detail[[3]], "stopped Social distancing")
})

test_that("intervention markers fall back to action log labels when run log is missing", {
  defs <- tibble::tibble(
    id = c("distancing", "rapid"),
    label = c("Social distancing", "Rapid labs")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 1L),
    interventions_active = c("None", "Rapid labs")
  )

  markers <- replay_extract_intervention_markers(
    run_log = tibble::tibble(),
    action_log = action_log,
    intervention_defs = defs
  )

  expect_equal(nrow(markers), 1)
  expect_identical(markers$turn[[1]], 1L)
  expect_identical(markers$marker_label[[1]], "Intervention started")
  expect_match(markers$marker_detail[[1]], "Rapid labs activated")
})

test_that("divergence markers require sustained separation", {
  turn_comparison <- tibble::tibble(
    turn = 0:4,
    player_truth_total = c(100, 97, 60, 55, 50),
    baseline_truth_total = c(100, 100, 100, 100, 100),
    player_deaths_total = c(0, 0, 0, 0, 0),
    baseline_deaths_total = c(0, 0, 0, 0, 0)
  )

  markers <- replay_extract_divergence_markers(turn_comparison)

  expect_equal(nrow(markers), 1)
  expect_identical(markers$turn[[1]], 2L)
  expect_identical(markers$marker_label[[1]], "Branches diverge")
  expect_match(markers$marker_detail[[1]], "Player truth starts to fall below baseline")
})

test_that("decision markers deduplicate same-turn events and cap at six", {
  defs <- tibble::tibble(
    id = c("distancing", "rapid", "community", "ventilation", "isolation", "labs", "finding"),
    label = c(
      "Social distancing",
      "Rapid labs",
      "Community case-finding",
      "Ventilation guidance",
      "Case isolation",
      "Extra labs",
      "Targeted finding"
    )
  )
  run_log <- tibble::tibble(
    turn = 0:7,
    active_interventions = c(
      "None",
      "Social distancing",
      "Social distancing, Rapid labs",
      "Social distancing, Rapid labs, Community case-finding",
      "Social distancing, Rapid labs, Community case-finding, Ventilation guidance",
      "Social distancing, Rapid labs, Community case-finding, Ventilation guidance, Case isolation",
      "Social distancing, Rapid labs, Community case-finding, Ventilation guidance, Case isolation, Extra labs",
      "Social distancing, Rapid labs, Community case-finding, Ventilation guidance, Case isolation, Extra labs, Targeted finding"
    )
  )
  turn_comparison <- tibble::tibble(
    turn = 0:7,
    player_truth_total = c(100, 98, 96, 55, 50, 48, 45, 42),
    baseline_truth_total = c(100, 100, 100, 100, 100, 100, 100, 100),
    player_deaths_total = c(0, 0, 0, 0, 0, 0, 0, 0),
    baseline_deaths_total = c(0, 0, 0, 0, 0, 0, 0, 0)
  )

  markers <- replay_build_decision_markers(
    run_log = run_log,
    action_log = tibble::tibble(),
    turn_comparison = turn_comparison,
    intervention_defs = defs
  )

  expect_lte(nrow(markers), 6)
  expect_lte(sum(markers$turn == 3L), 1)
})

test_that("turn notes and formatted comparison rows use selected-turn deltas", {
  turn_comparison <- tibble::tibble(
    turn = c(2L, 3L),
    player_truth_total = c(60, 55),
    baseline_truth_total = c(100, 100),
    player_deaths_total = c(0, 0),
    baseline_deaths_total = c(1, 1),
    player_observed_positives = c(4, 5),
    baseline_observed_positives = c(1, 1),
    player_total_turn_spend = c(200, 220),
    baseline_total_turn_spend = c(20, 20)
  )
  markers <- tibble::tibble(
    turn = 2L,
    marker_type = "divergence",
    marker_label = "Branches diverge",
    marker_detail = "Player truth starts to fall below baseline",
    priority = 30L,
    chip_class = "decision-marker--divergence"
  )

  formatted <- replay_format_turn_comparison_row(turn_comparison, target_turn = 2L)
  note_text <- replay_build_turn_note(
    selected_turn = 2L,
    decision_markers = markers,
    comparison_row = formatted
  )

  expect_true(all(c("case_gap_vs_baseline", "death_gap_vs_baseline") %in% names(formatted)))
  expect_identical(formatted$case_gap_vs_baseline[[1]], 40)
  expect_identical(formatted$death_gap_vs_baseline[[1]], 1)
  expect_match(note_text, "Decision moment")
  expect_match(note_text, "Case gap vs baseline: 40")
  expect_null(replay_build_turn_note(1L, markers, formatted))
})

test_that("prevalence plot no longer carries intervention fill scale", {
  defs <- tibble::tibble(
    id = c("distancing", "quarantine"),
    label = c("Social distancing", "Targeted quarantine")
  )
  truth_log <- tibble::tibble(
    turn = c(0L, 1L),
    borough_index = c(1L, 1L),
    incidence = c(4L, 6L)
  )
  baseline_truth_log <- tibble::tibble(
    turn = c(0L, 1L),
    borough_index = c(1L, 1L),
    incidence = c(3L, 5L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(0L, 1L),
    n_returned = c(20L, 20L),
    positives = c(1L, 2L)
  )
  run_log <- tibble::tibble(
    turn = c(0L, 1L),
    active_interventions = c("None", "Social distancing")
  )
  action_log <- tibble::tibble(
    turn = c(0L, 1L),
    interventions_active = c("None", "Social distancing")
  )

  plot_obj <- replay_plot_prevalence(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000),
    window = 2L,
    baseline_truth_log = baseline_truth_log,
    player_run_log = run_log,
    player_action_log = action_log,
    intervention_defs = defs
  )

  expect_s3_class(plot_obj, "ggplot")
  expect_null(plot_obj$scales$get_scales("fill"))
  expect_equal(plot_obj$labels$y, "Prevalence (%)")
})

test_that("prevalence plot remains prevalence-only with no secondary axis", {
  truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(3L, 5L, 2L)
  )
  baseline_truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(2L, 3L, 1L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(0L, 1L, 2L),
    n_returned = c(20L, 25L, 30L),
    positives = c(1L, 2L, 1L)
  )

  plot_obj <- replay_plot_prevalence(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000),
    window = 2L,
    baseline_truth_log = baseline_truth_log
  )

  expect_s3_class(plot_obj, "ggplot")
  y_scale <- plot_obj$scales$get_scales("y")
  if (!is.null(y_scale)) {
    expect_true(inherits(y_scale$secondary.axis, "waiver"))
  }

  layer_param <- function(layer, name, default = NULL) {
    value <- layer$aes_params[[name]]
    if (is.null(value)) default else value
  }

  expect_equal(length(plot_obj$layers), 3)
  player_style <- plot_obj$layers[[1]]
  observed_style <- plot_obj$layers[[2]]
  baseline_style <- plot_obj$layers[[3]]

  expect_equal(layer_param(player_style, "linewidth", NA_real_), 1.4)
  expect_equal(layer_param(player_style, "linetype", NA_character_), "solid")
  expect_equal(layer_param(player_style, "alpha", NA_real_), 1)
  expect_equal(layer_param(observed_style, "linewidth", NA_real_), 0.8)
  expect_equal(layer_param(observed_style, "linetype", NA_character_), "dashed")
  expect_equal(layer_param(observed_style, "alpha", NA_real_), 0.7)
  expect_equal(layer_param(baseline_style, "linewidth", NA_real_), 0.9)
  expect_equal(layer_param(baseline_style, "linetype", NA_character_), "longdash")
  expect_equal(layer_param(baseline_style, "alpha", NA_real_), 0.85)
})

test_that("cumulative plot uses ribbon, step, and direct labels on a linear scale", {
  truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(3L, 5L, 2L)
  )
  baseline_truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(2L, 3L, 1L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(0L, 1L, 2L),
    n_returned = c(20L, 25L, 30L),
    positives = c(1L, 2L, 1L)
  )

  plot_obj <- replay_plot_cumulative(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000),
    window = 2L,
    baseline_truth_log = baseline_truth_log,
    cumulative_series = c("observed", "player_truth", "baseline_truth")
  )

  expect_s3_class(plot_obj, "ggplot")
  expect_equal(plot_obj$labels$y, "Cumulative cases")
  geom_classes <- vapply(plot_obj$layers, function(layer) class(layer$geom)[1], character(1))
  expect_true(any(grepl("GeomRibbon", geom_classes)))
  expect_true(any(grepl("GeomStep", geom_classes)))
  expect_true(any(grepl("GeomLine", geom_classes)))
  expect_true(any(grepl("GeomPoint", geom_classes)))
  expect_true(any(grepl("GeomLabel", geom_classes)))

  y_scale <- plot_obj$scales$get_scales("y")
  trans_name <- ""
  if (!is.null(y_scale) && !is.null(y_scale$trans) && !is.null(y_scale$trans$name)) {
    trans_name <- y_scale$trans$name
  }
  expect_false(grepl("pseudo_log", trans_name, fixed = TRUE))

  ribbon_layer <- plot_obj$layers[[which(geom_classes == "GeomRibbon")[1]]]
  step_layer <- plot_obj$layers[[which(geom_classes == "GeomStep")[1]]]
  line_layers <- plot_obj$layers[geom_classes == "GeomLine"]

  expect_equal(ribbon_layer$aes_params$alpha, 0.24)
  expect_equal(step_layer$aes_params$linewidth, 0.8)
  expect_equal(step_layer$aes_params$linetype, "dashed")
  expect_equal(step_layer$aes_params$alpha, 0.7)
  expect_true(any(vapply(line_layers, function(layer) identical(layer$aes_params$linetype, "solid"), logical(1))))
  expect_true(any(vapply(line_layers, function(layer) identical(layer$aes_params$linetype, "longdash"), logical(1))))
})

test_that("cumulative plot omits baseline cumulative when baseline truth missing", {
  truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(3L, 5L, 2L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(0L, 1L, 2L),
    n_returned = c(20L, 25L, 30L),
    positives = c(1L, 2L, 1L)
  )

  plot_obj <- replay_plot_cumulative(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000),
    window = 2L,
    baseline_truth_log = tibble::tibble(),
    cumulative_series = c("observed", "player_truth", "baseline_truth")
  )

  geom_classes <- vapply(plot_obj$layers, function(layer) class(layer$geom)[1], character(1))
  expect_false(any(grepl("GeomRibbon", geom_classes)))

  built_plot <- ggplot2::ggplot_build(plot_obj)
  colours_seen <- unique(unlist(lapply(built_plot$data, function(layer_df) {
    if (!"colour" %in% names(layer_df)) return(character())
    as.character(layer_df$colour)
  }), use.names = FALSE))
  expect_false("#31a354" %in% colours_seen)
})

test_that("replay metric panels share the same x-axis expansion", {
  truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(3L, 5L, 2L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(0L, 1L, 2L),
    n_returned = c(20L, 25L, 30L),
    positives = c(1L, 2L, 1L)
  )
  defs <- tibble::tibble(
    id = c("distancing"),
    label = c("Social distancing")
  )
  run_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    active_interventions = c("None", "Social distancing", "Social distancing")
  )

  prevalence_plot <- replay_plot_prevalence(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000)
  )
  cumulative_plot <- replay_plot_cumulative(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000)
  )
  intervention_plot <- replay_plot_intervention_timeline(
    run_log = run_log,
    action_log = tibble::tibble(),
    intervention_defs = defs,
    turns = 0:2
  )

  expect_equal(prevalence_plot$scales$get_scales("x")$expand, c(0.01, 0, 0.07, 0))
  expect_equal(cumulative_plot$scales$get_scales("x")$expand, c(0.01, 0, 0.07, 0))
  expect_equal(intervention_plot$scales$get_scales("x")$expand, c(0.01, 0, 0.07, 0))
})

test_that("prevalence prep aligns turns across truth, observed, and baseline logs", {
  truth_log <- tibble::tibble(
    turn = c(0L, 1L, 2L),
    borough_index = c(1L, 1L, 1L),
    incidence = c(3L, 4L, 5L)
  )
  baseline_truth_log <- tibble::tibble(
    turn = c(1L, 3L),
    borough_index = c(1L, 1L),
    incidence = c(2L, 6L)
  )
  obs_log <- tibble::tibble(
    turn_reported = c(1L, 3L),
    n_returned = c(20L, 30L),
    positives = c(2L, 3L)
  )

  prep <- replay_prepare_prevalence_series(
    truth_log = truth_log,
    obs_log = obs_log,
    pop_tbl = tibble::tibble(pop = 1000),
    window = 2L,
    baseline_truth_log = baseline_truth_log
  )

  expect_false(is.null(prep))
  expect_true(isTRUE(prep$has_baseline_truth))
  expect_equal(prep$series$turn, 0:3)
  expect_true(all(diff(prep$series$observed_cumulative) >= 0))
  expect_true(all(diff(prep$series$player_truth_cumulative) >= 0))
  expect_true(all(diff(prep$series$baseline_truth_cumulative) >= 0))
})
