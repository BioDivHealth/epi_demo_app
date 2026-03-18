library(testthat)
library(tibble)

test_that("starter-intel-only history yields starter-only signal state", {
  history <- tibble(
    borough_index = 4L,
    n = 200L,
    turn_available = 0L,
    source = "starter_intel"
  )

  state <- compute_signal_quality_state(
    observed_history = history,
    queue_state = tibble(n = integer()),
    current_turn = 0L,
    total_boroughs = 33L
  )

  expect_identical(state$state, "starter_only")
  expect_identical(state$headline, "Starter alert only")
  expect_match(state$helper_text, "Wait for your own returned results")
  expect_null(state$rows)
})

test_that("rolling-window signal quality returns fair status and row hints", {
  history <- tibble(
    borough_index = c(1L, 2L, 3L, 4L, 5L, 6L),
    n = c(25L, 25L, 25L, 25L, 25L, 25L),
    turn_available = c(3L, 3L, 3L, 4L, 4L, 4L),
    source = rep("player_turn", 6)
  )

  state <- compute_signal_quality_state(
    observed_history = history,
    queue_state = tibble(n = c(60L, 60L)),
    current_turn = 5L,
    total_boroughs = 33L
  )

  expect_identical(state$state, "normal")
  expect_identical(state$headline, "Fair")
  expect_equal(state$metrics$signal_quality_points, 8L)

  row_keys <- vapply(state$rows, function(x) x$key, character(1))
  rows <- stats::setNames(state$rows, row_keys)

  expect_identical(rows$coverage$status, "Fair")
  expect_identical(rows$coverage$value_text, "6/33 boroughs observed over last 3 completed turns")
  expect_true(nzchar(rows$coverage$hint))
  expect_identical(rows$freshness$status, "Fair")
  expect_identical(rows$confidence$status, "Fair")
  expect_identical(rows$continuity$status, "Fair")
})

test_that("continuity is anchored to the last completed turn, not the empty current turn", {
  history <- tibble(
    borough_index = c(1L, 2L, 3L, 4L, 5L, 6L),
    n = c(30L, 30L, 30L, 30L, 30L, 30L),
    turn_available = c(12L, 12L, 13L, 13L, 14L, 14L),
    source = rep("player_turn", 6)
  )

  state <- compute_signal_quality_state(
    observed_history = history,
    queue_state = tibble(n = integer()),
    current_turn = 15L,
    total_boroughs = 33L
  )

  row_keys <- vapply(state$rows, function(x) x$key, character(1))
  rows <- stats::setNames(state$rows, row_keys)

  expect_identical(state$metrics$signal_turn, 14L)
  expect_identical(rows$continuity$status, "Strong")
  expect_match(rows$continuity$value_text, "3 of last 3 completed turns")
})

test_that("missing source is treated as player data and stale windows fall to no signal", {
  history <- tibble(
    borough_index = 1L,
    n = 40L,
    turn_available = 1L
  )

  state <- compute_signal_quality_state(
    observed_history = history,
    queue_state = tibble(n = integer()),
    current_turn = 5L,
    total_boroughs = 33L
  )

  expect_identical(state$state, "none")
  expect_identical(state$headline, "No signal yet")
  expect_match(state$helper_text, "No recent returned results")
})

test_that("batch signal summary is hidden outside guided auto", {
  summary <- compute_batch_signal_summary(
    observed_history = tibble(),
    queue_state = tibble(n = integer()),
    current_turn = 0L,
    total_boroughs = 33L,
    run_status = list(mode = "manual", turns_requested = 12L, turns_completed = 0L)
  )

  expect_false(summary$visible)
  expect_identical(summary$state, "hidden")
})

test_that("guided auto shows waiting batch summary before completed turns", {
  summary <- compute_batch_signal_summary(
    observed_history = tibble(
      borough_index = 4L,
      n = 200L,
      turn_available = 0L,
      source = "starter_intel"
    ),
    queue_state = tibble(n = c(30L, 10L)),
    current_turn = 0L,
    total_boroughs = 33L,
    run_status = list(mode = "guided_auto", turns_requested = 12L, turns_completed = 0L)
  )

  expect_true(summary$visible)
  expect_identical(summary$state, "waiting")
  expect_identical(summary$summary_text, "No completed auto turns yet")
  expect_identical(summary$metrics$pending_tests, 40L)
})

test_that("guided auto batch summary uses only completed turns in the current block", {
  history <- tibble(
    borough_index = c(1L, 2L, 3L, 4L, 5L, 6L, 7L),
    n = c(200L, 50L, 40L, 30L, 25L, 20L, 15L),
    turn_available = c(0L, 9L, 10L, 11L, 12L, 13L, 14L),
    source = c("starter_intel", rep("player_turn", 6))
  )

  summary <- compute_batch_signal_summary(
    observed_history = history,
    queue_state = tibble(n = c(18L, 12L)),
    current_turn = 15L,
    total_boroughs = 33L,
    run_status = list(mode = "guided_auto", turns_requested = 5L, turns_completed = 3L)
  )

  expect_true(summary$visible)
  expect_identical(summary$state, "summary")
  expect_identical(summary$metrics$signal_turn, 14L)
  expect_identical(summary$metrics$batch_start, 12L)
  expect_identical(summary$metrics$batch_turns_used, 3L)
  expect_identical(summary$metrics$coverage_n, 3L)
  expect_identical(summary$metrics$returned_tests, 60L)
  expect_identical(summary$metrics$pending_tests, 30L)
  expect_identical(
    summary$summary_text,
    "3/5 turns | 3/33 boroughs covered | 60 returns | 30 pending"
  )
})

test_that("guided auto batch summary can use the full configured block", {
  history <- tibble(
    borough_index = c(1L, 2L, 3L, 4L, 5L),
    n = c(20L, 25L, 30L, 35L, 40L),
    turn_available = c(5L, 6L, 7L, 8L, 9L),
    source = rep("player_turn", 5)
  )

  summary <- compute_batch_signal_summary(
    observed_history = history,
    queue_state = tibble(n = integer()),
    current_turn = 10L,
    total_boroughs = 33L,
    run_status = list(mode = "guided_auto", turns_requested = 5L, turns_completed = 5L)
  )

  expect_identical(summary$metrics$batch_start, 5L)
  expect_identical(summary$metrics$batch_turns_used, 5L)
  expect_identical(summary$metrics$coverage_n, 5L)
  expect_identical(summary$metrics$returned_tests, 150L)
})

test_that("planned turn mix labels cover no-plan, surveillance, balanced, and control-heavy states", {
  catalog <- intervention_catalog()
  state <- init_intervention_state(catalog)
  budget <- list(budget = 2000, costs = list(ag = 2, pcr = 6))
  small_budget <- list(budget = 1000, costs = list(ag = 2, pcr = 6))
  allocations <- tibble(
    borough_index = c(1L, 2L),
    borough_name = c("Barnet", "Camden"),
    n_ag = c(0L, 0L),
    n_pcr = c(0L, 0L)
  )

  mix_none <- compute_planned_turn_mix(
    allocations = allocations,
    selected_interventions = character(),
    budget = budget,
    intervention_defs = catalog,
    intervention_state = state,
    observed_this_turn = tibble()
  )
  expect_identical(mix_none$label, "No turn plan yet")
  expect_equal(mix_none$testing_share + mix_none$intervention_share + mix_none$leftover_share, 1)

  alloc_surveillance <- allocations
  alloc_surveillance$n_ag[1] <- 50L
  mix_surveillance <- compute_planned_turn_mix(
    allocations = alloc_surveillance,
    selected_interventions = character(),
    budget = budget,
    intervention_defs = catalog,
    intervention_state = state,
    observed_this_turn = tibble()
  )
  expect_identical(mix_surveillance$label, "Surveillance-heavy this turn")

  mix_balanced <- compute_planned_turn_mix(
    allocations = allocations,
    selected_interventions = "distancing",
    budget = budget,
    intervention_defs = catalog,
    intervention_state = state,
    observed_this_turn = tibble()
  )
  expect_identical(mix_balanced$label, "Balanced mix")

  mix_control <- compute_planned_turn_mix(
    allocations = allocations,
    selected_interventions = catalog$id,
    budget = small_budget,
    intervention_defs = catalog,
    intervention_state = state,
    observed_this_turn = tibble()
  )
  expect_identical(mix_control$label, "Control-heavy this turn")
  expect_gt(mix_control$intervention_share, 0.45)
})
