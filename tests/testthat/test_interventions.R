library(testthat)

local_catalog <- intervention_catalog()

test_that("intervention state normalisation preserves defaults and backfills partial state", {
  default_state <- init_intervention_state(local_catalog)

  expect_equal(
    normalise_intervention_state(default_state, local_catalog),
    default_state
  )

  partial_state <- tibble::tibble(
    id = c("distancing", "unknown"),
    active_streak = c(2L, 9L)
  )
  normalised <- normalise_intervention_state(partial_state, local_catalog)

  distancing_row <- normalised[normalised$id == "distancing", , drop = FALSE]
  tracing_row <- normalised[normalised$id == "case_isolation_tracing", , drop = FALSE]

  expect_equal(distancing_row$active_streak, 2L)
  expect_equal(distancing_row$inactive_streak, 0L)
  expect_equal(distancing_row$status, "off")
  expect_equal(tracing_row$active_streak, 0L)
  expect_equal(tracing_row$inactive_streak, 0L)
  expect_equal(tracing_row$status, "off")
  expect_false("unknown" %in% normalised$id)
})

test_that("empty intervention set yields neutral effects", {
  eff <- compute_intervention_effects(character())

  expect_equal(eff$cost, 0)
  expect_equal(eff$Rt_multiplier, 1)
  expect_equal(eff$imports_multiplier, 1)
  expect_equal(eff$delay_adjust$ag, 0)
  expect_equal(eff$delay_adjust$pcr, 0)
  expect_equal(eff$sampling_yield_multiplier, 1)
  expect_length(eff$labels, 0)
  expect_equal(eff$cost_details$total_cost, 0)
  expect_false(eff$cost_details$cap_applied)
})

test_that("initial selection charges activation costs and only applies non-warmup effects", {
  eff <- compute_intervention_effects(
    selected = c("distancing", "rapid_labs"),
    catalog = local_catalog,
    state = init_intervention_state(local_catalog),
    current_turn = 0L,
    turn_budget = 2000
  )

  expect_equal(eff$cost, 640)
  expect_equal(eff$Rt_multiplier, 0.75)
  expect_equal(eff$delay_adjust$pcr, 0)
  expect_equal(sort(eff$labels), sort(c("Social distancing", "Rapid labs")))
})

test_that("tracing becomes effective after warm-up and recent positives drive its strength", {
  tracing_state <- tibble::tibble(
    id = "case_isolation_tracing",
    label = "Case isolation and tracing",
    active_streak = 1L,
    inactive_streak = 0L,
    status = "warming_up"
  )
  obs_history <- tibble::tibble(
    turn_available = c(0L, 1L, 1L),
    positives = c(4L, 3L, 5L)
  )

  eff <- compute_intervention_effects(
    selected = "case_isolation_tracing",
    catalog = local_catalog,
    state = tracing_state,
    observation_history = obs_history,
    current_turn = 1L,
    turn_budget = 2000
  )

  expect_equal(eff$Rt_multiplier, 0.92)
  expect_equal(eff$cost, 200)
})

test_that("case-finding plateaus at sustained observation yield", {
  case_finding_state <- tibble::tibble(
    id = "community_case_finding",
    label = "Community case-finding",
    active_streak = 4L,
    inactive_streak = 0L,
    status = "peak"
  )

  eff <- compute_intervention_effects(
    selected = "community_case_finding",
    catalog = local_catalog,
    state = case_finding_state,
    current_turn = 6L,
    turn_budget = 1000
  )

  expect_equal(eff$sampling_yield_multiplier, 1.15)
})
