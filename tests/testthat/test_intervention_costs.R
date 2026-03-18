library(testthat)

local_catalog <- intervention_catalog()

test_that("activation and upkeep pricing differ at standard turn budget", {
  activation_details <- compute_intervention_costs(
    selected = c("distancing", "rapid_labs"),
    turn_budget = 2000,
    catalog = local_catalog,
    state = init_intervention_state(local_catalog)
  )

  upkeep_state <- tibble::tibble(
    id = c("distancing", "rapid_labs"),
    label = c("Social distancing", "Rapid labs"),
    active_streak = c(3L, 2L),
    inactive_streak = c(0L, 0L),
    status = c("peak", "sustained")
  )
  upkeep_details <- compute_intervention_costs(
    selected = c("distancing", "rapid_labs"),
    turn_budget = 2000,
    catalog = local_catalog,
    state = upkeep_state
  )

  expect_equal(activation_details$total_cost, 640)
  expect_equal(upkeep_details$total_cost, 340)

  by_id_activation <- stats::setNames(activation_details$by_intervention$cost_final, activation_details$by_intervention$id)
  by_id_upkeep <- stats::setNames(upkeep_details$by_intervention$cost_final, upkeep_details$by_intervention$id)
  expect_equal(unname(by_id_activation[["distancing"]]), 400)
  expect_equal(unname(by_id_activation[["rapid_labs"]]), 240)
  expect_equal(unname(by_id_upkeep[["distancing"]]), 240)
  expect_equal(unname(by_id_upkeep[["rapid_labs"]]), 100)
})

test_that("floor pricing applies at small turn budgets", {
  details <- compute_intervention_costs(
    selected = c("rapid_labs", "community_case_finding"),
    turn_budget = 50,
    catalog = local_catalog,
    state = init_intervention_state(local_catalog)
  )

  by_id_raw <- stats::setNames(details$by_intervention$cost_raw, details$by_intervention$id)
  by_id <- stats::setNames(details$by_intervention$cost_final, details$by_intervention$id)
  expect_true(details$cap_applied)
  expect_equal(unname(by_id_raw[["rapid_labs"]]), 25)
  expect_equal(unname(by_id_raw[["community_case_finding"]]), 20)
  expect_equal(unname(by_id[["rapid_labs"]]), 17)
  expect_equal(unname(by_id[["community_case_finding"]]), 13)
  expect_equal(sum(details$by_intervention$cost_final), 30)
})

test_that("cap logic scales costs with deterministic rounding", {
  synthetic_catalog <- tibble::tibble(
    id = c("a", "b", "c"),
    label = c("A", "B", "C"),
    effect_family = c("acute_rt", "acute_rt", "acute_rt"),
    activation_pct_turn = c(0.34, 0.33, 0.33),
    activation_floor = c(0, 0, 0),
    upkeep_pct_turn = c(0.10, 0.10, 0.10),
    upkeep_floor = c(0, 0, 0),
    warmup_turns = c(0L, 0L, 0L),
    peak_end_turn = c(1L, 1L, 1L),
    reset_after_off_turns = c(1L, 1L, 1L)
  )

  details_a <- compute_intervention_costs(
    selected = c("a", "b", "c"),
    turn_budget = 100,
    catalog = synthetic_catalog,
    state = init_intervention_state(synthetic_catalog),
    max_share = 0.59
  )
  details_b <- compute_intervention_costs(
    selected = c("a", "b", "c"),
    turn_budget = 100,
    catalog = synthetic_catalog,
    state = init_intervention_state(synthetic_catalog),
    max_share = 0.59
  )

  expect_equal(details_a$total_cost_uncapped, 100)
  expect_equal(details_a$total_cost, 59)
  expect_true(details_a$cap_applied)
  expect_equal(details_a$cost_share, 0.59)
  expect_equal(details_a$by_intervention$cost_final, c(20, 20, 19))
  expect_equal(details_a$by_intervention$cost_final, details_b$by_intervention$cost_final)
})

test_that("unknown selections are ignored", {
  details <- compute_intervention_costs(
    selected = c("unknown"),
    turn_budget = 2000,
    catalog = local_catalog,
    state = init_intervention_state(local_catalog)
  )
  effects <- compute_intervention_effects(
    selected = c("distancing", "unknown"),
    catalog = local_catalog,
    state = init_intervention_state(local_catalog),
    turn_budget = 2000
  )

  expect_equal(details$total_cost, 0)
  expect_equal(nrow(details$by_intervention), 0)
  expect_equal(effects$cost, 400)
  expect_equal(effects$Rt_multiplier, 0.75)
})

test_that("intervention labels include start and keep amounts", {
  labels_static <- format_intervention_choice_labels(local_catalog)
  labels_dynamic <- format_intervention_choice_labels(local_catalog, turn_budget = 2000)

  distancing_static <- names(labels_static)[labels_static == "distancing"]
  distancing_dynamic <- names(labels_dynamic)[labels_dynamic == "distancing"]

  expect_length(distancing_static, 1)
  expect_length(distancing_dynamic, 1)
  expect_match(distancing_static, "Social distancing (start £40; keep £24)", fixed = TRUE)
  expect_match(distancing_dynamic, "Social distancing (start £400; keep £240)", fixed = TRUE)
})
