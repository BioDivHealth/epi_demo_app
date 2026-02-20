library(testthat)

local_catalog <- intervention_catalog()

test_that("share pricing converts correctly at standard turn budget", {
  details <- compute_intervention_costs(
    selected = c("distancing", "rapid"),
    turn_budget = 2000,
    catalog = local_catalog
  )

  expect_equal(details$total_cost_uncapped, 700)
  expect_equal(details$total_cost, 700)
  expect_false(details$cap_applied)
  expect_equal(details$cost_share, 700 / 2000)

  by_id <- stats::setNames(details$by_intervention$cost_final, details$by_intervention$id)
  expect_equal(unname(by_id[["distancing"]]), 500)
  expect_equal(unname(by_id[["rapid"]]), 200)
})

test_that("floor pricing applies at small turn budgets", {
  rapid <- compute_intervention_costs(
    selected = "rapid",
    turn_budget = 100,
    catalog = local_catalog
  )
  distancing <- compute_intervention_costs(
    selected = "distancing",
    turn_budget = 50,
    catalog = local_catalog
  )

  expect_equal(rapid$total_cost, 40)
  expect_equal(rapid$total_cost_uncapped, 40)
  expect_equal(distancing$total_cost, 30)
  expect_equal(distancing$total_cost_uncapped, 30)
})

test_that("cap logic scales costs with deterministic rounding", {
  synthetic_catalog <- tibble::tibble(
    id = c("a", "b", "c"),
    label = c("A", "B", "C"),
    cost_pct_turn = c(0.34, 0.33, 0.33),
    cost_floor = c(0, 0, 0),
    Rt_multiplier = c(1, 1, 1),
    imports_multiplier = c(1, 1, 1),
    ag_delay_adjust = c(0, 0, 0),
    pcr_delay_adjust = c(0, 0, 0)
  )

  details_a <- compute_intervention_costs(
    selected = c("a", "b", "c"),
    turn_budget = 100,
    catalog = synthetic_catalog,
    max_share = 0.59
  )
  details_b <- compute_intervention_costs(
    selected = c("a", "b", "c"),
    turn_budget = 100,
    catalog = synthetic_catalog,
    max_share = 0.59
  )

  expect_equal(details_a$total_cost_uncapped, 100)
  expect_equal(details_a$total_cost, 59)
  expect_true(details_a$cap_applied)
  expect_equal(details_a$cost_share, 0.59)
  expect_equal(details_a$by_intervention$cost_final, c(20L, 20L, 19L))
  expect_equal(details_a$by_intervention$cost_final, details_b$by_intervention$cost_final)
})

test_that("unknown selections are ignored", {
  details <- compute_intervention_costs(
    selected = c("unknown"),
    turn_budget = 2000,
    catalog = local_catalog
  )
  effects <- compute_intervention_effects(
    selected = c("distancing", "unknown"),
    catalog = local_catalog,
    turn_budget = 2000
  )

  expect_equal(details$total_cost, 0)
  expect_equal(nrow(details$by_intervention), 0)
  expect_equal(effects$cost, 500)
  expect_equal(effects$Rt_multiplier, 0.8)
})

test_that("intervention labels include share/floor and optional current amount", {
  labels_static <- format_intervention_choice_labels(local_catalog)
  labels_dynamic <- format_intervention_choice_labels(local_catalog, turn_budget = 2000)

  distancing_static <- names(labels_static)[labels_static == "distancing"]
  distancing_dynamic <- names(labels_dynamic)[labels_dynamic == "distancing"]

  expect_length(distancing_static, 1)
  expect_length(distancing_dynamic, 1)
  expect_match(distancing_static, "25%/turn, min £30", fixed = TRUE)
  expect_match(distancing_dynamic, "25%/turn, min £30, now ~£500", fixed = TRUE)
})
