library(testthat)

local_catalog <- intervention_catalog()

test_that("empty intervention set yields neutral effects", {
  eff <- compute_intervention_effects(character())
  expect_equal(eff$cost, 0)
  expect_equal(eff$Rt_multiplier, 1)
  expect_equal(eff$imports_multiplier, 1)
  expect_equal(eff$delay_adjust$ag, 0)
  expect_equal(eff$delay_adjust$pcr, 0)
  expect_length(eff$labels, 0)
  expect_equal(eff$cost_details$total_cost, 0)
  expect_false(eff$cost_details$cap_applied)
})

test_that("combined interventions accumulate effects with turn budget pricing", {
  eff <- compute_intervention_effects(
    c("distancing", "rapid"),
    local_catalog,
    turn_budget = 2000
  )
  expect_equal(eff$cost, 700)
  expect_equal(eff$Rt_multiplier, 0.8)
  expect_equal(eff$imports_multiplier, 1)
  expect_equal(eff$delay_adjust$pcr, -1)
  expect_equal(sort(eff$labels), sort(c("Social distancing", "Rapid labs")))
  expect_equal(eff$cost_details$total_cost_uncapped, 700)
  expect_false(eff$cost_details$cap_applied)
})

test_that("floor pricing is used when turn budget is not provided", {
  eff <- compute_intervention_effects(c("distancing", "rapid"), local_catalog)

  expect_equal(eff$cost, 70)
  expect_equal(eff$cost_details$total_cost, 70)
  expect_equal(eff$cost_details$total_cost_uncapped, 70)
  expect_true(is.na(eff$cost_details$cost_share))
  expect_false(eff$cost_details$cap_applied)
})
