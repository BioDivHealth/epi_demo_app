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
})

test_that("combined interventions accumulate effects at full strength", {
  eff <- compute_intervention_effects(c("distancing", "quarantine"), local_catalog)
  expected_cost <- sum(local_catalog$cost[local_catalog$id %in% c("distancing", "quarantine")])
  expect_equal(eff$cost, expected_cost)
  expect_equal(eff$Rt_multiplier, 0.8)
  expect_equal(eff$imports_multiplier, 0.5)
  expect_equal(sort(eff$labels), sort(c("Social distancing", "Targeted quarantine")))
})

test_that("interventions do nothing during their lag but still cost", {
  streaks <- update_intervention_streaks(integer(), c("distancing", "quarantine"), local_catalog)
  eff <- compute_intervention_effects(c("distancing", "quarantine"), local_catalog, streaks = streaks)
  expect_equal(eff$Rt_multiplier, 1)
  expect_equal(eff$imports_multiplier, 1)
  expect_gt(eff$cost, 0)
})

test_that("distancing fades with fatigue and resets when switched off", {
  dist <- local_catalog[local_catalog$id == "distancing", ]
  streaks <- integer()
  rt <- numeric()
  for (i in 1:4) {
    streaks <- update_intervention_streaks(streaks, "distancing", local_catalog)
    rt[i] <- compute_intervention_effects("distancing", local_catalog, streaks = streaks)$Rt_multiplier
  }

  # lag turn, then full strength, then weaker each turn
  expect_equal(rt[1], 1)
  expect_equal(rt[2], dist$Rt_multiplier)
  expect_gt(rt[3], rt[2])
  expect_gt(rt[4], rt[3])

  streaks <- update_intervention_streaks(streaks, character(), local_catalog)
  expect_equal(get_streak(streaks, "distancing"), 0L)
})
