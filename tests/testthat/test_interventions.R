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

test_that("distancing at full strength halves spread and costs its price", {
  eff <- compute_intervention_effects("distancing", local_catalog)
  expect_equal(eff$cost, local_catalog$cost[local_catalog$id == "distancing"])
  expect_equal(eff$Rt_multiplier, 0.5)
  expect_equal(eff$imports_multiplier, 1)
  expect_equal(eff$labels, "Social distancing")
})

test_that("interventions do nothing during their lag but still cost", {
  streaks <- update_intervention_streaks(integer(), "distancing", local_catalog)
  eff <- compute_intervention_effects("distancing", local_catalog, streaks = streaks)
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

# Contact tracing follows up hospital-reported cases too -------------------------

test_that("tracing follows up reported admissions, worth more for a slow disease", {
  tp <- teaching_params()
  admissions <- c(10, 0, 0)

  slow <- list(delay_decay = 0.85, hosp_delay = 2)   # Ebola-like
  fast <- list(delay_decay = 0.35, hosp_delay = 1)   # flu-like

  off <- isolated_from_results(NULL, 3, tp, tracing = FALSE, bio = slow, admissions = admissions)
  slow_on <- isolated_from_results(NULL, 3, tp, tracing = TRUE, Rt = 1.2, bio = slow,
                                   gens_per_turn = 7 / 15, admissions = admissions)
  fast_on <- isolated_from_results(NULL, 3, tp, tracing = TRUE, Rt = 1.2, bio = fast,
                                   gens_per_turn = 7 / 3, admissions = admissions)

  expect_equal(sum(off), 0)                 # no tracing, no follow-up
  expect_gt(slow_on[1], 0)
  expect_equal(slow_on[2:3], c(0, 0))       # only where the admissions were
  expect_gt(slow_on[1], 5 * fast_on[1])     # a fast disease has moved on
})

test_that("tracers are swamped when cases outnumber their capacity", {
  tp <- teaching_params()
  bio <- list(delay_decay = 0.85, hosp_delay = 2)
  few <- isolated_from_results(NULL, 1, tp, tracing = TRUE, bio = bio, admissions = 10)
  many <- isolated_from_results(NULL, 1, tp, tracing = TRUE, bio = bio, admissions = 100 * tp$tracing$capacity)
  # per admission, the swamped team does far less
  expect_lt(many / (100 * tp$tracing$capacity), (few / 10) / 50)
})
