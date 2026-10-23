library(testthat)

# Fixed report cadence -----------------------------------------------------------
# Every disease is played with a report every report_days (7) days. The hidden
# outbreak steps one report at a time, so each step spans report_days /
# gen_days generations of infection.

data_dir <- testthat::test_path("..", "..", "data")
inputs <- load_inputs(data_dir = data_dir)

test_that("a step of g generations grows by R^g", {
  K1 <- matrix(1, 1, 1)
  start <- init_truth(n_boroughs = 1, seed_boroughs = 1, seed_cases = 100000L)

  # no randomness (Poisson with a huge count), no depletion (no pop given)
  two_gens <- step_truth(start, K1, Rt_global = 1.3, k_overdisp = Inf,
                         imports_prob = 0, lambda_import = 0, gens_per_step = 2)
  expect_equal(sum(two_gens$incidence) / 100000, 1.3^2, tolerance = 0.01)

  # less than one generation per step: slower growth per step
  half_gen <- step_truth(start, K1, Rt_global = 1.3, k_overdisp = Inf,
                         imports_prob = 0, lambda_import = 0, gens_per_step = 0.5)
  expect_equal(sum(half_gen$incidence) / 100000, 1.3^0.5, tolerance = 0.01)
})

test_that("one generation per step is the old model exactly", {
  K1 <- matrix(1, 1, 1)
  start <- init_truth(n_boroughs = 1, seed_boroughs = 1, seed_cases = 500L)
  set.seed(5)
  old <- step_truth(start, K1, Rt_global = 1.6, k_overdisp = 0.5, imports_prob = 0, lambda_import = 0)
  set.seed(5)
  new <- step_truth(start, K1, Rt_global = 1.6, k_overdisp = 0.5, imports_prob = 0, lambda_import = 0,
                    gens_per_step = 1)
  expect_equal(old$incidence, new$incidence)
})

test_that("growth per day does not depend on the report cadence", {
  # 7 days of a 5-day generation: R^(7/5) per weekly report
  config <- scenario_config("covid_early", inputs)
  expect_equal(config$gens_per_turn, 7 / 5)

  weekly_R <- config$epi$Rt^config$gens_per_turn
  daily_rate <- log(weekly_R) / 7
  per_generation_rate <- log(config$epi$Rt) / 5
  expect_equal(daily_rate, per_generation_rate)
})

test_that("every disease reports weekly and its delays are converted from days", {
  for (id in setdiff(scenario_catalog()$id, "custom")) {
    config <- scenario_config(id, inputs)
    sc <- scenario_lookup(id)
    expect_equal(turn_day(1, config) - turn_day(0, config), 7L)
    expect_equal(config$bio$hosp_delay, max(1L, round(sc$hosp_delay_days / 7)))
    expect_equal(config$mort$death_delay, round(sc$death_delay_days / 7))
  }
})

test_that("the dashboard's Rt is per generation, so 1 still means flat", {
  # an outbreak growing R^g per weekly report must read as R
  g <- 7 / 3
  weekly <- round(50 * (1.4^g)^(0:10))
  per_report <- estimate_rt_series(weekly)$mean
  expect_equal(per_report^(1 / g), 1.4, tolerance = 0.03)
})
