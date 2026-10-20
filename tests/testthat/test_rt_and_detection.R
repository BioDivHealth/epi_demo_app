library(testthat)

# Rt shown to students ---------------------------------------------------------
# One turn is one generation, so a series growing by 1.3x per turn has Rt 1.3.

test_that("Rt is right for a series with known growth", {
  growing <- round(50 * 1.3^(0:12))
  expect_equal(estimate_rt_series(growing)$mean, 1.3, tolerance = 0.03)

  flat <- rep(200, 12)
  expect_equal(estimate_rt_series(flat)$mean, 1, tolerance = 0.03)

  shrinking <- round(2000 * 0.7^(0:10))
  expect_equal(estimate_rt_series(shrinking)$mean, 0.7, tolerance = 0.05)
})

test_that("Rt is not reported when there are too few cases to judge", {
  expect_true(is.na(estimate_rt_series(rep(0, 10))$mean))
  expect_true(is.na(estimate_rt_series(c(0, 0, 1, 0, 1, 0, 0))$mean))
})

test_that("Rt from the hidden outbreak matches the Rt it was run with", {
  K1 <- matrix(1, 1, 1)
  state <- init_truth(n_boroughs = 1, seed_boroughs = 1, seed_cases = 2000L)
  set.seed(3)
  for (i in 1:8) {
    state <- step_truth(state, K1, Rt_global = 1.3, k_overdisp = Inf,
                        imports_prob = 0, lambda_import = 0)
  }
  incidence <- vapply(state$history, sum, numeric(1))
  expect_equal(estimate_rt_series(incidence)$mean, 1.3, tolerance = 0.05)
})

# Detection metrics in the reveal ----------------------------------------------

make_history <- function(source, positives, true_pos, turn) {
  tibble::tibble(
    borough_index = 1L, borough_name = "A", type = "pcr", n = 100L,
    positives = as.integer(positives), true_pos = as.integer(true_pos),
    infectious_pos = as.integer(true_pos), mode = "symptomatic", source = source,
    turn_sampled = as.integer(turn), turn_available = as.integer(turn)
  )
}

pop_stub <- tibble::tibble(gss_code = "X", borough_name = "A", pop = 100000)
truth_stub <- list(c(100L), c(130L), c(170L), c(220L))

test_that("the free starter alert does not count as the player finding the outbreak", {
  hist <- make_history("starter", positives = 10, true_pos = 10, turn = 0)
  metrics <- score_run(truth_stub, hist, pop_stub)
  expect_true(is.na(metrics$time_to_detection))
  expect_equal(metrics$real_cases_found, 0)
})

test_that("false positives do not count as finding the outbreak", {
  hist <- dplyr::bind_rows(
    make_history("player", positives = 5, true_pos = 0, turn = 1),
    make_history("player", positives = 8, true_pos = 3, turn = 2)
  )
  metrics <- score_run(truth_stub, hist, pop_stub)
  expect_equal(metrics$time_to_detection, 2)
  expect_equal(metrics$real_cases_found, 3)
  expect_equal(metrics$false_positives, 10)
})

test_that("the built-in Rt matches EpiEstim exactly (1-turn serial interval)", {
  skip_if_not_installed("EpiEstim")
  series <- list(
    growing = round(50 * 1.3^(0:12)),
    flat = rep(200, 12),
    noisy = c(5, 12, 9, 20, 31, 25, 44, 60, 52, 80, 95, 120)
  )
  for (incid in series) {
    ours <- estimate_rt_table(incid, window = 3, min_cases = 0)
    n <- length(incid)
    t_end <- seq(4, n)
    config <- EpiEstim::make_config(list(si_distr = c(0, 1), t_start = t_end - 2, t_end = t_end))
    theirs <- suppressWarnings(suppressMessages(
      EpiEstim::estimate_R(incid = incid, method = "non_parametric_si", config = config)
    ))$R
    expect_equal(ours$mean, theirs$`Mean(R)`, tolerance = 1e-8)
    expect_equal(ours$lower, theirs$`Quantile.0.025(R)`, tolerance = 1e-6)
    expect_equal(ours$upper, theirs$`Quantile.0.975(R)`, tolerance = 1e-6)
  }
})
