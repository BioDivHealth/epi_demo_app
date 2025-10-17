library(testthat)

make_identity_kernel <- function(n) {
  diag(1, nrow = n, ncol = n)
}

test_that("step_truth outputs non-negative integer incidence", {
  K <- make_identity_kernel(3)
  state <- init_truth(n_boroughs = 3L, seed_boroughs = 1L, seed_cases = 5L)

  set.seed(123)
  next_state <- step_truth(
    state = state,
    K = K,
    Rt_global = 1.2,
    k_overdisp = 0.5,
    imports_prob = 0.1,
    lambda_import = 0.5
  )

  expect_true(all(next_state$incidence >= 0))
  expect_true(all(next_state$incidence == as.integer(next_state$incidence)))
  expect_equal(next_state$turn, 1L)
  expect_length(next_state$history, length(state$history) + 1)
})

test_that("expected growth tracks Rt when k is infinite", {
  K <- make_identity_kernel(1)
  state <- init_truth(n_boroughs = 1L, seed_boroughs = 1L, seed_cases = 20L)
  Rt <- 1.4

  set.seed(456)
  draws <- replicate(
    n = 500,
    expr = step_truth(
      state = state,
      K = K,
      Rt_global = Rt,
      k_overdisp = Inf,
      imports_prob = 0,
      lambda_import = 0
    )$incidence[1]
  )

  expect_equal(mean(draws), 20 * Rt, tolerance = 0.1)
})
