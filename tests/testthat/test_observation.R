library(testthat)

# Use testthat helper to find project root
data_dir <- testthat::test_path("..", "..", "data")
inputs <- load_inputs(data_dir = data_dir)

test_that("p_star extremes behave as expected", {
  n_b <- nrow(inputs$pop_tbl)
  base_alloc <- tibble(
    borough_index = seq_len(n_b),
    borough_name = inputs$pop_tbl$borough_name,
    n_ag = integer(n_b),
    n_pcr = integer(n_b)
  )

  # focus on first borough with a sizeable prevalence
  base_alloc$n_pcr[1] <- 400
  truth_incidence <- integer(n_b)
  truth_incidence[1] <- floor(0.05 * inputs$pop_tbl$pop[1])

  # Perfect test (Se=1, Sp=1)
  set.seed(123)
  params <- list(
    sens = list(ag = 1, pcr = 1),
    spec = list(ag = 1, pcr = 1),
    delays = list(ag = 0, pcr = 0),
    prev_floor = 1e-6
  )
  res <- observe_tests(
    state = init_observation_state(),
    turn = 0,
    allocations = base_alloc,
    truth_incidence = truth_incidence,
    pop_tbl = inputs$pop_tbl,
    params = params
  )
  pcr_row <- dplyr::filter(res$new_results, type == "pcr")
  prev <- truth_incidence[1] / inputs$pop_tbl$pop[1]
  expect_equal(pcr_row$positives / pcr_row$n, prev, tolerance = 0.05)

  # Zero sensitivity -> zero positives
  params$sens$pcr <- 0
  res_zero <- observe_tests(
    state = init_observation_state(),
    turn = 0,
    allocations = base_alloc,
    truth_incidence = truth_incidence,
    pop_tbl = inputs$pop_tbl,
    params = params
  )
  pcr_row_zero <- dplyr::filter(res_zero$new_results, type == "pcr")
  expect_equal(pcr_row_zero$positives, 0)

  # Perfect sensitivity, no specificity -> near-universal positivity
  params$sens$pcr <- 1
  params$spec$pcr <- 0
  set.seed(321)
  res_fp <- observe_tests(
    state = init_observation_state(),
    turn = 0,
    allocations = base_alloc,
    truth_incidence = truth_incidence,
    pop_tbl = inputs$pop_tbl,
    params = params
  )
  pcr_row_fp <- dplyr::filter(res_fp$new_results, type == "pcr")
  expect_true(pcr_row_fp$positives >= pcr_row$n)
})

test_that("observation source metadata propagates through results and queues", {
  n_b <- nrow(inputs$pop_tbl)
  alloc <- tibble(
    borough_index = seq_len(n_b),
    borough_name = inputs$pop_tbl$borough_name,
    n_ag = integer(n_b),
    n_pcr = integer(n_b)
  )
  alloc$n_pcr[1] <- 12L
  alloc$n_ag[2] <- 10L

  truth_incidence <- integer(n_b)
  truth_incidence[1:2] <- c(50L, 40L)

  params <- list(
    sens = list(ag = 1, pcr = 1),
    spec = list(ag = 1, pcr = 1),
    delays = list(ag = 0L, pcr = 1L),
    prev_floor = 1e-6
  )

  res <- observe_tests(
    state = init_observation_state(),
    turn = 0L,
    allocations = alloc,
    truth_incidence = truth_incidence,
    pop_tbl = inputs$pop_tbl,
    params = params,
    source = "starter_intel"
  )

  expect_true("source" %in% names(res$new_results))
  expect_true(all(res$new_results$source == "starter_intel"))
  expect_true(all(res$available$source == "starter_intel"))
  expect_true(all(res$queue$source == "starter_intel"))
})

test_that("result queues honour delays", {
  n_b <- nrow(inputs$pop_tbl)
  base_alloc <- tibble(
    borough_index = seq_len(n_b),
    borough_name = inputs$pop_tbl$borough_name,
    n_ag = integer(n_b),
    n_pcr = integer(n_b)
  )
  base_alloc$n_pcr[1] <- 20

  zero_alloc <- base_alloc
  zero_alloc$n_pcr <- 0
  zero_alloc$n_ag <- 0

  truth_incidence <- integer(n_b)
  truth_incidence[1] <- 100

  params <- list(
    sens = list(ag = 1, pcr = 1),
    spec = list(ag = 1, pcr = 1),
    delays = list(ag = 0, pcr = 2),
    prev_floor = 1e-6
  )

  state <- init_observation_state()

  res0 <- observe_tests(state, 0, base_alloc, truth_incidence, inputs$pop_tbl, params)
  expect_equal(nrow(res0$available), 0)
  expect_gt(nrow(res0$queue), 0)

  state <- list(queue = res0$queue, history = res0$history)
  res1 <- observe_tests(state, 1, zero_alloc, truth_incidence, inputs$pop_tbl, params)
  expect_equal(nrow(res1$available), 0)

  state <- list(queue = res1$queue, history = res1$history)
  res2 <- observe_tests(state, 2, zero_alloc, truth_incidence, inputs$pop_tbl, params)
  expect_gt(nrow(res2$available), 0)
  expect_true(all(res2$available$turn_available == 2))
  expect_true(all(res2$available$source == "player_turn"))
})
