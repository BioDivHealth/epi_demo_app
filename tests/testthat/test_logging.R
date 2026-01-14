library(testthat)

test_that("turn logs initialise with expected structure", {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2"),
    borough_name = c("Alpha", "Beta"),
    pop = c(1000, 1200)
  )

  logs <- init_turn_logs(pop_tbl)
  expect_named(logs, c("truth", "observations", "actions"))
  expect_equal(nrow(logs$truth), 0)
  expect_equal(nrow(logs$observations), 0)
  expect_equal(nrow(logs$actions), 0)
})

test_that("truth log appends one row per borough with metadata", {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2"),
    borough_name = c("Alpha", "Beta"),
    pop = c(1000, 1200)
  )
  logs <- init_turn_logs(pop_tbl)

  updated <- append_truth_log(
    truth_log = logs$truth,
    turn = 3L,
    incidence = c(7L, 2L),
    cumulative = c(30L, 12L),
    imports = c(1L, 0L),
    Rt = 1.4,
    pop_tbl = pop_tbl
  )

  expect_equal(nrow(updated), 2)
  expect_true(all(updated$turn == 3L))
  expect_equal(updated$incidence, c(7L, 2L))
  expect_equal(updated$imports, c(1L, 0L))
  expect_equal(updated$borough_name, pop_tbl$borough_name)
})

test_that("observation log captures reported turn and positivity", {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2"),
    borough_name = c("Alpha", "Beta"),
    pop = c(1000, 1200)
  )
  logs <- init_turn_logs(pop_tbl)

  available <- tibble::tibble(
    borough_index = c(1L, 2L),
    borough_name = c("Alpha", "Beta"),
    type = c("ag", "pcr"),
    n = c(10L, 5L),
    positives = c(3L, 0L),
    turn_sampled = c(1L, 1L),
    turn_available = c(2L, 2L)
  )

  updated <- append_observation_log(
    observation_log = logs$observations,
    available_results = available,
    turn_reported = 2L,
    pop_tbl = pop_tbl
  )

  expect_equal(nrow(updated), 2)
  expect_true(all(updated$turn_reported == 2L))
  expect_equal(updated$positivity, c(0.3, 0))

  unchanged <- append_observation_log(
    observation_log = updated,
    available_results = tibble::tibble(),
    turn_reported = 3L,
    pop_tbl = pop_tbl
  )
  expect_equal(nrow(unchanged), 2)
})

test_that("action log records allocations with spend metadata", {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2"),
    borough_name = c("Alpha", "Beta"),
    pop = c(1000, 1200)
  )
  logs <- init_turn_logs(pop_tbl)

  allocations <- tibble::tibble(
    borough_index = c(1L, 2L),
    borough_name = c("Alpha", "Beta"),
    n_ag = c(5L, 0L),
    n_pcr = c(0L, 4L)
  )

  meta <- list(
    allocation_spend = 30,
    intervention_cost = 5,
    total_turn_spend = 35,
    cumulative_spend = 80,
    budget_remaining = 120,
    interventions_active = "Masking"
  )

  updated <- append_action_log(
    action_log = logs$actions,
    turn = 4L,
    allocations = allocations,
    pop_tbl = pop_tbl,
    costs = list(ag = 2, pcr = 5),
    meta = meta
  )

  expect_equal(nrow(updated), 2)
  expect_true(all(updated$turn == 4L))
  expect_equal(sum(updated$n_allocated), 9)
  expect_equal(sum(updated$spend), 2 * 5 + 5 * 4)
  expect_true(all(updated$allocation_spend == meta$allocation_spend))
  expect_true(all(updated$interventions_active == meta$interventions_active))

  zero_alloc <- allocations
  zero_alloc$n_ag <- 0L
  zero_alloc$n_pcr <- 0L
  unchanged <- append_action_log(
    action_log = updated,
    turn = 5L,
    allocations = zero_alloc,
    pop_tbl = pop_tbl,
    costs = list(ag = 2, pcr = 5),
    meta = meta
  )
  expect_equal(nrow(unchanged), nrow(updated))
})

test_that("active infections rolling window sums correctly", {
  truth_log <- tibble::tibble(
    turn = rep(0:4, each = 2),
    borough_index = rep(c(1L, 2L), times = 5),
    borough_name = rep(c("Alpha", "Beta"), times = 5),
    gss_code = rep(c("A1", "B2"), times = 5),
    incidence = c(
      5L, 0L,  # t0
      3L, 1L,  # t1
      0L, 2L,  # t2
      4L, 0L,  # t3
      1L, 1L   # t4
    )
  )

  active_t4_window3 <- compute_active_infections(truth_log, turn = 4L, window = 3L)
  expect_equal(
    dplyr::arrange(active_t4_window3, borough_index)$active,
    c(5L, 3L) # turns 2-4 for Alpha: 0+4+1, for Beta: 2+0+1
  )

  active_t2_window2 <- compute_active_infections(truth_log, turn = 2L, window = 2L)
  expect_equal(
    dplyr::arrange(active_t2_window2, borough_index)$active,
    c(3L, 3L) # turns 1-2: Alpha 3+0, Beta 1+2
  )

  # Window larger than available history should still respect available data
  active_t1_window5 <- compute_active_infections(truth_log, turn = 1L, window = 5L)
  expect_equal(
    dplyr::arrange(active_t1_window5, borough_index)$active,
    c(8L, 1L)
  )
})
