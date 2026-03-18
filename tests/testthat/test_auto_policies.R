library(testthat)

make_policy_request_fixture <- function(policy_id = "manual") {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2", "C3", "D4"),
    borough_name = c("Alpha", "Beta", "Gamma", "Delta"),
    pop = c(1200, 900, 1500, 1100)
  )

  allocs_manual <- tibble::tibble(
    borough_index = 1:4,
    borough_name = pop_tbl$borough_name,
    n_ag = c(12L, 8L, 5L, 2L),
    n_pcr = c(4L, 3L, 2L, 1L)
  )

  history <- tibble::tibble(
    borough_index = c(1L, 2L, 3L),
    borough_name = c("Alpha", "Beta", "Gamma"),
    type = c("ag", "ag", "pcr"),
    n = c(20L, 10L, 8L),
    positives = c(6L, 1L, 3L),
    turn_sampled = c(3L, 3L, 3L),
    turn_available = c(3L, 3L, 4L)
  )

  available_now <- tibble::tibble(
    borough_index = c(1L, 3L),
    borough_name = c("Alpha", "Gamma"),
    type = c("ag", "pcr"),
    n = c(16L, 10L),
    positives = c(5L, 4L),
    turn_sampled = c(4L, 4L),
    turn_available = c(4L, 4L)
  )

  list(
    policy_id = policy_id,
    allocs_manual = allocs_manual,
    pop_tbl = pop_tbl,
    budget = list(
      budget = 400,
      costs = list(ag = 2, pcr = 10)
    ),
    intervention_cost = 35,
    current_turn = 4L,
    observation_state = list(queue = init_observation_state()$queue, history = history),
    available_now = available_now
  )
}

make_symptomatic_focus_request_fixture <- function(signal_mode = c("weak", "strong")) {
  signal_mode <- match.arg(signal_mode)

  borough_count <- 15L
  borough_index <- seq_len(borough_count)
  pop_tbl <- tibble::tibble(
    borough_index = borough_index,
    borough_name = paste0("Borough ", borough_index),
    pop = seq(1000, 1700, length.out = borough_count)
  )

  allocs_manual <- tibble::tibble(
    borough_index = borough_index,
    borough_name = pop_tbl$borough_name,
    n_ag = integer(borough_count),
    n_pcr = integer(borough_count)
  )

  if (identical(signal_mode, "strong")) {
    history <- tibble::tibble(
      borough_index = borough_index,
      borough_name = pop_tbl$borough_name,
      type = rep("ag", borough_count),
      n = seq(30L, 16L, by = -1L),
      positives = seq(15L, 1L, by = -1L),
      turn_sampled = rep(3L, borough_count),
      turn_available = rep(3L, borough_count)
    )

    available_now <- tibble::tibble(
      borough_index = borough_index,
      borough_name = pop_tbl$borough_name,
      type = rep("pcr", borough_count),
      n = seq(20L, 6L, by = -1L),
      positives = c(seq(10L, 0L, by = -1L), rep(0L, borough_count - 11L)),
      turn_sampled = rep(4L, borough_count),
      turn_available = rep(4L, borough_count)
    )
  } else {
    history <- tibble::tibble(
      borough_index = borough_index,
      borough_name = pop_tbl$borough_name,
      type = rep("ag", borough_count),
      n = rep(3L, borough_count),
      positives = rep(0L, borough_count),
      turn_sampled = rep(3L, borough_count),
      turn_available = rep(3L, borough_count)
    )

    available_now <- tibble::tibble(
      borough_index = integer(),
      borough_name = character(),
      type = character(),
      n = integer(),
      positives = integer(),
      turn_sampled = integer(),
      turn_available = integer()
    )
  }

  list(
    policy_id = "symptomatic_proxy",
    allocs_manual = allocs_manual,
    pop_tbl = pop_tbl,
    budget = list(
      budget = 600,
      costs = list(ag = 2, pcr = 10)
    ),
    intervention_cost = 0,
    current_turn = 4L,
    observation_state = list(queue = init_observation_state()$queue, history = history),
    available_now = available_now
  )
}

test_that("policy outputs have valid allocation schema", {
  policy_ids <- c("manual", "all_boroughs", "symptomatic_proxy")

  for (policy_id in policy_ids) {
    request <- make_policy_request_fixture(policy_id = policy_id)
    set.seed(2026)
    allocs <- build_policy_allocations(request)

    expect_true(inherits(allocs, "data.frame"))
    expect_true(all(c("borough_index", "borough_name", "n_ag", "n_pcr") %in% names(allocs)))
    expect_equal(nrow(allocs), nrow(request$pop_tbl))
    expect_equal(as.character(allocs$borough_name), as.character(request$pop_tbl$borough_name))
  }
})

test_that("policies return non-negative integer allocations", {
  policy_ids <- c("manual", "all_boroughs", "symptomatic_proxy")

  for (policy_id in policy_ids) {
    request <- make_policy_request_fixture(policy_id = policy_id)
    set.seed(1)
    allocs <- build_policy_allocations(request)

    expect_true(all(allocs$n_ag >= 0L))
    expect_true(all(allocs$n_pcr >= 0L))
    expect_true(all(allocs$n_ag == as.integer(allocs$n_ag)))
    expect_true(all(allocs$n_pcr == as.integer(allocs$n_pcr)))
  }
})

test_that("cap_allocations_to_budget enforces budget feasibility", {
  request <- make_policy_request_fixture(policy_id = "manual")
  request$allocs_manual$n_ag <- c(200L, 200L, 200L, 200L)
  request$allocs_manual$n_pcr <- c(100L, 100L, 100L, 100L)

  capped <- cap_allocations_to_budget(
    allocs = request$allocs_manual,
    budget = request$budget,
    intervention_cost = request$intervention_cost
  )

  spend <- summarise_budget(capped, request$budget$costs, request$budget$budget)$spend
  limit <- request$budget$budget - request$intervention_cost

  expect_lte(spend, limit + 1e-6)
})

test_that("all_boroughs policy is deterministic across RNG seeds", {
  request <- make_policy_request_fixture(policy_id = "all_boroughs")

  set.seed(812)
  allocs_a <- build_policy_allocations(request)
  set.seed(47)
  allocs_b <- build_policy_allocations(request)

  expect_equal(allocs_a, allocs_b)
})

test_that("all_boroughs policy covers every borough when affordable", {
  request <- make_policy_request_fixture(policy_id = "all_boroughs")
  request$budget <- list(
    budget = 80,
    costs = list(ag = 1, pcr = 5)
  )
  request$intervention_cost <- 0

  allocs <- build_policy_allocations(request)
  totals <- allocs$n_ag + allocs$n_pcr

  expect_true(all(totals >= 1L))
})

test_that("symptomatic policy stays broad when recent positives are absent", {
  request <- make_symptomatic_focus_request_fixture(signal_mode = "weak")

  allocs <- build_policy_allocations(request)
  non_zero_boroughs <- sum((allocs$n_ag + allocs$n_pcr) > 0L)
  spend <- summarise_budget(allocs, request$budget$costs, request$budget$budget)$spend

  expect_gt(non_zero_boroughs, 12L)
  expect_lte(spend, request$budget$budget - request$intervention_cost + 1e-6)
})

test_that("symptomatic policy focuses on top 12 boroughs once signal is established", {
  request <- make_symptomatic_focus_request_fixture(signal_mode = "strong")

  set.seed(812)
  allocs_a <- build_policy_allocations(request)
  set.seed(47)
  allocs_b <- build_policy_allocations(request)

  non_zero_boroughs <- sum((allocs_a$n_ag + allocs_a$n_pcr) > 0L)
  spend <- summarise_budget(allocs_a, request$budget$costs, request$budget$budget)$spend

  expect_equal(allocs_a, allocs_b)
  expect_lte(non_zero_boroughs, 12L)
  expect_true(all((allocs_a$n_ag + allocs_a$n_pcr)[13:15] == 0L))
  expect_lte(spend, request$budget$budget - request$intervention_cost + 1e-6)
})

test_that("manual policy returns exact manual allocations when affordable", {
  request <- make_policy_request_fixture(policy_id = "manual")
  request$budget$budget <- 2000
  request$intervention_cost <- 0

  expected <- normalise_policy_allocations(request$allocs_manual, request$pop_tbl)
  actual <- build_policy_allocations(request)

  expect_equal(actual, expected)
})

test_that("guided_auto turn allocations use borough policy logic", {
  request <- make_policy_request_fixture(policy_id = "all_boroughs")
  request$allocs_manual$n_ag <- 0L
  request$allocs_manual$n_pcr <- 0L

  expected <- build_policy_allocations(request)
  actual <- build_run_mode_allocations("guided_auto", request)

  expect_equal(actual, expected)
  expect_gt(sum(actual$n_ag + actual$n_pcr), 0L)
})

test_that("manual turn allocations keep manual allocations even when auto policy is selected", {
  request <- make_policy_request_fixture(policy_id = "all_boroughs")
  request$budget$budget <- 2000
  request$intervention_cost <- 0

  expected <- normalise_policy_allocations(request$allocs_manual, request$pop_tbl)
  actual <- build_run_mode_allocations("manual", request)

  expect_equal(actual, expected)
})

test_that("baseline no_action returns zero allocations", {
  request <- make_policy_request_fixture(policy_id = "manual")
  baseline_request <- list(
    pop_tbl = request$pop_tbl,
    budget = request$budget,
    intervention_cost = 0
  )

  allocs <- build_baseline_allocations("no_action", baseline_request)
  expect_true(all(allocs$n_ag == 0L))
  expect_true(all(allocs$n_pcr == 0L))
})

test_that("baseline basic_surveillance is budget feasible and antigen-only", {
  request <- make_policy_request_fixture(policy_id = "manual")
  baseline_request <- list(
    pop_tbl = request$pop_tbl,
    budget = request$budget,
    intervention_cost = 25
  )

  allocs <- build_baseline_allocations("basic_surveillance", baseline_request)
  spend <- summarise_budget(allocs, request$budget$costs, request$budget$budget)$spend
  limit <- request$budget$budget - baseline_request$intervention_cost

  expect_true(all(allocs$n_pcr == 0L))
  expect_true(all(allocs$n_ag >= 0L))
  expect_true(all(allocs$n_ag == as.integer(allocs$n_ag)))
  expect_lte(spend, limit + 1e-6)
})
