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

test_that("policy outputs have valid allocation schema", {
  policy_ids <- c("manual", "random_population", "symptomatic_proxy")

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
  policy_ids <- c("manual", "random_population", "symptomatic_proxy")

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

test_that("random_population policy is deterministic under fixed seed", {
  request <- make_policy_request_fixture(policy_id = "random_population")

  set.seed(812)
  allocs_a <- build_policy_allocations(request)
  set.seed(812)
  allocs_b <- build_policy_allocations(request)

  expect_equal(allocs_a, allocs_b)
})

test_that("manual policy returns exact manual allocations when affordable", {
  request <- make_policy_request_fixture(policy_id = "manual")
  request$budget$budget <- 2000
  request$intervention_cost <- 0

  expected <- normalise_policy_allocations(request$allocs_manual, request$pop_tbl)
  actual <- build_policy_allocations(request)

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
