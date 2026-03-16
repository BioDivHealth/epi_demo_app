library(testthat)

empty_results_dual <- function() {
  tibble::tibble(
    borough_index = integer(),
    borough_name = character(),
    type = character(),
    n = integer(),
    positives = integer(),
    turn_sampled = integer(),
    turn_available = integer()
  )
}

make_dual_fixture <- function() {
  pop_tbl <- tibble::tibble(
    gss_code = c("A1", "B2", "C3"),
    borough_name = c("Alpha", "Beta", "Gamma"),
    pop = c(1000, 1400, 1100)
  )

  init <- init_truth(n_boroughs = 3, seed_boroughs = 1L, seed_cases = 25L)
  pair <- initialise_branch_pair(
    init_truth = init,
    player_observation = init_observation_state(),
    player_available = empty_results_dual(),
    pop_tbl = pop_tbl,
    budget_total = 320
  )

  list(
    pop_tbl = pop_tbl,
    pair = pair,
    budget = list(budget = 320, costs = list(ag = 2, pcr = 8)),
    params_epi = list(Rt = 1.3, k = 0.8, imports_prob = 0.01, lambda_import = 0.25),
    params_mort = list(death_ifr = 0.001, death_delay = 1L),
    testing_params = list(
      sens = list(ag = 0.8, pcr = 0.98),
      spec = list(ag = 0.995, pcr = 0.995),
      delays = list(ag = 0L, pcr = 1L),
      prev_floor = 1e-6,
      sampling_mode = "symptomatic",
      sampling_pool_frac = 0.1
    ),
    active_kernel = matrix(
      c(
        0.7, 0.2, 0.1,
        0.2, 0.6, 0.2,
        0.1, 0.2, 0.7
      ),
      nrow = 3,
      byrow = TRUE
    ),
    intervention_defs = intervention_catalog()
  )
}

build_dual_request <- function(fx, branch, allocs, active_ids = character(), truth_seed = NULL) {
  list(
    current_turn = branch$turn,
    allocs = allocs,
    budget = fx$budget,
    params_epi = fx$params_epi,
    params_mort = fx$params_mort,
    testing_params = fx$testing_params,
    active_ids = active_ids,
    intervention_defs = fx$intervention_defs,
    intervention_state = branch$intervention_state,
    total_spend = branch$total_spend,
    truth_state = branch$truth_state,
    observation_state = branch$observation_state,
    active_kernel = fx$active_kernel,
    effective_pop_tbl = fx$pop_tbl,
    pop_tbl = fx$pop_tbl,
    run_log = branch$run_log,
    turn_logs = branch$turn_logs,
    branch = branch$name,
    truth_seed = truth_seed
  )
}

test_that("dual branches advance in lockstep for one turn", {
  fx <- make_dual_fixture()
  pair <- fx$pair

  player_allocs <- tibble::tibble(
    borough_index = 1:3,
    borough_name = fx$pop_tbl$borough_name,
    n_ag = c(12L, 6L, 4L),
    n_pcr = c(2L, 1L, 0L)
  )
  baseline_allocs <- build_baseline_allocations(
    "basic_surveillance",
    list(pop_tbl = fx$pop_tbl, budget = fx$budget, intervention_cost = 0)
  )

  player_result <- run_single_turn(build_dual_request(
    fx = fx,
    branch = pair$player,
    allocs = player_allocs,
    active_ids = character(),
    truth_seed = 909L
  ))
  baseline_result <- run_single_turn(build_dual_request(
    fx = fx,
    branch = pair$baseline,
    allocs = baseline_allocs,
    active_ids = character(),
    truth_seed = 909L
  ))

  expect_equal(player_result$status, "ok")
  expect_equal(baseline_result$status, "ok")

  pair$player <- apply_branch_turn_result(pair$player, player_result)
  pair$baseline <- apply_branch_turn_result(pair$baseline, baseline_result)

  expect_equal(pair$player$turn, 1L)
  expect_equal(pair$baseline$turn, 1L)
  expect_equal(pair$player$turn, pair$baseline$turn)
})

test_that("baseline policies satisfy PR3 invariants", {
  fx <- make_dual_fixture()

  alloc_none <- build_baseline_allocations(
    "no_action",
    list(pop_tbl = fx$pop_tbl, budget = fx$budget, intervention_cost = 0)
  )
  expect_true(all(alloc_none$n_ag == 0L))
  expect_true(all(alloc_none$n_pcr == 0L))

  alloc_surv <- build_baseline_allocations(
    "basic_surveillance",
    list(pop_tbl = fx$pop_tbl, budget = fx$budget, intervention_cost = 0)
  )
  spend <- summarise_budget(alloc_surv, fx$budget$costs, fx$budget$budget)$spend
  expect_true(all(alloc_surv$n_pcr == 0L))
  expect_lte(spend, fx$budget$budget + 1e-6)

  baseline_result <- run_single_turn(build_dual_request(
    fx = fx,
    branch = fx$pair$baseline,
    allocs = alloc_surv,
    active_ids = character(),
    truth_seed = 511L
  ))
  expect_equal(baseline_result$status, "ok")
  expect_equal(baseline_result$run_log$active_interventions[[1]], "None")
})

test_that("truth_seed produces deterministic truth update", {
  fx <- make_dual_fixture()

  allocs <- tibble::tibble(
    borough_index = 1:3,
    borough_name = fx$pop_tbl$borough_name,
    n_ag = c(4L, 4L, 4L),
    n_pcr = c(0L, 0L, 0L)
  )

  request_a <- build_dual_request(
    fx = fx,
    branch = fx$pair$player,
    allocs = allocs,
    active_ids = character(),
    truth_seed = 2222L
  )
  request_b <- build_dual_request(
    fx = fx,
    branch = fx$pair$baseline,
    allocs = allocs,
    active_ids = character(),
    truth_seed = 2222L
  )

  res_a <- run_single_turn(request_a)
  res_b <- run_single_turn(request_b)

  expect_equal(res_a$status, "ok")
  expect_equal(res_b$status, "ok")
  expect_equal(res_a$truth_state$incidence, res_b$truth_state$incidence)
})

test_that("dual apply guard prevents partial updates on budget failure", {
  fx <- make_dual_fixture()
  pair_before <- fx$pair

  player_allocs <- tibble::tibble(
    borough_index = 1:3,
    borough_name = fx$pop_tbl$borough_name,
    n_ag = c(200L, 200L, 200L),
    n_pcr = c(80L, 80L, 80L)
  )
  baseline_allocs <- build_baseline_allocations(
    "no_action",
    list(pop_tbl = fx$pop_tbl, budget = fx$budget, intervention_cost = 0)
  )

  player_result <- run_single_turn(build_dual_request(
    fx = fx,
    branch = pair_before$player,
    allocs = player_allocs,
    active_ids = character(),
    truth_seed = 333L
  ))
  baseline_result <- run_single_turn(build_dual_request(
    fx = fx,
    branch = pair_before$baseline,
    allocs = baseline_allocs,
    active_ids = character(),
    truth_seed = 333L
  ))

  expect_equal(player_result$status, "budget_exceeded")
  expect_equal(baseline_result$status, "ok")

  pair_after <- pair_before
  if (identical(player_result$status, "ok") && identical(baseline_result$status, "ok")) {
    pair_after$player <- apply_branch_turn_result(pair_after$player, player_result)
    pair_after$baseline <- apply_branch_turn_result(pair_after$baseline, baseline_result)
  }

  expect_equal(pair_after$player$turn, pair_before$player$turn)
  expect_equal(pair_after$baseline$turn, pair_before$baseline$turn)
})
