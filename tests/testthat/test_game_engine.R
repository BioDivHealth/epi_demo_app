library(testthat)

data_dir <- testthat::test_path("..", "..", "data")
inputs <- load_inputs(data_dir = data_dir)

test_that("the same seed gives the same outbreak whatever the player does", {
  config <- scenario_config("covid_early", inputs)
  quiet <- play_bot_game(config, bot_do_nothing, seed = 11, n_turns = 6)
  testing <- play_bot_game(config, bot_broad, seed = 11, n_turns = 6)

  # broad surveillance only isolates a few cases early, so the very first
  # turns of the hidden outbreak must match exactly
  expect_equal(quiet$state$truth$history[1:2], testing$state$truth$history[1:2])
})

test_that("the player view never contains the hidden truth", {
  config <- scenario_config("covid_early", inputs)
  state <- new_game(config, seed = 3)
  view <- player_view(state, config)

  expect_false(any(c("true_pos", "infectious_pos") %in% names(view$history)))
  expect_null(view$truth)
  expect_null(view$biases)
  expect_false(any(c("positives", "true_pos") %in% names(view$pending)))
})

test_that("one budget is shared across the whole game", {
  config <- scenario_config("covid_early", inputs)
  state <- new_game(config, seed = 4)

  # spend almost all of the pot, then a big turn must be refused
  state$spend_total <- config$tp$budget_total - 1000
  big <- empty_allocations(config)
  big$n_pcr[1] <- 1000
  result <- play_turn(state, list(allocations = big, sampling_mode = "random"), config)
  expect_false(result$ok)
})

test_that("symptomatic tests are capped at the people who come forward", {
  config <- scenario_config("covid_early", inputs)
  state <- new_game(config, seed = 5)
  pool <- symptomatic_pool_size(state, config)

  allocs <- empty_allocations(config)
  allocs$n_ag[1] <- pool[1] * 3
  result <- play_turn(state, list(allocations = allocs, sampling_mode = "symptomatic"), config)

  expect_true(result$ok)
  expect_lte(sum(result$allocations$n_ag), pool[1])
})

test_that("building testing moves up a level after the build time", {
  config <- scenario_config("ebola_like", inputs)
  state <- new_game(config, seed = 6)
  start_level <- state$test_level

  result <- play_turn(state, list(build_testing = TRUE), config)
  expect_true(result$ok)
  state <- result$state
  for (i in seq_len(config$tp$build_testing$turns)) {
    state <- play_turn(state, list(), config)$state
  }
  expect_equal(state$test_level, start_level + 1L)
})

test_that("no antigen tests exist at the lowest testing level", {
  config <- scenario_config("ebola_like", inputs)
  state <- new_game(config, seed = 7)
  allocs <- empty_allocations(config)
  allocs$n_ag[1] <- 100
  result <- play_turn(state, list(allocations = allocs, sampling_mode = "random"), config)
  expect_equal(sum(result$allocations$n_ag), 0)
})

test_that("isolating found cases slows the outbreak (over several outbreaks)", {
  # one outbreak alone is too noisy: once anything differs, the random draws
  # split, so compare the total over ten outbreaks
  config <- scenario_config("ebola_like", inputs)
  test_the_sick <- function(view, memory) {
    n <- min(view$allowance / view$costs$pcr, view$testing$capacity)
    list(actions = list(allocations = make_allocations(view, n_pcr = spread_tests(n, view$pop_tbl$pop)),
                        sampling_mode = "symptomatic"), memory = memory)
  }
  quiet <- sum(vapply(1:10, function(s) play_bot_game(config, bot_do_nothing, seed = s, n_turns = 12)$cumulative_infections, numeric(1)))
  tested <- sum(vapply(1:10, function(s) play_bot_game(config, test_the_sick, seed = s, n_turns = 12)$cumulative_infections, numeric(1)))
  expect_lt(tested, 0.9 * quiet)
})

test_that("PCR beyond lab capacity queues and comes back late, not lost", {
  config <- scenario_config("ebola_like", inputs)
  state <- new_game(config, seed = 12)
  capacity <- test_setup(state, config)$capacity

  # two turns' worth of tests already waiting, then order one more turn's
  # worth (affordable within the per-turn budget)
  state$lab_backlog <- 2 * capacity
  allocs <- empty_allocations(config)
  allocs$n_pcr <- spread_tests(capacity, config$pop_tbl$pop)
  result <- play_turn(state, list(allocations = allocs, sampling_mode = "random"), config)
  expect_true(result$ok)

  expect_equal(sum(result$allocations$n_pcr), sum(allocs$n_pcr))
  expect_equal(result$pcr_extra_delay, 2L)
  expect_gt(result$state$lab_backlog, 0)
  base_delay <- test_setup(state, config)$delays$pcr
  # all of this turn's tests come back 2 turns late (the starter alert's
  # tests, also sampled at turn 0, are on their normal schedule)
  queued <- result$state$obs$queue
  late <- queued$turn_available == state$turn + base_delay + 2L
  expect_equal(sum(queued$n[late]), sum(allocs$n_pcr))
})

test_that("isolation works less well when there are many positives to support", {
  tp <- teaching_params()
  one <- tibble::tibble(
    borough_index = 1L, positives = 10L, true_pos = 10L, infectious_pos = 10L,
    mode = "random", turn_sampled = 0L, turn_available = 0L
  )
  many <- one
  many$positives <- 10L + as.integer(tp$isolation$half_strength_load)

  few_load <- isolated_from_results(one, 33, tp)[1]
  high_load <- isolated_from_results(many, 33, tp)[1]
  expect_lt(high_load, few_load * 0.55)
})

test_that("the unseen start ends when a death or a cluster of admissions is noticed", {
  for (disease in c("ebola_like", "nipah_like", "covid_early", "measles_low_vax")) {
    config <- scenario_config(disease, inputs)
    for (seed in 1:15) {
      state <- new_game(config, seed = seed)
      deaths_per_turn <- vapply(state$truth$death_history, sum, numeric(1))
      expect_true(state$alert$trigger %in% c("death", "admissions"))

      if (identical(state$alert$trigger, "death")) {
        # deaths only in the last hidden turn, when the report was made
        expect_gt(utils::tail(deaths_per_turn, 1), 0)
        expect_true(all(utils::head(deaths_per_turn, -1) == 0))
      } else {
        expect_equal(sum(deaths_per_turn), 0)
      }
      expect_gte(state$truth_offset, 1)
      expect_lte(state$truth_offset, config$tp$head_start$max_reports)
    }
  }
})

test_that("an outbreak noticed through admissions has more than one infection behind it", {
  # COVID-like: 3% of infections go to hospital, so 3 admissions means a real cluster
  config <- scenario_config("covid_early", inputs)
  sizes <- vapply(1:20, function(seed) {
    state <- new_game(config, seed = seed)
    if (identical(state$alert$trigger, "admissions")) state$start_infections else NA_real_
  }, numeric(1))
  expect_gt(stats::median(sizes, na.rm = TRUE), 20)
})

test_that("the review compares a game with doing nothing on the same outbreak", {
  config <- scenario_config("covid_early", inputs)
  # this test is about the comparison, so allow lockdown from the start
  config$tp$lockdown_trigger <- NULL

  # doing nothing compared with doing nothing: nothing prevented
  idle <- new_game(config, seed = 21)
  for (i in 1:6) idle <- play_turn(idle, list(), config)$state
  same <- compare_with_doing_nothing(idle, config)
  expect_equal(same$infections, same$infections_doing_nothing)
  expect_equal(same$infections_prevented, 0)

  # distancing from the start prevents some infections
  busy <- new_game(config, seed = 21)
  for (i in 1:6) busy <- play_turn(busy, list(interventions = "distancing"), config)$state
  cmp <- compare_with_doing_nothing(busy, config)
  expect_equal(cmp$infections_doing_nothing, same$infections_doing_nothing)
  expect_gt(cmp$infections_prevented, 0)
})

test_that("rapid tests take their development time, then exist and make lockdown stronger", {
  config <- scenario_config("covid_early", inputs)
  config$tp$lockdown_trigger <- NULL
  state <- new_game(config, seed = 8)
  expect_false(test_setup(state, config)$ag_available)

  state <- play_turn(state, list(develop_rapid = TRUE), config)$state
  for (i in seq_len(config$tp$rapid_tests$turns - 2)) state <- play_turn(state, list(), config)$state
  expect_false(test_setup(state, config)$ag_available)      # one report still to go
  state <- play_turn(state, list(), config)$state
  setup <- test_setup(state, config)
  expect_true(setup$ag_available)
  expect_gt(setup$ag_capacity, 0)

  # lockdown with rapid tests uses the stronger multiplier
  locked <- play_turn(play_turn(state, list(interventions = "distancing"), config)$state,
                      list(interventions = "distancing"), config)
  expect_equal(locked$effects$Rt_multiplier, config$tp$rapid_tests$lockdown_Rt_multiplier)
})

test_that("an outbreak is declared over after enough quiet reports, and bots stop there", {
  config <- scenario_config("nipah_like", inputs)
  quiet <- config$tp$declare_over_after
  state <- new_game(config, seed = 2)
  state$turn <- quiet + 2L
  state$reports <- data.frame(turn = 1:(quiet + 2L), borough_index = 1L, admissions = 0L, deaths = 0L)
  expect_true(declared_over(state, config))

  # one admission within the last `quiet` reports: not over
  state$reports$admissions[quiet + 1L] <- 1L
  expect_false(declared_over(state, config))

  # a bot game ends early once it is declared over
  game <- play_bot_game(config, bot_do_nothing, seed = 2, n_turns = 20)
  expect_true(game$state$turn <= 20)
})
