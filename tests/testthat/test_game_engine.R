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
  quiet <- sum(vapply(1:10, function(s) play_bot_game(config, bot_do_nothing, seed = s, n_turns = 12)$cumulative_infections, numeric(1)))
  build <- sum(vapply(1:10, function(s) play_bot_game(config, bot_build, seed = s, n_turns = 12)$cumulative_infections, numeric(1)))
  expect_lt(build, 0.9 * quiet)
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

test_that("the unseen start ends at the first death (deaths are always noticed)", {
  for (disease in c("ebola_like", "nipah_like", "covid_early")) {
    config <- scenario_config(disease, inputs)
    for (seed in 1:15) {
      state <- new_game(config, seed = seed)
      deaths_per_turn <- vapply(state$truth$death_history, sum, numeric(1))

      if (identical(state$alert$trigger, "death")) {
        # deaths only in the last hidden turn, when the report was made
        expect_gt(utils::tail(deaths_per_turn, 1), 0)
        expect_true(all(utils::head(deaths_per_turn, -1) == 0))
      } else {
        expect_equal(sum(deaths_per_turn), 0)
      }
      # at most 10 generations unseen, counted in weekly reports
      max_reports <- max(1, round(config$tp$head_start$max_turns / config$gens_per_turn))
      expect_lte(state$truth_offset, max_reports)
      expect_gt(sum(state$truth$incidence) + sum(state$truth$cumulative_deaths), 0)
    }
  }
})
