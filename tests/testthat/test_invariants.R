library(testthat)

# Invariants ------------------------------------------------------------------
#
# Things that must ALWAYS be true, whatever the disease, strategy or seed.
# They are checked by playing short games with every bot on every disease
# and checking every turn. Bugs like the borough-order mix-up (populations
# and names in a different order from the kernel and map) break one of
# these, so they fail here instead of silently skewing results.

data_dir <- testthat::test_path("..", "..", "data")
inputs <- load_inputs(data_dir = data_dir)

diseases <- setdiff(scenario_catalog()$id, "custom")
bots <- c(strategy_bots(), "Do nothing" = bot_do_nothing)

# columns a player may see in returned results (anything else is hidden truth)
public_result_cols <- c("borough_index", "borough_name", "type", "n", "positives",
                        "turn_sampled", "turn_available")
hidden_view_fields <- c("truth", "biases", "truth_rng", "carry_isolated", "alert", "obs")

# 1. Everything per-borough is in the same order ------------------------------

test_that("every per-borough table and matrix uses the map's borough order", {
  n_b <- nrow(inputs$boros)
  expect_equal(inputs$pop_tbl$borough_name, inputs$boros$borough_name)
  expect_equal(inputs$pop_tbl$gss_code, inputs$boros$gss_code)
  expect_equal(dim(inputs$K), c(n_b, n_b))
  expect_equal(dim(inputs$A), c(n_b, n_b))

  # populations agree with the population stored on the map itself
  if ("pop" %in% names(inputs$boros)) {
    expect_equal(inputs$pop_tbl$pop, inputs$boros$pop)
  }

  # the kernel only links boroughs that touch (or a borough to itself)
  touches <- sf::st_touches(sf::st_make_valid(inputs$boros), sparse = FALSE)
  off_diag <- row(inputs$K) != col(inputs$K)
  expect_true(all(inputs$K[off_diag & !touches] == 0))

  # allocation tables (what map clicks write into) follow the same order
  config <- scenario_config("covid_early", inputs)
  expect_equal(empty_allocations(config)$borough_name, inputs$boros$borough_name)
})

# 2. Every turn of every game obeys the rules ---------------------------------

check_turn <- function(before, result, actions, config, label) {
  tp <- config$tp
  after <- result$state
  n_b <- nrow(config$pop_tbl)
  info <- paste(label, "turn", before$turn)

  # hidden outbreak: whole, non-negative numbers that add up
  expect_true(all(after$truth$incidence >= 0), info = info)
  expect_true(all(after$truth$incidence == round(after$truth$incidence)), info = info)
  expect_equal(after$truth$cumulative, Reduce(`+`, after$truth$history), info = info)
  expect_equal(after$turn, before$turn + 1L, info = info)

  # deaths (including extra deaths when hospitals are full) add up, and
  # hospitals never hold a negative number of patients
  expect_equal(sum(after$truth$cumulative_deaths), sum(unlist(after$truth$death_history)), info = info)
  expect_true(all(after$hospital$occupancy >= 0), info = info)
  expect_true(all(after$hospital$extra_deaths >= 0), info = info)
  expect_gte(after$lab_backlog, 0)

  # money: never over the per-turn cap or the total pot
  expect_lte(result$turn_spend, tp$tests$budget + 1e-6)
  expect_lte(after$spend_total, tp$budget_total + 1e-6)
  expect_equal(after$spend_total, before$spend_total + result$turn_spend, info = info)

  # tests actually run: never more than asked, than capacity, or (if
  # symptomatic) than the people who came forward; no antigen if none exists
  allocs <- result$allocations
  expect_equal(allocs$borough_name, config$pop_tbl$borough_name, info = info)
  asked <- actions$allocations %||% empty_allocations(config)
  expect_true(all(allocs$n_ag <= asked$n_ag & allocs$n_pcr <= asked$n_pcr), info = info)
  setup <- test_setup(before, config)
  if (!setup$ag_available) {
    expect_equal(sum(allocs$n_ag), 0, info = info)
  }
  if (identical(actions$sampling_mode %||% "symptomatic", "symptomatic")) {
    expect_true(all(allocs$n_ag + allocs$n_pcr <= symptomatic_pool_size(before, config)), info = info)
  }

  # results: positives within tests; real <= positives; infectious <= real
  res <- after$obs$history
  if (nrow(res) > 0) {
    expect_true(all(res$positives >= 0 & res$positives <= res$n), info = info)
    expect_true(all(res$true_pos <= res$positives), info = info)
    expect_true(all(res$infectious_pos <= res$true_pos, na.rm = TRUE), info = info)
    expect_true(all(res$turn_available >= res$turn_sampled), info = info)
    expect_true(all(res$borough_name == config$pop_tbl$borough_name[res$borough_index]), info = info)
  }
  # results come back exactly when due, never early
  expect_true(all(result$available$turn_available <= before$turn), info = info)
  expect_true(all(after$obs$queue$turn_available > before$turn), info = info)

  # reports: never negative, never more deaths than really happened
  rep_now <- after$reports[after$reports$turn == after$turn, ]
  expect_true(all(rep_now$admissions >= 0), info = info)
  expect_true(all(rep_now$deaths <= after$truth$deaths), info = info)

  # local restrictions: streaks count only restricted boroughs
  restricted <- result$restricted
  expect_true(all(after$local_streaks[restricted] >= 1), info = info)
  expect_true(all(after$local_streaks[setdiff(seq_len(n_b), restricted)] == 0), info = info)

  # the player view never carries hidden truth
  view <- player_view(after, config)
  expect_false(any(hidden_view_fields %in% names(view)), info = info)
  expect_true(all(names(view$history) %in% public_result_cols), info = info)
  expect_false(any(c("positives", "true_pos", "infectious_pos") %in% names(view$pending)), info = info)
}

test_that("every turn of every game obeys the rules (all diseases x all bots)", {
  for (disease in diseases) {
    config <- scenario_config(disease, inputs)
    for (bot_name in names(bots)) {
      for (seed in 1:2) {
        state <- new_game(config, seed = seed)
        memory <- list()
        for (i in 1:8) {
          view <- player_view(state, config)
          choice <- bots[[bot_name]](view, memory)
          memory <- choice$memory
          result <- play_turn(state, choice$actions, config)
          actions <- choice$actions
          if (!result$ok) {
            actions <- list()
            result <- play_turn(state, actions, config)
          }
          expect_true(result$ok)
          check_turn(state, result, actions, config, paste(disease, bot_name, seed))
          state <- result$state
        }
      }
    }
  }
})

# 3. The same seed is the same game -------------------------------------------

test_that("a seed always gives exactly the same game", {
  config <- scenario_config("covid_early", inputs)
  a <- play_bot_game(config, bot_chase, seed = 21, n_turns = 8)
  b <- play_bot_game(config, bot_chase, seed = 21, n_turns = 8)
  expect_identical(a$state$truth$history, b$state$truth$history)
  expect_identical(a$state$obs$history, b$state$obs$history)
  expect_identical(a$spend, b$spend)
})

# 4. The spread kernel conserves R --------------------------------------------

test_that("each case causes Rt new cases on average, wherever it is", {
  # Put 1,000 cases in one borough at a time and spread them one turn with no
  # randomness (k = Inf is Poisson; use the expected value mu directly).
  for (K in list(inputs$K, inputs$K_measles)) {
    for (j in seq_len(nrow(K))) {
      cases <- numeric(nrow(K))
      cases[j] <- 1000
      state <- init_truth(nrow(K), seed_boroughs = j, seed_cases = 1000L)
      stepped <- step_truth(state, K, Rt_global = 1.5, k_overdisp = Inf,
                            imports_prob = 0, lambda_import = 0)
      expect_equal(sum(stepped$last_mu), 1500, tolerance = 1e-6)
    }
  }
})

test_that("some spread stays inside a borough and the rest only reaches touching boroughs", {
  expect_true(all(diag(inputs$K) > 0))
  expect_true(all(diag(inputs$K_measles) >= diag(inputs$K)))
})
