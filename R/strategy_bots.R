# Strategy bots ----------------------------------------------------------------
#
# Each bot plays the game the way a student following one strategy would.
# A bot only gets player_view() (what a student can see), never the truth.
#
# A bot is function(view, memory) returning list(actions, memory).
# `memory` lets a bot remember things between turns (starts as list()).
#
# Bots use whichever control levers the current game offers, so they keep
# working as new levers are added.

# Helpers ----------------------------------------------------------------------

# Positives and tests per borough in results returned over the last `window` turns
recent_results <- function(view, window = 2) {
  n_b <- nrow(view$pop_tbl)

  recent <- view$history |>
    dplyr::filter(turn_available >= view$turn - window)

  positives <- integer(n_b)
  tests <- integer(n_b)
  if (nrow(recent) > 0) {
    by_boro <- recent |>
      dplyr::group_by(borough_index) |>
      dplyr::summarise(positives = sum(positives), tests = sum(n), .groups = "drop")
    positives[by_boro$borough_index] <- by_boro$positives
    tests[by_boro$borough_index] <- by_boro$tests
  }

  list(positives = positives, tests = tests)
}

# Boroughs whose positivity is clearly above what false positives alone
# would give (a crude "is this signal real?" check)
signal_boroughs <- function(view, window = 2, type_spec = NULL) {
  res <- recent_results(view, window)

  # worst-case false positive rate across the tests in use
  spec <- type_spec %||% min(unlist(view$spec))
  fp_rate <- 1 - spec

  expected_fp <- res$tests * fp_rate
  # signal = more positives than false positives could plausibly explain
  which(res$positives >= 2 & res$positives > expected_fp + 2 * sqrt(expected_fp + 1))
}

# Money left for tests after the chosen interventions and local restrictions
test_budget <- function(view, interventions, restricted = integer()) {
  cost <- sum(view$interventions$cost[view$interventions$id %in% interventions])
  cost <- cost + sum(view$local_costs[restricted])
  max(0, (view$allowance %||% view$budget) - cost)
}

# Keep only as many boroughs (in the order given) as the allowance can pay
# to restrict, leaving `keep_for_tests` of the allowance for testing
affordable_restrictions <- function(view, boroughs, keep_for_tests = 0.3) {
  money <- (view$allowance %||% view$budget) * (1 - keep_for_tests)
  costs <- view$local_costs[boroughs]
  boroughs[cumsum(costs) <= money]
}

# Spread a number of tests across boroughs in proportion to weights
spread_tests <- function(n_tests, weights) {
  weights <- weights / sum(weights)
  floor(n_tests * weights)
}

# Build an allocation table
make_allocations <- function(view, n_ag = 0L, n_pcr = 0L) {
  n_b <- nrow(view$pop_tbl)
  tibble::tibble(
    borough_index = seq_len(n_b),
    borough_name = view$pop_tbl$borough_name,
    n_ag = as.integer(rep_len(n_ag, n_b)),
    n_pcr = as.integer(rep_len(n_pcr, n_b))
  )
}

# The bots ---------------------------------------------------------------------

# 1. Chase the signal: pour tests (and any local controls) into wherever
#    positives turn up; follow the starter alert until then.
bot_chase <- function(view, memory) {
  res <- recent_results(view, window = 1)

  if (sum(res$positives) > 0) {
    target <- which.max(res$positives)
  } else if (!is.null(memory$target)) {
    target <- memory$target
  } else {
    target <- view$alerted
  }
  if (is.na(target)) {
    target <- sample.int(nrow(view$pop_tbl), 1)
  }
  memory$target <- target

  # 70% of tests in the target, 30% shared across its neighbours
  neighbours <- which(view$A[target, ] > 0)
  weights <- numeric(nrow(view$pop_tbl))
  weights[target] <- 0.7
  weights[neighbours] <- 0.3 / max(1, length(neighbours))

  # respond once positives are seen: lock down the target borough
  restricted <- if (sum(res$positives) > 0) affordable_restrictions(view, target) else integer()

  # cheapest fast test available
  money <- test_budget(view, character(), restricted)
  if (view$testing$ag_available) {
    n_ag <- spread_tests(money / view$costs$ag, weights)
    allocs <- make_allocations(view, n_ag = n_ag)
  } else {
    n_pcr <- spread_tests(money / view$costs$pcr, weights)
    allocs <- make_allocations(view, n_pcr = n_pcr)
  }

  list(
    actions = list(
      allocations = allocs,
      sampling_mode = "symptomatic",
      interventions = character(),
      local_restrictions = restricted
    ),
    memory = memory
  )
}

# 2. Broad surveillance: random sample across all boroughs every turn, act
#    once the signal rises above the false-positive noise.
bot_broad <- function(view, memory) {
  # restrict boroughs where the signal is clearly real, strongest first
  signal <- signal_boroughs(view, window = 2)
  res <- recent_results(view, window = 2)
  signal <- signal[order(-res$positives[signal])]
  restricted <- affordable_restrictions(view, signal, keep_for_tests = 0.5)

  # cheap fast antigen if it exists (accepting false positives), else PCR,
  # spread by population
  money <- test_budget(view, character(), restricted)
  if (view$testing$ag_available) {
    allocs <- make_allocations(view, n_ag = spread_tests(money / view$costs$ag, view$pop_tbl$pop))
  } else {
    allocs <- make_allocations(view, n_pcr = spread_tests(money / view$costs$pcr, view$pop_tbl$pop))
  }

  list(
    actions = list(
      allocations = allocs,
      sampling_mode = "random",
      interventions = character(),
      local_restrictions = restricted
    ),
    memory = memory
  )
}

# 3. Test, trace, isolate: steady symptomatic PCR testing everywhere, plus
#    contact tracing.
bot_tti <- function(view, memory) {
  interventions <- character()
  tracing_cost <- view$tracing_cost %||% 0

  money <- test_budget(view, interventions) - tracing_cost
  n_pcr <- spread_tests(max(0, money) / view$costs$pcr, view$pop_tbl$pop)

  list(
    actions = list(
      allocations = make_allocations(view, n_pcr = n_pcr),
      sampling_mode = "symptomatic",
      interventions = interventions,
      tracing = TRUE
    ),
    memory = memory
  )
}

# 4. Build testing first: keep investing in better testing until the top
#    level, testing symptomatic people widely with whatever exists meanwhile.
bot_build <- function(view, memory) {
  build <- isTRUE(view$testing$can_build)
  money <- test_budget(view, character()) - if (build) view$testing$build_cost else 0

  n_pcr <- spread_tests(max(0, money) / view$costs$pcr, view$pop_tbl$pop)

  list(
    actions = list(
      allocations = make_allocations(view, n_pcr = n_pcr),
      sampling_mode = "symptomatic",
      interventions = character(),
      build_testing = build
    ),
    memory = memory
  )
}

# 5. Hit hard early: London-wide measures from turn 0, spend little on tests.
bot_hit_hard <- function(view, memory) {
  interventions <- c("distancing", "quarantine")

  list(
    actions = list(
      allocations = make_allocations(view),
      sampling_mode = "symptomatic",
      interventions = interventions
    ),
    memory = memory
  )
}

# 5b. All-in early: like Hit hard early, but spends the full per-turn amount
#     from turn 0 (London-wide measures plus wide symptomatic testing) with no
#     pacing, until the money runs out.
bot_all_in <- function(view, memory) {
  interventions <- c("distancing", "quarantine")

  # spend everything allowed this turn, not a fair share of what is left
  money <- view$budget - sum(view$interventions$cost[view$interventions$id %in% interventions])
  if (money < 0) {
    # cannot even pay for the measures any more
    return(list(actions = list(), memory = memory))
  }

  if (view$testing$ag_available) {
    allocs <- make_allocations(view, n_ag = spread_tests(money / view$costs$ag, view$pop_tbl$pop))
  } else {
    allocs <- make_allocations(view, n_pcr = spread_tests(money / view$costs$pcr, view$pop_tbl$pop))
  }

  list(
    actions = list(
      allocations = allocs,
      sampling_mode = "symptomatic",
      interventions = interventions
    ),
    memory = memory
  )
}

# 6. Watch and wait: no testing; rely on hospital reports and restrict the
#    boroughs where admissions turn up.
bot_watch <- function(view, memory) {
  recent <- view$reports |>
    dplyr::filter(turn >= view$turn - 1) |>
    dplyr::group_by(borough_index) |>
    dplyr::summarise(admissions = sum(admissions), .groups = "drop") |>
    dplyr::filter(admissions > 0) |>
    dplyr::arrange(dplyr::desc(admissions))

  restricted <- affordable_restrictions(view, recent$borough_index, keep_for_tests = 0)

  list(
    actions = list(
      allocations = make_allocations(view),
      sampling_mode = "symptomatic",
      interventions = character(),
      local_restrictions = restricted
    ),
    memory = memory
  )
}

# Reference: do nothing at all. Every strategy is scored against this on the
# same outbreak. Not shown as a strategy.
bot_do_nothing <- function(view, memory) {
  list(actions = list(), memory = memory)
}

strategy_bots <- function() {
  list(
    "Chase the signal" = bot_chase,
    "Broad surveillance" = bot_broad,
    "Test, trace, isolate" = bot_tti,
    "Build testing first" = bot_build,
    "Hit hard early" = bot_hit_hard,
    "All-in early" = bot_all_in,
    "Watch and wait" = bot_watch
  )
}

# Play a whole game with one bot ------------------------------------------------

play_bot_game <- function(config, bot, seed, n_turns = 20L) {
  state <- new_game(config, seed = seed)
  memory <- list()

  for (i in seq_len(n_turns)) {
    view <- player_view(state, config)
    choice <- bot(view, memory)
    memory <- choice$memory

    result <- play_turn(state, choice$actions, config)
    if (!result$ok) {
      # over budget: play the turn doing nothing instead
      result <- play_turn(state, list(), config)
    }
    state <- result$state
  }

  history <- state$truth$history

  # infections and deaths during the response (what happened before the
  # first report is the same whatever the player does)
  deaths_eventual <- eventual_deaths(state$truth, config) - (state$start_deaths_eventual %||% 0)

  list(
    cumulative_infections = sum(state$truth$cumulative) - (state$start_infections %||% 0),
    final_incidence = sum(history[[length(history)]]),
    deaths = sum(state$truth$cumulative_deaths),
    deaths_eventual = deaths_eventual,
    extra_deaths_no_bed = sum(state$hospital$extra_deaths),
    peak_bed_use = if (length(state$hospital$occupancy)) max(state$hospital$occupancy) / config$tp$hospital$beds else 0,
    spend = state$spend_total,
    state = state
  )
}
