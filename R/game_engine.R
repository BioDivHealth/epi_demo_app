# Game engine ------------------------------------------------------------------
#
# One game = new_game() then play_turn() once per turn.
# The Shiny app and the strategy bots (sim/) both use these functions, so
# whatever is tuned with the bots is exactly what students play.
#
# What a player is allowed to see is built by player_view(). Bots only ever
# get that view, never the hidden truth.

# Calendar ----------------------------------------------------------------------
#
# Day 0 is the day the first patient is reported; each turn is one report,
# report_days apart (teaching_params.R), whatever the disease.
report_days <- function(config) {
  config$tp$report_days %||% 7L
}

turn_day <- function(turn, config) {
  1L + as.integer(turn) * report_days(config)
}

# Days of the disease's timescale to whole reports (at least `min_turns`)
days_to_turns <- function(days, config, min_turns = 0L) {
  max(as.integer(min_turns), as.integer(round(days / report_days(config))))
}

turn_date <- function(turn, config) {
  as.Date(config$tp$start_date) + turn_day(turn, config)
}

# Length of the response in days, and the day it ends
response_days <- function(config) {
  config$tp$game_turns * report_days(config)
}

response_end_date <- function(config) {
  as.Date(config$tp$start_date) + response_days(config)
}

format_date <- function(date) {
  trimws(format(as.Date(date), "%e %B %Y"))
}

# Scale London's money and capacity to another place ---------------------------
#
# Prices per test stay the same; the budget, per-turn cap, measure costs,
# lab capacity, beds, tracers and isolation support grow with population.
scale_to_place <- function(tp, pop_total) {
  ratio <- pop_total / tp$reference_pop
  if (!is.finite(ratio) || abs(ratio - 1) < 1e-9) {
    return(tp)
  }

  tp$budget_total <- tp$budget_total * ratio
  tp$tests$budget <- tp$tests$budget * ratio
  tp$interventions$cost <- tp$interventions$cost * ratio
  tp$tracing$cost <- tp$tracing$cost * ratio
  tp$tracing$capacity <- round(tp$tracing$capacity * ratio)
  tp$build_testing$cost <- tp$build_testing$cost * ratio
  tp$test_levels$capacity <- round(tp$test_levels$capacity * ratio)
  tp$hospital$beds <- round(tp$hospital$beds * ratio)
  tp$isolation$half_strength_load <- tp$isolation$half_strength_load * ratio
  tp
}

# Build the settings for one game ---------------------------------------------

game_config <- function(
    inputs,
    epi,
    mort = list(death_ifr = 0, death_delay = 0L),
    kernel_key = "default",
    susceptible_frac = 1,
    test_level = 3L,
    bio = list(asymptomatic = 0, presymptomatic = 0, delay_decay = 0.5, hosp_frac = 0.02),
    tp = teaching_params()) {

  # money and capacity scale with the place's population (1 for London)
  tp <- scale_to_place(tp, sum(inputs$pop_tbl$pop))

  # small lookup tables used every turn: plain data frames are much quicker
  tp$test_levels <- as.data.frame(tp$test_levels)
  tp$interventions <- as.data.frame(tp$interventions)

  # imports are per area each turn: keep the place-wide rate like London's
  n_units <- nrow(inputs$pop_tbl)
  epi$imports_prob <- min(1, epi$imports_prob * tp$reference_units / n_units)

  # choose the spatial kernel
  K <- inputs$K
  if (identical(kernel_key, "measles") && !is.null(inputs$K_measles)) {
    K <- inputs$K_measles
  }

  # generations of infection per report: a fast disease goes through several
  # generations between weekly reports, a slow one less than one
  gens_per_turn <- (tp$report_days %||% 7L) / (bio$gen_days %||% (tp$report_days %||% 7L))

  list(
    K = K,
    gens_per_turn = gens_per_turn,
    A = inputs$A,
    pop_tbl = inputs$pop_tbl,
    # share of people susceptible at the start (vaccination, past infection)
    susceptible_frac = min(1, max(0, susceptible_frac)),
    epi = epi,
    mort = mort,
    tp = tp,
    interventions = tp$interventions,
    test_level = test_level,
    bio = bio
  )
}

# Settings for one of the scenario presets in scenarios.R
scenario_config <- function(scenario_id, inputs, tp = teaching_params()) {
  sc <- scenario_lookup(scenario_id)

  epi <- list(
    Rt = sc$Rt[[1]],
    k = sc$k[[1]],
    imports_prob = sc$imports_prob[[1]],
    lambda_import = sc$lambda_import[[1]],
    seed_borough = sc$seed_borough[[1]],
    seed_cases = sc$seed_cases[[1]]
  )

  # delays are given in days; the game counts them in reports
  report_every <- tp$report_days %||% 7L
  mort <- list(
    death_ifr = ifelse(is.na(sc$death_ifr[[1]]), 0.002, sc$death_ifr[[1]]),
    death_delay = as.integer(round(sc$death_delay_days[[1]] / report_every))
  )

  game_config(
    inputs = inputs,
    epi = epi,
    mort = mort,
    kernel_key = sc$kernel_key[[1]],
    susceptible_frac = sc$susceptible_frac[[1]],
    test_level = sc$test_level[[1]],
    bio = list(
      asymptomatic = sc$asymptomatic[[1]],
      presymptomatic = sc$presymptomatic[[1]],
      delay_decay = sc$delay_decay[[1]],
      hosp_frac = sc$hosp_frac[[1]],
      gen_days = sc$gen_days[[1]],
      hosp_delay = max(1L, as.integer(round(sc$hosp_delay_days[[1]] / report_every)))
    ),
    tp = tp
  )
}

# Separate random number streams -----------------------------------------------
#
# The hidden outbreak draws from its own random stream, so for a given seed
# every strategy faces the same outbreak (until its actions change the
# spread). Test results and other draws use the normal stream.

# Run `expr` using the saved stream `rng`; returns list(value, rng)
with_stream <- function(rng, expr) {
  outer <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  assign(".Random.seed", rng, envir = globalenv())

  value <- force(expr)

  rng_after <- get(".Random.seed", envir = globalenv())
  if (is.null(outer)) {
    rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", outer, envir = globalenv())
  }
  list(value = value, rng = rng_after)
}

# A fresh stream started from a seed, without disturbing the current one
new_stream <- function(seed) {
  outer <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
  set.seed(seed)
  rng <- get(".Random.seed", envir = globalenv())
  if (is.null(outer)) {
    rm(".Random.seed", envir = globalenv())
  } else {
    assign(".Random.seed", outer, envir = globalenv())
  }
  rng
}

# Testing settings for one turn, with this game's hidden biases --------------

# The testing level the game is at now (one row of test_levels)
current_test_level <- function(state, config) {
  levels <- config$tp$test_levels
  levels[levels$level == state$test_level, ]
}

# Costs, delays and capacity at the current level
test_setup <- function(state, config) {
  lvl <- current_test_level(state, config)
  build <- config$tp$build_testing

  capacity <- lvl$capacity
  if (state$build_left > 0) {
    capacity <- capacity * build$capacity_while_building
  }

  list(
    level = lvl$level,
    label = lvl$label,
    ag_available = !is.na(lvl$ag_cost),
    costs = list(ag = ifelse(is.na(lvl$ag_cost), 0, lvl$ag_cost), pcr = lvl$pcr_cost),
    delays = list(ag = ifelse(is.na(lvl$ag_delay), 0L, lvl$ag_delay), pcr = lvl$pcr_delay),
    capacity = floor(capacity),
    building = state$build_left > 0,
    build_left = state$build_left,
    can_build = state$build_left == 0 && lvl$level < max(config$tp$test_levels$level),
    build_cost = build$cost,
    build_turns = build$turns
  )
}

# Antigen kits are limited by supply: scale antigen tests down to capacity,
# and drop them if they do not exist yet at this level. PCR is not capped
# here; beyond lab capacity it queues (see lab_backlog_delay()).
cap_to_capacity <- function(allocs, setup) {
  if (!setup$ag_available) {
    allocs$n_ag <- 0L
  }
  total_ag <- sum(allocs$n_ag)
  if (total_ag > setup$capacity) {
    scale <- setup$capacity / total_ag
    allocs$n_ag <- as.integer(floor(allocs$n_ag * scale))
  }
  allocs
}

# Lab backlog: labs process `capacity` PCR tests a turn. Tests waiting from
# earlier turns go first, so this turn's results are late by however many
# extra turns it takes to clear the queue in front of them.
lab_backlog_delay <- function(backlog_before, pcr_today, capacity) {
  extra_delay <- 0L
  if (pcr_today > 0) {
    extra_delay <- as.integer(floor((backlog_before + pcr_today - 1) / capacity))
  }
  list(
    extra_delay = extra_delay,
    backlog_after = max(0, backlog_before + pcr_today - capacity)
  )
}

# People still test-positive in each borough: infections over the last
# `window` turns (this turn included)
detectable_infections <- function(truth, window) {
  history <- truth$history
  recent <- utils::tail(history, window)
  Reduce(`+`, recent)
}

testing_for_turn <- function(state, config, sampling_mode = "symptomatic") {
  tests <- config$tp$tests
  setup <- test_setup(state, config)
  window <- config$tp$detection_window

  testing <- list(
    sens = tests$sens,
    spec = tests$spec,
    delays = setup$delays,
    prev_floor = tests$prev_floor,
    sampling_mode = sampling_mode,
    sampling_pool_frac = config$tp$symptomatic$pool_frac,
    detectable = list(
      ag = detectable_infections(state$truth, window$ag),
      pcr = detectable_infections(state$truth, window$pcr)
    )
  )

  # symptomatic testing is distorted by care-seeking and the illness wave
  if (identical(sampling_mode, "symptomatic") && !is.null(state$biases)) {
    testing$sampling_pool_frac <- symptomatic_pool_frac(state$turn, state$biases$wave_peak, config$tp)
    testing$care_seeking <- state$biases$care_seeking
    testing$symptomatic_share <- 1 - (config$bio$asymptomatic %||% 0)
  }

  testing
}

# Cases isolated this turn because their positive result came back.
# Only real infections count, discounted by how late the result was.
# With contact tracing on, each traced case also takes out some of the
# people it has already infected (about Rt of them), up to the tracers'
# capacity. Tracers follow up every positive, real or false.
isolated_from_results <- function(results, n_boroughs, tp, tracing = FALSE, Rt = 1, bio = list(),
                                  gens_per_turn = 1, admissions = NULL) {
  isolated <- numeric(n_boroughs)

  # with tracing on, hospital-reported cases are followed up too: tracers
  # share their capacity between positives and admissions
  n_admissions <- if (is.null(admissions)) 0 else sum(admissions)
  n_positives <- if (is.null(results)) 0 else sum(results$positives)
  traced_share <- min(1, tp$tracing$capacity / max(1, n_positives + n_admissions))

  if (isTRUE(tracing) && n_admissions > 0) {
    # an admission was infected hosp_delay reports ago: its contacts are
    # worth less the more generations have passed since
    late <- (bio$delay_decay %||% 0.5)^((bio$hosp_delay %||% 1) * gens_per_turn)
    isolated <- isolated + admissions * traced_share * tp$tracing$effectiveness * Rt * late
  }

  if (is.null(results) || nrow(results) == 0) {
    return(isolated)
  }

  iso <- tp$isolation
  delay <- results$turn_available - results$turn_sampled
  # delay_decay is per generation; a report's delay spans gens_per_turn of them
  timeliness <- (bio$delay_decay %||% 0.5)^(delay * gens_per_turn)

  # cases found because they had symptoms have already spread pre-symptoms
  found_by_symptoms <- !is.na(results$mode) & results$mode == "symptomatic"
  timeliness <- timeliness * ifelse(found_by_symptoms, 1 - (bio$presymptomatic %||% 0), 1)

  # isolation is stretched when there are many positives to support
  strain <- 1 / (1 + sum(results$positives) / iso$half_strength_load)
  per_case <- iso$effectiveness * strain

  if (isTRUE(tracing)) {
    per_case <- per_case + traced_share * tp$tracing$effectiveness * Rt
  }

  # only cases still infectious when tested are worth isolating
  still_infectious <- results$infectious_pos
  if (is.null(still_infectious)) {
    still_infectious <- results$true_pos
  }
  still_infectious[is.na(still_infectious)] <- results$true_pos[is.na(still_infectious)]

  effective <- still_infectious * per_case * timeliness

  by_boro <- tapply(effective, results$borough_index, sum)
  idx <- as.integer(names(by_boro))
  isolated[idx] <- isolated[idx] + by_boro
  isolated
}

# Admissions per borough in the latest hospital report
reported_admissions <- function(state, n_boroughs) {
  out <- numeric(n_boroughs)
  rep <- state$reports
  if (is.null(rep) || nrow(rep) == 0) {
    return(out)
  }
  latest <- rep[rep$turn == state$turn, , drop = FALSE]
  if (nrow(latest) == 0) {
    return(out)
  }
  by_boro <- tapply(latest$admissions, latest$borough_index, sum)
  out[as.integer(names(by_boro))] <- by_boro
  out
}

# How many people come forward for a test in each borough this turn
symptomatic_pool_size <- function(state, config) {
  pool_frac <- symptomatic_pool_frac(state$turn, state$biases$wave_peak, config$tp)
  floor(config$pop_tbl$pop * pool_frac)
}

# Symptomatic testing can only test the people who come forward: scale the
# allocation down in any borough where it asks for more tests than that
cap_to_pool <- function(allocs, pool_size) {
  wanted <- allocs$n_ag + allocs$n_pcr
  scale <- ifelse(wanted > pool_size, pool_size / pmax(wanted, 1), 1)
  allocs$n_ag <- as.integer(floor(allocs$n_ag * scale))
  allocs$n_pcr <- as.integer(floor(allocs$n_pcr * scale))
  allocs
}

# Per-turn cost of restricting each borough
local_restriction_costs <- function(config) {
  config$tp$local_restrictions$cost_per_100k * config$pop_tbl$pop / 1e5
}

# Effect of local restrictions this turn, given how long each borough has
# been restricted (streak, including this turn)
local_restriction_multiplier <- function(local_streaks, tp) {
  lr <- tp$local_restrictions
  strength <- vapply(local_streaks, intervention_strength, numeric(1), lag = lr$lag, fatigue = lr$fatigue)
  1 - strength * (1 - lr$Rt_multiplier)
}

# Hospitals (hidden truth) ----------------------------------------------------
#
# Admissions come from infections `hosp_delay` turns ago. Beds are a
# London-wide pool; patients stay `stay_turns`. Patients admitted when there
# is no bed for them die at an extra rate (added to the truth's deaths).
hospital_step <- function(truth, hospital, config) {
  hosp <- config$tp$hospital
  n_b <- nrow(config$pop_tbl)

  # infections that are now sick enough for hospital
  source_turn <- truth$turn - (config$bio$hosp_delay %||% config$tp$reports$hosp_delay)
  admitted <- integer(n_b)
  if (source_turn >= 0) {
    infected <- truth$history[[source_turn + 1]]
    admitted <- stats::rbinom(n_b, size = infected, prob = config$bio$hosp_frac)
  }

  # beds in use = everyone admitted over the last stay_turns turns
  admissions <- c(hospital$admissions, list(admitted))
  occupancy <- sum(unlist(utils::tail(admissions, hosp$stay_turns)))

  # share of patients without a proper bed, and the extra deaths that causes
  overload <- if (occupancy > hosp$beds) (occupancy - hosp$beds) / occupancy else 0
  extra_deaths <- stats::rbinom(n_b, size = admitted, prob = overload * hosp$death_if_no_bed)

  truth$deaths <- as.integer(truth$deaths + extra_deaths)
  truth$cumulative_deaths <- as.integer(truth$cumulative_deaths + extra_deaths)
  truth$death_history[[length(truth$death_history)]] <- truth$deaths

  list(
    truth = truth,
    hospital = list(
      admissions = admissions,
      occupancy = c(hospital$occupancy, occupancy),
      extra_deaths = c(hospital$extra_deaths, sum(extra_deaths))
    ),
    admitted = admitted,
    overload = overload
  )
}

# Hospital admissions and deaths reported this turn -----------------------------
#
# Hospitals report the admissions they record (fewer when they are full);
# most are reported in the patient's own borough, the rest at a neighbouring
# borough's hospital. They also publish how full they are.
draw_reports <- function(truth, config, admitted, overload = 0, offset = 0) {
  rep_par <- config$tp$reports
  n_b <- nrow(config$pop_tbl)
  turn_now <- truth$turn - offset

  ascertainment <- rep_par$hosp_ascertainment * (1 - config$tp$hospital$reporting_drop_when_full * overload)
  admissions_home <- stats::rbinom(n_b, size = admitted, prob = ascertainment)

  # where they are reported: own borough or a neighbour's hospital
  admissions <- integer(n_b)
  for (i in which(admissions_home > 0)) {
    at_home <- stats::rbinom(1, admissions_home[i], rep_par$own_borough)
    admissions[i] <- admissions[i] + at_home
    elsewhere <- admissions_home[i] - at_home
    neighbours <- which(config$A[i, ] > 0)
    if (elsewhere > 0 && length(neighbours) > 0) {
      dest <- neighbours[sample.int(length(neighbours), elsewhere, replace = TRUE)]
      admissions <- admissions + tabulate(dest, nbins = n_b)
    } else {
      admissions[i] <- admissions[i] + elsewhere
    }
  }

  deaths <- stats::rbinom(n_b, size = as.integer(truth$deaths), prob = rep_par$death_ascertainment)

  data.frame(
    turn = as.integer(turn_now),
    borough_index = seq_len(n_b),
    admissions = as.integer(admissions),
    deaths = as.integer(deaths)
  )
}

# Empty allocation table (no tests anywhere)
empty_allocations <- function(config) {
  n_b <- nrow(config$pop_tbl)
  data.frame(
    borough_index = seq_len(n_b),
    borough_name = config$pop_tbl$borough_name,
    n_ag = integer(n_b),
    n_pcr = integer(n_b),
    stringsAsFactors = FALSE
  )
}

# Start a new game -------------------------------------------------------------

new_game <- function(config, seed = NULL, sampling_mode = "symptomatic") {
  if (is.null(seed)) {
    seed <- sample.int(1e6, 1)
  }
  set.seed(seed)

  n_b <- nrow(config$pop_tbl)
  epi <- config$epi

  # where the outbreak starts
  origin <- epi$seed_borough
  if (is.null(origin) || is.na(origin)) {
    origin <- sample.int(n_b, 1L)
  }

  # it runs unseen until a cluster of severe illness is noticed: enough
  # hospital admissions in total, or the first death
  hs <- config$tp$head_start
  gens_per_turn <- config$gens_per_turn %||% 1
  hosp_delay <- config$bio$hosp_delay %||% config$tp$reports$hosp_delay
  hosp_frac <- config$bio$hosp_frac %||% 0

  # the hidden outbreak's own random stream
  truth_rng <- new_stream(seed + 7919L)

  # run the hidden turns from one infected person (an outbreak that dies out
  # before anyone notices is never reported, so draw again)
  for (attempt in 1:50) {
    truth <- init_truth(n_boroughs = n_b, seed_boroughs = origin, seed_cases = hs$index_cases)
    admitted <- numeric(n_b)
    hidden_turns <- 0L

    trigger <- "none"
    for (i in seq_len(hs$max_reports)) {
      stepped <- with_stream(truth_rng, step_truth(
        state = truth,
        K = config$K,
        Rt_global = epi$Rt,
        k_overdisp = epi$k,
        imports_prob = epi$imports_prob,
        lambda_import = epi$lambda_import,
        pop = config$pop_tbl$pop,
        susceptible_frac = config$susceptible_frac,
        death_ifr = config$mort$death_ifr,
        death_delay = config$mort$death_delay,
        gens_per_step = gens_per_turn
      ))
      truth <- stepped$value
      truth_rng <- stepped$rng
      hidden_turns <- i

      # people infected hosp_delay reports ago who are now in hospital
      source_turn <- length(truth$history) - hosp_delay
      if (source_turn >= 1) {
        infected <- truth$history[[source_turn]]
        drawn <- with_stream(truth_rng, stats::rbinom(n_b, size = infected, prob = hosp_frac))
        admitted <- admitted + drawn$value
        truth_rng <- drawn$rng
      }

      # a death, or a cluster of admissions, is noticed: reported now
      if (sum(truth$cumulative_deaths) > 0) {
        trigger <- "death"
        break
      }
      if (sum(admitted) >= hs$admissions) {
        trigger <- "admissions"
        break
      }

      # died out unseen (nothing infectious, no deaths still to come)
      if (sum(truth$incidence) == 0 && sum(unlist(truth$death_queue)) == 0) {
        break
      }
    }

    if (trigger != "none") {
      break
    }
  }

  # if it kept dying out unseen (spread far below 1), the first report is a
  # single fresh spillover patient where it began
  if (trigger == "none") {
    trigger <- "admissions"
    admitted[origin] <- 1
    truth$incidence[origin] <- 1L
    truth$cumulative[origin] <- truth$cumulative[origin] + 1L
    truth$history[[length(truth$history)]] <- truth$incidence
  }

  # the first report comes from one area: where someone died, or the area
  # with the most admissions (ties broken at random). Not necessarily where
  # it began.
  report_counts <- if (trigger == "death") truth$cumulative_deaths else admitted
  top <- which(report_counts == max(report_counts))
  first_patient <- if (length(top) == 1) top else sample(top, 1L)

  state <- list(
    turn = 0L,
    truth = truth,
    # turns of hidden history before the game's turn 0
    truth_offset = hidden_turns,
    # infections and deaths already set in motion before the game began
    start_infections = sum(truth$cumulative),
    start_deaths_eventual = eventual_deaths(truth, config),
    start_deaths = sum(truth$cumulative_deaths),
    obs = init_observation_state(),
    available = init_observation_state()$history,
    biases = draw_game_biases(n_b, config$tp),
    streaks = integer(),
    local_streaks = integer(n_b),
    test_level = config$test_level,
    build_left = 0L,
    lab_backlog = 0,
    reports = data.frame(turn = integer(), borough_index = integer(), admissions = integer(), deaths = integer()),
    hospital = list(admissions = list(), occupancy = numeric(), extra_deaths = numeric()),
    spend_total = 0,
    alert = list(
      alerted = first_patient,
      true_start = origin,
      hidden_turns = hidden_turns,
      trigger = trigger,
      admissions = sum(admitted)
    ),
    seed = seed,
    truth_rng = truth_rng
  )

  # no free tests: the first report is one patient, nothing more
  state$starter_alloc <- empty_allocations(config)
  state
}

# Deaths so far plus deaths still to come from people already infected
eventual_deaths <- function(truth, config) {
  in_pipeline <- sum(unlist(truth$death_queue))
  sum(truth$cumulative_deaths) + in_pipeline * config$mort$death_ifr
}

# Compare a finished game with doing nothing ------------------------------------
#
# Replays the same outbreak (same seed, so the same hidden start and the same
# random spread until actions change it) for the same number of reports with
# no response, and counts infections and deaths during the response in both.
# Deaths include those already set in motion (eventual deaths).
compare_with_doing_nothing <- function(state, config) {
  nothing <- new_game(config, seed = state$seed)
  for (i in seq_len(state$turn)) {
    nothing <- play_turn(nothing, list(), config)$state
  }

  during <- function(s) {
    list(
      infections = sum(s$truth$cumulative) - (s$start_infections %||% 0),
      deaths = max(0, eventual_deaths(s$truth, config) - (s$start_deaths_eventual %||% 0))
    )
  }
  you <- during(state)
  none <- during(nothing)

  # share prevented (negative = worse than doing nothing)
  prevented <- function(a, b) if (b > 0) 1 - a / b else NA_real_

  list(
    infections = you$infections,
    infections_doing_nothing = none$infections,
    deaths = you$deaths,
    deaths_doing_nothing = none$deaths,
    infections_prevented = prevented(you$infections, none$infections),
    deaths_prevented = prevented(you$deaths, none$deaths)
  )
}

# Play one turn ----------------------------------------------------------------
#
# actions: list with
#   allocations   tibble of n_ag / n_pcr per borough (default: none)
#   sampling_mode "symptomatic" or "random"
#   interventions character vector of intervention ids
#
# Returns list(ok, state, ...). If the actions cost more than the budget,
# ok is FALSE and the state is unchanged.

play_turn <- function(state, actions, config) {
  tp <- config$tp

  allocs <- actions$allocations %||% empty_allocations(config)
  sampling_mode <- actions$sampling_mode %||% "symptomatic"
  selected <- actions$interventions %||% character()
  restricted <- unique(as.integer(actions$local_restrictions %||% integer()))
  tracing <- isTRUE(actions$tracing)
  n_b <- nrow(config$pop_tbl)

  # testing level, costs and capacity this turn
  setup <- test_setup(state, config)
  start_build <- isTRUE(actions$build_testing) && setup$can_build
  build_cost <- if (start_build) setup$build_cost else 0
  if (start_build) {
    # capacity drops straight away while staff are diverted
    setup <- test_setup(modifyList(state, list(build_left = setup$build_turns)), config)
  }
  allocs <- cap_to_capacity(allocs, setup)

  # symptomatic testing is limited to the people who come forward
  if (identical(sampling_mode, "symptomatic")) {
    allocs <- cap_to_pool(allocs, symptomatic_pool_size(state, config))
  }

  # local restrictions: per-borough streaks give lag and fatigue
  local_streaks <- ifelse(seq_len(n_b) %in% restricted, state$local_streaks + 1L, 0L)
  local_mult <- local_restriction_multiplier(local_streaks, tp)
  local_cost <- sum(local_restriction_costs(config)[restricted])

  # interventions: lag and fatigue depend on how long each has been on
  streaks <- update_intervention_streaks(state$streaks, selected, config$interventions)
  effects <- compute_intervention_effects(selected, config$interventions, streaks = streaks)

  # spend this turn: capped per turn and by what is left in the pot
  budget <- min(tp$tests$budget, tp$budget_total - state$spend_total)
  spend_stats <- summarise_budget(allocs, setup$costs, budget)
  tracing_cost <- if (tracing) tp$tracing$cost else 0
  turn_spend <- spend_stats$spend + effects$cost + local_cost + tracing_cost + build_cost
  if (turn_spend > budget + 1e-6) {
    return(list(ok = FALSE, reason = "over_budget", state = state))
  }

  # testing this turn (interventions can shorten delays)
  testing <- testing_for_turn(state, config, sampling_mode)
  testing$delays$ag <- max(0, testing$delays$ag + effects$delay_adjust$ag)
  testing$delays$pcr <- max(0, testing$delays$pcr + effects$delay_adjust$pcr)

  # PCR beyond lab capacity waits in a backlog, making results later
  backlog <- lab_backlog_delay(state$lab_backlog %||% 0, sum(allocs$n_pcr), setup$capacity)
  testing$delays$pcr <- testing$delays$pcr + backlog$extra_delay

  obs <- observe_tests(
    state = state$obs,
    turn = state$turn,
    allocations = allocs,
    truth_incidence = state$truth$incidence,
    pop_tbl = config$pop_tbl,
    params = testing
  )

  # cases found this turn are isolated (and traced) before they pass it on
  isolated <- isolated_from_results(
    obs$available, n_b, tp,
    tracing = tracing,
    # each case's onward infections by the next report: effective R
    # (immunity included, not R0), compounded over the report's generations
    Rt = (config$epi$Rt * effects$Rt_multiplier * config$susceptible_frac)^(config$gens_per_turn %||% 1),
    bio = config$bio,
    gens_per_turn = config$gens_per_turn %||% 1,
    # admissions hospitals reported in the latest report (where the hospital is)
    admissions = reported_admissions(state, n_b)
  )
  if (!is.null(state$carry_isolated)) {
    isolated <- isolated + state$carry_isolated
  }

  # hidden outbreak moves on one turn
  epi <- config$epi
  stepped <- with_stream(state$truth_rng, step_truth(
    state = state$truth,
    K = config$K,
    Rt_global = epi$Rt * effects$Rt_multiplier,
    k_overdisp = epi$k,
    imports_prob = min(1, max(0, epi$imports_prob * effects$imports_multiplier)),
    lambda_import = epi$lambda_import,
    pop = config$pop_tbl$pop,
    susceptible_frac = config$susceptible_frac,
    death_ifr = config$mort$death_ifr,
    death_delay = config$mort$death_delay,
    isolated = isolated,
    local_multiplier = local_mult,
    gens_per_step = config$gens_per_turn %||% 1
  ))
  next_truth <- stepped$value

  # hospitals fill up (hidden), on the outbreak's own random stream
  hospital_now <- with_stream(stepped$rng, hospital_step(next_truth, state$hospital, config))
  next_truth <- hospital_now$value$truth

  # label shows how strong each intervention was this turn
  intervention_label <- if (length(effects$labels) == 0) {
    "None"
  } else {
    paste0(effects$labels, " (", round(100 * effects$strength), "%)", collapse = ", ")
  }

  new_state <- state
  new_state$turn <- state$turn + 1L
  new_state$truth <- next_truth
  new_state$truth_rng <- hospital_now$rng
  new_state$hospital <- hospital_now$value$hospital

  # what hospitals and death registrations report this turn
  new_state$reports <- rbind(
    state$reports,
    draw_reports(next_truth, config, hospital_now$value$admitted, hospital_now$value$overload,
                 offset = state$truth_offset %||% 0)
  )
  new_state$carry_isolated <- NULL
  new_state$obs <- list(queue = obs$queue, history = obs$history)
  new_state$available <- obs$available
  new_state$streaks <- streaks
  new_state$local_streaks <- local_streaks

  # building better testing: counts down, then moves up a level
  build_left <- if (start_build) setup$build_turns else state$build_left
  if (build_left > 0) {
    build_left <- build_left - 1L
    if (build_left == 0L) {
      new_state$test_level <- state$test_level + 1L
    }
  }
  new_state$build_left <- build_left
  new_state$lab_backlog <- backlog$backlog_after
  new_state$spend_total <- state$spend_total + turn_spend

  list(
    ok = TRUE,
    state = new_state,
    turn_played = state$turn,
    allocations = allocs,
    available = obs$available,
    queue = obs$queue,
    allocation_spend = spend_stats$spend,
    intervention_cost = effects$cost,
    local_cost = local_cost,
    tracing_cost = tracing_cost,
    build_cost = build_cost,
    test_level = new_state$test_level,
    pcr_extra_delay = backlog$extra_delay,
    restricted = restricted,
    turn_spend = turn_spend,
    remaining = budget - turn_spend,
    effects = effects,
    intervention_label = intervention_label,
    Rt_used = epi$Rt * effects$Rt_multiplier
  )
}

# What a player is allowed to see ---------------------------------------------
#
# Only results that have come back, how many tests are still pending (not
# their results), money, and public facts about the boroughs and tests.

player_view <- function(state, config) {
  tp <- config$tp

  # returned results only, without the hidden columns
  history <- state$obs$history[, c("borough_index", "borough_name", "type", "n", "positives",
                                   "turn_sampled", "turn_available"), drop = FALSE]

  # tests still at the lab, by type (not their results)
  queue <- state$obs$queue
  pending <- if (nrow(queue) == 0) {
    data.frame(type = character(), n = integer(), stringsAsFactors = FALSE)
  } else {
    waiting <- tapply(queue$n, queue$type, sum)
    data.frame(type = names(waiting), n = as.integer(waiting), stringsAsFactors = FALSE)
  }

  # test setup once (it is used three times below)
  setup <- test_setup(state, config)

  # money left, and a fair share of it per remaining turn
  budget_left <- tp$budget_total - state$spend_total
  turns_left <- max(1, tp$game_turns - state$turn)

  list(
    turn = state$turn,
    turns_left = turns_left,
    history = history,
    pending = pending,
    budget = min(tp$tests$budget, budget_left),
    budget_left = budget_left,
    allowance = min(tp$tests$budget, budget_left / turns_left),
    spend_total = state$spend_total,
    testing = setup,
    costs = setup$costs,
    sens = tp$tests$sens,
    spec = tp$tests$spec,
    delays = setup$delays,
    interventions = config$interventions,
    streaks = state$streaks,
    local_costs = local_restriction_costs(config),
    tracing_cost = tp$tracing$cost,
    # labs publish their backlog (PCR tests waiting to be processed)
    lab_backlog = state$lab_backlog %||% 0,
    # how many people came forward for a test this turn (public: test demand)
    pool_size = symptomatic_pool_size(state, config),
    local_streaks = state$local_streaks,
    alerted = if (is.null(state$alert)) NA_integer_ else state$alert$alerted,
    reports = state$reports,
    # hospitals publish how full they are (share of beds in use)
    bed_pressure = if (length(state$hospital$occupancy) == 0) 0 else
      utils::tail(state$hospital$occupancy, 1) / tp$hospital$beds,
    pop_tbl = config$pop_tbl,
    A = config$A
  )
}
