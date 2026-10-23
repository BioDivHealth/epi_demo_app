# Teaching parameters ----------------------------------------------------------
#
# Every value that controls game balance lives here, so the difficulty and the
# "ways the data can mislead you" can be tuned in one place before a class.
#
# Each block says which trap it sets up for students.

teaching_params <- function() {
  list(

    # New Game draws a fresh random outbreak by default, so students cannot
    # memorise where it starts from a previous round. Untick "Random" in the
    # app (or set this to FALSE) to give a whole class the same outbreak.
    random_seed_default = TRUE,

    # Money is in a made-up unit so it does not suggest any real country's
    # prices. Change the name here to change it everywhere.
    currency = "credits",

    # All money and capacity values below are set for London (33 boroughs,
    # about 9.1 million people). In another place they are scaled by its
    # population (a bigger place has a bigger health budget, more labs and
    # beds), and imports by its number of areas. See scale_to_place().
    reference_pop = 9089736,

    # The outbreak starts unseen, from one infected person, and is noticed
    # when a cluster of severe illness turns up: `admissions` hospital
    # admissions in total, or the first death. An outbreak that dies out
    # before that is never noticed, so a new one is drawn. Mild, fast
    # spreaders are bigger by the time anyone notices; deadly ones are
    # noticed early. The report comes from where the patients are, not
    # necessarily where it started.
    # Trap: the first case found is not the first case.
    head_start = list(admissions = 3L, index_cases = 1L, max_reports = 104L),

    # The outbreak is declared over (and the response ends by itself) after
    # this many reports in a row with no hospital admissions and no deaths
    # reported. Calibrated in sim/checks/calibrate_declared_over.R: right
    # (it never comes back) 97% of the time across all diseases; silent
    # polio-like is the exception. Test positives do not count: false
    # positives never stop while anyone tests.
    declare_over_after = 6L,

    # Reports until the reference lab identifies the disease (then its name
    # is shown). Before that, students have to judge it from the data.
    identify_after = 6L,

    # Days between reports (one turn). Every disease is played at this
    # cadence: 7 = a weekly report, 1 = daily. Spread, hospital and death
    # delays are converted from the disease's own timescale (scenarios.R).
    # Money, lab capacity, test delays and measure lags below are per report.
    report_days = 7L,

    # Date the first patient is reported (day 0). Day 1 is the first turn.
    start_date = "2027-10-04",
    reference_units = 33L,

    # Starter intel: an early alert sends 200 antigen tests to one borough.
    # The alert is only right some of the time; otherwise it points at a
    # borough picked at random (a false alarm).
    # Trap: early signals and rumours can send you to the wrong place.
    starter_alert = list(
      tests = 200L,
      accuracy = 0.6
    ),

    # Test performance, delays and costs.
    # Antigen is cheap and fast but its specificity is lower than PCR, so
    # mass antigen testing where prevalence is low produces mostly false
    # positives.
    # Trap: positive predictive value depends on how common the infection is.
    tests = list(
      sens = list(ag = 0.80, pcr = 0.98),
      spec = list(ag = 0.98, pcr = 0.999),
      delays = list(ag = 0L, pcr = 1L),
      costs = list(ag = 2, pcr = 10),
      budget = 300000,        # most that can be spent in one report (credits)
      prev_floor = 1e-6
    ),

    # Detection window: a test stays positive for a while after infection,
    # so a random sample finds people infected over the last few turns, not
    # just this turn (PCR picks up old infections for longer than antigen).
    # Only the ones infected this turn are still spreading, so only they
    # count for isolation. Symptomatic testing finds people who are sick
    # now, so it is not affected.
    # Trap: lots of positives can mean an outbreak that has already passed.
    detection_window = list(ag = 2L, pcr = 3L),

    # One pot of money for the whole outbreak, and how long the game lasts.
    # Spending big early leaves less for later.
    # Trap: you cannot both see everything and control everything.
    game_turns = 20L,
    budget_total = 2400000,

    # Isolation of confirmed cases.
    # Every true case found by a test is isolated, cutting onward spread.
    # How much survives a slow result depends on the disease (delay_decay
    # in scenarios.R), and cases found through symptomatic testing have
    # already done their pre-symptom spreading (presymptomatic).
    # Trap: a slow result finds a case after it has already spread.
    isolation = list(
      effectiveness = 0.8,
      # Isolation gets harder to support as positives pile up (checks,
      # support payments, enforcement, households). At `half_strength_load`
      # positives a turn, each isolation works half as well. Every positive
      # counts, real or false.
      half_strength_load = 2000
    ),

    # Hospital capacity.
    # A London-wide pool of beds for this disease. Patients stay `stay_turns`.
    # When more patients need a bed than there are beds, the share without a
    # proper bed die at an extra rate, and hospitals under pressure report
    # fewer admissions. Hospitals publish how full they are.
    # Set at a teaching level (not real bed numbers): severe diseases fill
    # it if left alone, mild ones do not.
    # Trap: "wait for the hospital data" is fine until hospitals are full.
    hospital = list(
      beds = 600,
      stay_turns = 2L,
      death_if_no_bed = 0.3,
      reporting_drop_when_full = 0.5
    ),

    # Reports that arrive whatever the player does: hospital admissions and
    # deaths. Admissions are reported where the hospital is (usually, not
    # always, the patient's own borough), some time after infection, and
    # not all are recognised as this disease.
    # Trap: hospital data is late, incomplete and in the wrong place.
    reports = list(
      hosp_delay = 3L,        # fallback only: each disease sets its own (scenarios.R)
      hosp_ascertainment = 0.8,
      own_borough = 0.5,
      death_ascertainment = 0.9
    ),

    # Testing.
    # PCR comes in two levels (set per disease in scenarios.R): a reference
    # lab only, or hospital labs (cheaper, more capacity). "Improve PCR"
    # moves up a level: a lump sum, a few reports, and half capacity while
    # staff are diverted. capacity = PCR tests the labs can process per
    # report across London; more than that joins a backlog and comes back
    # late. PCR results always come back with the next report.
    # Trap: ordering more tests than labs can process makes every result late.
    test_levels = tibble::tibble(
      level = 1:2,
      label = c("Reference lab only", "Hospital labs"),
      pcr_cost = c(40, 20),
      pcr_delay = c(1L, 1L),
      capacity = c(2000, 6000)
    ),
    build_testing = list(
      cost = 150000,
      turns = 3L,
      capacity_while_building = 0.5
    ),

    # Rapid antigen tests do not exist for a new disease: they have to be
    # developed, paying `cost` every report for `turns` reports. Then they are cheap,
    # instant and plentiful (`kits` a report), and because people can test
    # themselves at home, a lockdown works better (`lockdown_Rt_multiplier`
    # replaces social distancing's usual effect).
    # Trap: invest now in a tool you can only use later.
    rapid_tests = list(
      cost = 40000,       # per report while being developed
      turns = 8L,
      ag_cost = 2,
      kits = 60000,
      lockdown_Rt_multiplier = 0.4
    ),

    # National public health agency: every report it tests the same number of
    # people with symptoms, spread across all areas by population, free to
    # the player: a positivity figure that does not move when the player
    # changes their own testing. The agency's Rt comes from hospital
    # admissions (a fixed share of cases, whatever anyone tests), the
    # steadiest signal. Information only (nobody is isolated because of it).
    sentinel = list(tests = 1000),

    # Contact tracing.
    # A team that follows up known cases and quarantines their contacts,
    # removing some of the people each case has already infected. Known
    # cases are your positive tests and the admissions hospitals report
    # (which cost nothing to find, but are found late: an admission was
    # infected hosp_delay ago, so for a fast disease its contacts have long
    # moved on). Limited capacity per turn.
    # Trap: tracing looks great when cases are few (an early Ebola-like
    # outbreak), then silently fails when cases outnumber tracers (and you
    # cannot see the ones you never found).
    tracing = list(
      cost = 40000,          # per turn while switched on
      capacity = 400,        # known cases followed up per report
      effectiveness = 0.5,   # share of each case's onward infections caught
      contacts_per_case = 10 # people named and checked for each case (for the report)
    ),

    # Local restrictions on single boroughs.
    # Infected people in a restricted borough pass it on less (anywhere).
    # Much cheaper than London-wide measures, but only useful in the right
    # place. Cost scales with the borough's population. Same lag/fatigue
    # rules as the London-wide interventions.
    # Only `max_pop_share` of the population can be under local restrictions
    # at once (always at least one area): police, support and goodwill run
    # out, so you have to choose which clusters matter most.
    # Trap: acting hard on an early positive is great if it was the origin,
    # wasted if it was a false positive or a side branch.
    local_restrictions = list(
      max_pop_share = 0.1,
      Rt_multiplier = 0.3,
      cost_per_100k = 9000,
      lag = 1L,
      fatigue = 0.9
    ),

    # Symptomatic testing pool.
    # Testing people with symptoms finds more cases than random testing, but
    # (1) people in some boroughs are less likely to come forward for a test
    #     (hidden care-seeking multiplier, drawn fresh each game), and
    # (2) a wave of some other respiratory illness swells the number of
    #     people with symptoms partway through the game, diluting positivity.
    # Random testing is unaffected by both but finds far fewer cases.
    # Traps: who gets tested shapes the map; positivity can fall while
    # infections rise.
    symptomatic = list(
      pool_frac = 0.005,               # share of population seeking a test each turn (baseline);
                                       # symptomatic tests per borough are capped at this many people
      care_seeking_range = c(0.3, 1.5), # per-borough multiplier, uniform draw
      wave_height = 2,                 # pool grows to (1 + height) x baseline at peak
      wave_width = 2,                  # turns (sd of a Gaussian bump)
      wave_peak_range = c(4L, 12L)      # peak turn, drawn each game
    ),

    # Interventions.
    # Each one costs budget every turn it is ticked, only starts working the
    # turn after it is switched on (lag), and some lose effect the longer
    # they run (fatigue: effect multiplied by `fatigue` each extra turn).
    # Once switched off, the fading wears off one report per report.
    # Social distancing (lockdown) is only possible once the public would
    # accept it: after `lockdown_trigger$deaths` reported deaths, or with
    # hospital beds `lockdown_trigger$bed_share` full.
    lockdown_trigger = list(deaths = 10, bed_share = 0.25),

    # Traps: you cannot judge an intervention on the turn you start it, and
    # money spent on control is money not spent on seeing the outbreak.
    interventions = tibble::tibble(
      id = "distancing",
      label = "Social distancing",
      cost = 150000,
      Rt_multiplier = 0.5,
      imports_multiplier = 1,
      ag_delay_adjust = 0,
      pcr_delay_adjust = 0,
      lag = 1L,
      fatigue = 0.8
    )
  )
}

# Draw the hidden biases for one game -----------------------------------------

draw_game_biases <- function(n_boroughs, params = teaching_params()) {
  sym <- params$symptomatic

  care_seeking <- stats::runif(
    n_boroughs,
    min = sym$care_seeking_range[1],
    max = sym$care_seeking_range[2]
  )

  wave_peak <- sample(
    seq(sym$wave_peak_range[1], sym$wave_peak_range[2]),
    size = 1
  )

  list(
    care_seeking = care_seeking,
    wave_peak = wave_peak
  )
}

# Size of the symptomatic pool on a given turn ---------------------------------

symptomatic_pool_frac <- function(turn, wave_peak, params = teaching_params()) {
  sym <- params$symptomatic

  # Gaussian bump centred on the wave peak
  bump <- exp(-((turn - wave_peak)^2) / (2 * sym$wave_width^2))

  sym$pool_frac * (1 + sym$wave_height * bump)
}

# Format an amount of money for display, e.g. "60,000 credits" -----------------

format_money <- function(x) {
  paste(format_round(x), teaching_params()$currency)
}

# A round, readable number: 77,460 -> "80,000" (see nice_round())
format_round <- function(x) {
  format(nice_round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}
