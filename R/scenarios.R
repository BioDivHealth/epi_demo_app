# Scenario presets ------------------------------------------------------------
#
# One row per disease. Each one is meant to reward a different strategy, so
# that no single approach always works:
#   measles (current vaccination)  only just spreading: watch, act locally
#   measles (low vaccination)      explosive: near impossible to contain
#   COVID-like                     silent spread: cannot be contained, only slowed
#   Ebola-like                     severe, slow, often dies out: hospital reports
#   Nipah-like                     a cluster that fizzles: act on hospital cases, hold back
#   SARS-like                      spreads only once sick: find and isolate the sick
#   Mpox-like                      slow, close contact: contact tracing
#   MERS-like                      hospital clusters: hospital reports, act locally
#   Pandemic flu                   fast, deadly wave: flatten it, protect hospitals
#   Smallpox-like                  rash before spread: ring response (trace, isolate)
#   Polio-like                     99% silent: only random sampling sees it
#
# Columns:
#   Rt               R per generation if nobody were immune (R0); effective R at
#                    the start is Rt x susceptible_frac
#   k                superspreading (small = a few people cause most spread)
#   imports_prob     chance of an introduction from outside per area per
#   lambda_import    report, and its mean size. All 0: the game is one
#                    outbreak from one infected person (introductions would
#                    restart outbreaks that had died out and add clusters
#                    unrelated to it)
#   susceptible_frac share who can catch it at the start (vaccination, past infection)
#   death_ifr        share of infected people who die
#   death_delay_days days from infection to death
#   test_level       PCR at the start: 1 reference lab, 2 hospital labs
#                    (rapid antigen tests have to be developed)
#   asymptomatic     share of infections with no symptoms (never come forward)
#   presymptomatic   share of a case's spread before symptoms (a case found
#                    through its symptoms has already done this much damage)
#   delay_decay      isolation value left after each generation of result delay
#   hosp_frac        share of infections admitted to hospital (always reported)
#   gen_days         days per generation of infection
#   hosp_delay_days  days from infection to hospital admission
#   kernel_key       spatial kernel ("measles" = less mobility)

scenario_catalog <- function() {
  diseases <- tibble::tribble(
    ~id,               ~Rt,  ~k,   ~imports_prob, ~lambda_import, ~susceptible_frac, ~death_ifr, ~death_delay_days, ~test_level, ~asymptomatic, ~presymptomatic, ~delay_decay, ~hosp_frac, ~gen_days, ~hosp_delay_days, ~kernel_key,
    "custom",          1.3,  0.5,  0,             0.5,            1,                 0.002,      20,                2L,          0.2,           0.3,             0.5,          0.02,       5L,        10,               "default",
    "measles_current", 15,   1.5,  0,             0.2,            0.08,              0.001,      24,                2L,          0.05,          0.3,             0.6,          0.1,        12L,       12,               "measles",
    "measles_low_vax", 15,   1.5,  0,             0.2,            0.2,               0.001,      24,                2L,          0.05,          0.3,             0.6,          0.1,        12L,       12,               "measles",
    "covid_early",     2.0,  0.25, 0,             1.0,            0.9,               0.002,      20,                2L,          0.45,          0.45,            0.4,          0.03,       5L,        10,               "default",
    "ebola_like",      1.5,  0.4,  0,             0.2,            1.0,               0.5,        15,                1L,          0.05,          0,               0.85,         0.5,        15L,       15,               "default",
    "nipah_like",      0.8,  0.1,  0,             0.2,            1.0,               0.6,        20,                1L,          0.1,           0.1,             0.8,          0.6,        10L,       10,               "default",
    "sars_like",       2.5,  0.16, 0,             0.3,            1.0,               0.1,        24,                1L,          0.1,           0.05,            0.8,          0.6,        8L,        8,                "default",
    "mpox_like",       1.6,  0.3,  0,             0.5,            0.8,               0.001,      25,                2L,          0.1,           0.2,             0.8,          0.05,       10L,       14,               "default",
    "mers_like",       1.3,  0.1,  0,             0.2,            1.0,               0.35,       15,                1L,          0.2,           0.05,            0.8,          0.7,        7L,        7,                "default",
    "flu_pandemic",    2.0,  1.0,  0,             1.0,            1.0,               0.02,       14,                2L,          0.3,           0.3,             0.35,         0.05,       3L,        7,                "default",
    "smallpox_like",   4.0,  0.6,  0,             0.2,            0.5,               0.3,        21,                1L,          0,             0,               0.9,          0.5,        15L,       14,               "default",
    "polio_like",      6.0,  1.0,  0,             0.5,            0.25,              0.0005,     30,                2L,          0.99,          0.5,             0.7,          0.005,      12L,       14,               "default"
  )

  # names and what each one was (the blurb is shown at the start of the review)
  text <- tibble::tribble(
    ~id, ~label, ~objective, ~blurb,
    "custom", "Custom (no preset)", "Use default parameters or adjust manually.",
    "A new respiratory infection with moderate spread.",
    "measles_current", "Measles-like (current vaccination)", "High transmissibility with current vaccination levels and reduced mobility.",
    paste(
      "A measles-like virus: extremely contagious through the air (each case would infect about 15 people",
      "if nobody were immune). Almost everyone infected becomes ill, and cases spread it for a few days",
      "before the rash. Most people here are vaccinated (8% can still catch it), so it only just keeps spreading."
    ),
    "measles_low_vax", "Measles-like (low vaccination)", "High transmissibility with low vaccination coverage and reduced mobility.",
    paste(
      "A measles-like virus: extremely contagious through the air (each case would infect about 15 people",
      "if nobody were immune). Almost everyone infected becomes ill, and cases spread it for a few days",
      "before the rash. Vaccination here is low (20% can catch it), enough for explosive spread."
    ),
    "covid_early", "COVID-like (early, superspreading)", "Superspreading with higher imports and fast early growth.",
    paste(
      "A new coronavirus, COVID-like. Nearly half of infections have no symptoms, much of the spread",
      "happens before symptoms start, and a few people infect very many others (superspreading).",
      "At first only hospital labs could test for it, slowly."
    ),
    "ebola_like", "Ebola-like (sustained, high mortality)", "Sustained transmission with high mortality and moderate clustering.",
    paste(
      "An Ebola-like virus: spread through close contact with very sick people, with almost no spread",
      "before symptoms, so isolating cases works well. Half of those infected die. At first only a",
      "reference lab could test for it."
    ),
    "nipah_like", "Nipah-like (spillover cluster)", "Low average spread with strong clustering, from one jump from animals.",
    paste(
      "A Nipah-like virus: it jumped from an animal into one person, and spreads little between",
      "people (each case infects fewer than one other on average) but kills most of those it",
      "infects. Clusters like this usually die out on their own."
    ),
    "sars_like", "SARS-like (spreads once sick)", "Fast spread with superspreading, but almost none before symptoms.",
    paste(
      "A SARS-like coronavirus: each case would infect two or three others, a few infect many, and",
      "most need hospital care. Almost all spread happens after symptoms start, so finding and",
      "isolating sick people works: the real SARS outbreak of 2003 was stopped this way."
    ),
    "mpox_like", "Mpox-like (slow, close contact)", "Slow spread through close contact, with clear symptoms.",
    paste(
      "An mpox-like virus: spread by close contact, slowly (a new generation every 10 days or so),",
      "and most cases have a visible rash. Few need hospital care and very few die, so hospital",
      "reports say little: following up contacts is what works. Some older people are protected",
      "by past smallpox vaccination."
    ),
    "mers_like", "MERS-like (hospital clusters)", "Little community spread; outbreaks grow in clusters around hospitals.",
    paste(
      "A MERS-like coronavirus: most cases are severe and end up in hospital, a third of them die,",
      "and most spread happens in clusters where very sick patients are cared for (a few cases",
      "infect many). Hospital reports show it clearly; acting where the clusters are works."
    ),
    "flu_pandemic", "Pandemic flu (1918-like)", "Fast, deadly influenza that nobody is immune to.",
    paste(
      "A new pandemic influenza, like 1918: nobody is immune, a new generation of infections every",
      "3 days, much spread before symptoms, and 2% of those infected die. It cannot be contained;",
      "the aim is to slow the wave so hospitals are not overwhelmed."
    ),
    "smallpox_like", "Smallpox-like (rash before spread)", "Severe, slow, with no spread before the rash.",
    paste(
      "A smallpox-like virus: each case would infect about four others, but only after the rash",
      "appears, and a generation takes about two weeks. Older people are protected by past",
      "vaccination (half can still catch it). A third of those infected die. Tracing and isolating",
      "the contacts of every case (a ring response) is how smallpox was eradicated."
    ),
    "polio_like", "Polio-like (silent spread)", "Almost every infection is silent; very few cases are ever seen.",
    paste(
      "A polio-like virus: highly contagious, but 99 in 100 infections cause no symptoms and only",
      "about 1 in 200 ends in hospital (paralysis). Testing people with symptoms or waiting for",
      "hospital reports misses almost all of it; only testing a random sample of people finds it.",
      "A quarter of people here are unvaccinated."
    )
  )

  diseases |>
    dplyr::left_join(text, by = "id") |>
    dplyr::mutate(
      # kept for older code paths: the outbreak now starts from one person
      seed_borough = NA_integer_,
      seed_cases = 1L
    )
}

scenario_lookup <- function(id, catalog = scenario_catalog()) {
  if (is.null(id) || length(id) == 0 || is.na(id)) {
    id <- "custom"
  }
  match_idx <- match(id, catalog$id)
  if (is.na(match_idx)) {
    match_idx <- match("custom", catalog$id)
  }
  catalog[match_idx, , drop = FALSE]
}

scenario_choices <- function(catalog = scenario_catalog()) {
  stats::setNames(catalog$id, catalog$label)
}
