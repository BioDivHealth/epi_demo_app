# Scenario presets ------------------------------------------------------------

scenario_catalog <- function() {
  tibble::tibble(
    id = c(
      "custom",
      "measles_current",
      "measles_low_vax",
      "covid_early",
      "influenza_seasonal",
      "ebola_like",
      "nipah_like"
    ),
    label = c(
      "Custom (no preset)",
      "Measles-like (current vaccination)",
      "Measles-like (low vaccination)",
      "COVID-like (early, superspreading)",
      "Influenza-like (seasonal)",
      "Ebola-like (sustained, high mortality)",
      "Nipah-like (clustered spillovers)"
    ),
    objective = c(
      "Use default parameters or adjust manually.",
      "High transmissibility with current vaccination levels and reduced mobility.",
      "High transmissibility with low vaccination coverage and reduced mobility.",
      "Superspreading with higher imports and fast early growth.",
      "Moderate spread with steady imports; sampling bias still matters.",
      "Sustained transmission with high mortality and moderate clustering.",
      "Low average spread with strong clustering and repeated introductions."
    ),
    # R per turn in a fully susceptible population (R0); the effective R at
    # the start is Rt x susceptible_frac. Measles: R0 ~15, so 8% susceptible
    # (current coverage) sits just above 1 and 20% susceptible is explosive.
    Rt = c(1.3, 15, 15, 2.0, 1.6, 1.5, 0.8),
    k = c(0.5, 1.5, 1.5, 0.25, 0.6, 0.4, 0.1),
    imports_prob = c(0.01, 0.01, 0.01, 0.04, 0.03, 0.004, 0.01),
    lambda_import = c(0.5, 0.2, 0.2, 1.0, 0.6, 0.2, 0.2),
    seed_borough = c(
      NA_integer_, NA_integer_, NA_integer_,
      NA_integer_, NA_integer_, NA_integer_,
      NA_integer_
    ),
    seed_cases = c(100L, 30L, 30L, 40L, 60L, 20L, 15L),
    kernel_key = c("default", "measles", "measles", "default", "default", "default", "default"),
    susceptible_frac = c(1, 0.08, 0.2, 0.9, 0.8, 1.0, 1.0),
    death_ifr = c(NA_real_, 0.001, 0.001, 0.002, 0.001, 0.5, 0.6),
    death_delay = c(NA_integer_, NA_integer_, NA_integer_, 8L, 8L, 10L, 10L),
    # testing level at the start (see teaching_params()$test_levels):
    # 1 reference lab only, 2 hospital labs, 3 established, 4 rapid everywhere
    test_level = c(3L, 3L, 3L, 2L, 3L, 1L, 1L),
    # share of infections with no symptoms (never come forward for a test)
    asymptomatic = c(0.2, 0.05, 0.05, 0.45, 0.35, 0.05, 0.1),
    # share of a case's spread that happens before symptoms, so a case found
    # through symptomatic testing has already done that much damage
    presymptomatic = c(0.3, 0.3, 0.3, 0.45, 0.4, 0, 0.1),
    # how much isolation/tracing value survives each turn of result delay
    # (low = fast spreader, a slow result is nearly worthless)
    delay_decay = c(0.5, 0.6, 0.6, 0.4, 0.35, 0.85, 0.8),
    # share of infections admitted to hospital (what gets reported anyway)
    hosp_frac = c(0.02, 0.1, 0.1, 0.03, 0.01, 0.5, 0.6)
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
