# Scenario presets ------------------------------------------------------------

scenario_catalog <- function() {
  tibble::tibble(
    id = c(
      "custom",
      "measles_current",
      "measles_low_vax",
      "covid_early",
      "influenza_seasonal",
      "ebola_nipah"
    ),
    label = c(
      "Custom (no preset)",
      "Measles-like (current vaccination)",
      "Measles-like (low vaccination)",
      "COVID-like (early, superspreading)",
      "Influenza-like (seasonal)",
      "Ebola/Nipah-like (high mortality)"
    ),
    objective = c(
      "Use default parameters or adjust manually.",
      "High transmissibility with current vaccination levels and reduced mobility.",
      "High transmissibility with low vaccination coverage and reduced mobility.",
      "Superspreading with higher imports and fast early growth.",
      "Moderate spread with steady imports; sampling bias still matters.",
      "High mortality with clustered transmission and slower growth."
    ),
    Rt = c(1.3, 2.8, 2.8, 1.8, 1.3, 1.2),
    k = c(0.5, 1.5, 1.5, 0.25, 0.6, 0.1),
    imports_prob = c(0.01, 0.01, 0.01, 0.04, 0.03, 0.006),
    lambda_import = c(0.5, 0.2, 0.2, 1.0, 0.6, 0.2),
    seed_borough = c(
      NA_integer_, NA_integer_, NA_integer_,
      NA_integer_, NA_integer_, NA_integer_
    ),
    seed_cases = c(100L, 30L, 30L, 40L, 60L, 15L),
    kernel_key = c("default", "measles", "measles", "default", "default", "default"),
    susceptible_frac = c(1, 0.2, 0.85, 0.9, 0.8, 1.0),
    death_ifr = c(NA_real_, 0.001, 0.001, 0.002, 0.001, 0.5),
    death_delay = c(NA_integer_, NA_integer_, NA_integer_, 8L, 8L, 10L)
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
