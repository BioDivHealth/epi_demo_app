# What is achievable, and what tends to work ------------------------------------
#
# After a game the review compares the player with the best approaches the
# strategy bots found for that disease (benchmarks/avertable.csv, made by
# sim/make_benchmarks.R from a long run of every way of playing), and gives
# one piece of advice for that type of disease.

# The stored benchmarks: one row per disease (NULL if the file is missing)
load_benchmarks <- function(path = file.path("benchmarks", "avertable.csv")) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

# What type of disease this is, from its traits, and what tends to work.
# The same rules a student could learn: how well it spreads, how many cases
# reach hospital, how much is silent, and how fast it moves.
disease_advice <- function(scenario) {
  r_start <- scenario$Rt[[1]] * scenario$susceptible_frac[[1]]
  hosp <- scenario$hosp_frac[[1]]
  silent <- scenario$asymptomatic[[1]]
  gen_days <- scenario$gen_days[[1]]

  if (r_start < 1) {
    list(
      type = "infections spread poorly between people",
      advice = paste(
        "Most clusters like this die out on their own. Act on the hospital cases you hear about,",
        "and do not spend heavily chasing an outbreak that is already fading."
      )
    )
  } else if (hosp >= 0.3) {
    list(
      type = "most cases end up in hospital",
      advice = paste(
        "Hospital reports show where it is, for free. Restrict the areas with clusters and trace",
        "the contacts of every known case: this can stop it."
      )
    )
  } else if (silent >= 0.9) {
    list(
      type = "almost every infection is silent",
      advice = paste(
        "People with symptoms miss it, and random samples rarely find it. The few hospital cases are",
        "the only reliable sign: act locally wherever they appear, and expect to see only the tip."
      )
    )
  } else if (gen_days <= 5 || r_start >= 2.5) {
    list(
      type = "infections spread too fast to contain",
      advice = paste(
        "It cannot be contained, only slowed. A lockdown timed for when hospitals start to fill",
        "prevents the most deaths, with local action and testing alongside where you can."
      )
    )
  } else {
    list(
      type = "infections spread slowly and mostly show symptoms",
      advice = paste(
        "There is time to act. Developing rapid tests and tracing the contacts of cases can catch",
        "most of it."
      )
    )
  }
}
