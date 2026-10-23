# Audit of earlier re-weightings -----------------------------------------------
#
# A. Respond to hospital reports (then called "Watch and wait"): before the rebalance it won 53% (COVID) / 85% (Ebola).
#    Was that a sensible mechanism (hospital reports point at real hotspots,
#    restrictions there work) or an artefact? Compare old and new settings.
# B. Contact tracing barely helped. Was that the result delay (as assumed),
#    tracer capacity, or something else?
#
#   Rscript --vanilla sim/checks/audit_reweights.R

suppressMessages({ library(dplyr); library(tibble) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")
n_turns <- teaching_params()$game_turns

# settings before the rebalance
tp_old <- teaching_params()
tp_old$local_restrictions$Rt_multiplier <- 0.4
tp_old$local_restrictions$cost_per_100k <- 6000
tp_old$reports$hosp_delay <- 2L
tp_old$reports$own_borough <- 0.7

# A. Respond to hospital reports -------------------------------------------------------------

audit_watch <- function(disease, tp, label, seeds = 1:30) {
  rows <- list()
  for (seed in seeds) {
    config <- scenario_config(disease, inputs, tp = tp)
    state <- new_game(config, seed = seed)
    memory <- list()
    for (i in seq_len(n_turns)) {
      view <- player_view(state, config)
      choice <- bot_hospital_reports(view, memory)
      memory <- choice$memory
      result <- play_turn(state, choice$actions, config)
      if (!result$ok) result <- play_turn(state, list(), config)

      inc <- state$truth$incidence
      top5 <- order(-inc)[1:5]
      if (length(result$restricted) > 0) {
        rows[[length(rows) + 1]] <- tibble(
          setting = label, disease = disease, seed = seed, turn = state$turn,
          n_restricted = length(result$restricted),
          share_on_top5 = mean(result$restricted %in% top5),
          share_of_cases_covered = sum(inc[result$restricted]) / max(1, sum(inc))
        )
      }
      state <- result$state
    }
  }
  bind_rows(rows)
}

watch <- bind_rows(
  audit_watch("covid_early", tp_old, "before"),
  audit_watch("covid_early", teaching_params(), "after"),
  audit_watch("ebola_like", tp_old, "before"),
  audit_watch("ebola_like", teaching_params(), "after")
)

cat("\n== A. Respond to hospital reports: where do its restrictions land? ==\n")
watch |>
  group_by(disease, setting) |>
  summarise(
    first_restriction_turn = median(tapply(turn, seed, min)),
    boroughs_restricted_per_turn = round(mean(n_restricted), 1),
    share_on_true_top5 = round(mean(share_on_top5), 2),
    share_of_new_cases_covered = round(mean(share_of_cases_covered), 2),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

# B. Contact tracing -----------------------------------------------------------

audit_tracing <- function(disease, seeds = 1:20) {
  rows <- list()
  for (seed in seeds) {
    config <- scenario_config(disease, inputs)
    state <- new_game(config, seed = seed)
    memory <- list()
    for (i in seq_len(n_turns)) {
      view <- player_view(state, config)
      choice <- bot_tti(view, memory)
      memory <- choice$memory
      result <- play_turn(state, choice$actions, config)
      if (!result$ok) result <- play_turn(state, list(), config)

      avail <- result$available
      if (nrow(avail) > 0) {
        delay <- avail$turn_available - avail$turn_sampled
        rows[[length(rows) + 1]] <- tibble(
          disease = disease, seed = seed, turn = state$turn,
          positives = sum(avail$positives),
          real_infectious_found = sum(avail$infectious_pos),
          new_infections = sum(state$truth$incidence),
          traced_share = min(1, config$tp$tracing$capacity / max(1, sum(avail$positives))),
          timeliness = mean(config$bio$delay_decay^delay *
                              ifelse(avail$mode == "symptomatic", 1 - config$bio$presymptomatic, 1)),
          tests_run = sum(result$allocations$n_ag + result$allocations$n_pcr)
        )
      }
      state <- result$state
    }
  }
  bind_rows(rows)
}

tracing <- bind_rows(lapply(c("measles_current", "influenza_seasonal", "covid_early"), audit_tracing))

cat("\n== B. Test, trace, isolate: what limits it? ==\n")
tracing |>
  group_by(disease) |>
  summarise(
    tests_per_turn = round(median(tests_run)),
    share_of_new_infections_found = round(sum(real_infectious_found) / sum(new_infections), 3),
    turns_over_tracer_capacity = round(mean(traced_share < 1), 2),
    value_left_after_delay = round(mean(timeliness), 2),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()
