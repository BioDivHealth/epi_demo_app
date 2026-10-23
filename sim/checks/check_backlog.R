# Does any strategy actually build up a lab backlog? ---------------------------
#
# For each disease and strategy, the average extra turns PCR results waited
# because labs were over capacity, and the largest backlog reached.
#
#   Rscript --vanilla sim/checks/check_backlog.R

suppressMessages({ library(dplyr); library(tibble) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")
n_turns <- teaching_params()$game_turns

rows <- list()
for (d in c("covid_early", "ebola_like", "influenza_seasonal")) {
  config <- scenario_config(d, inputs)
  for (b in names(strategy_bots())) {
    for (seed in 1:5) {
      state <- new_game(config, seed = seed)
      memory <- list()
      for (i in seq_len(n_turns)) {
        choice <- strategy_bots()[[b]](player_view(state, config), memory)
        memory <- choice$memory
        result <- play_turn(state, choice$actions, config)
        if (!result$ok) result <- play_turn(state, list(), config)
        rows[[length(rows) + 1]] <- tibble(
          disease = d, bot = b, seed = seed, turn = state$turn,
          pcr = sum(result$allocations$n_pcr),
          extra_delay = result$pcr_extra_delay,
          backlog = result$state$lab_backlog
        )
        state <- result$state
      }
    }
  }
}

bind_rows(rows) |>
  group_by(disease, bot) |>
  summarise(
    pcr_per_turn = round(mean(pcr)),
    mean_extra_delay = round(mean(extra_delay), 2),
    max_backlog = max(backlog),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()
