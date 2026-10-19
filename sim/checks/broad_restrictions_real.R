# Are broad surveillance's flu restrictions aimed at real hotspots? -----------
#
# With 60,000 random antigen tests a turn, ~2% come back falsely positive.
# For each borough the bot restricts, check whether it was really among the
# most infected boroughs that turn.
#
#   Rscript --vanilla sim/checks/broad_restrictions_real.R

suppressMessages({ library(dplyr); library(tibble) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")

config <- scenario_config("influenza_seasonal", inputs)
rows <- list()

for (seed in 1:20) {
  state <- new_game(config, seed = seed)
  memory <- list()
  for (i in 1:12) {
    view <- player_view(state, config)
    choice <- bot_broad(view, memory)
    memory <- choice$memory
    result <- play_turn(state, choice$actions, config)
    if (!result$ok) result <- play_turn(state, list(), config)

    inc <- state$truth$incidence
    for (b in result$restricted) {
      rows[[length(rows) + 1]] <- tibble(
        seed = seed, turn = state$turn,
        had_cases = inc[b] > 0,
        rank_among_33 = rank(-inc, ties.method = "min")[b]
      )
    }
    state <- result$state
  }
}

res <- bind_rows(rows)
cat("Restrictions placed:", nrow(res), "\n")
cat("Share on boroughs with any infections:", round(mean(res$had_cases), 2), "\n")
cat("Share on a top-5 borough:", round(mean(res$rank_among_33 <= 5), 2), "\n")
