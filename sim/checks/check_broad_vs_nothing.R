# Can broad surveillance ever make an outbreak worse than doing nothing? ------
#
# It only isolates real cases and restricts boroughs, which can only reduce
# spread. But once anything differs, the outbreak's random draws diverge, so
# individual outbreaks can still end up bigger by chance. Check, per seed,
# how often and by how much, and whether the very first turns (before any
# divergence) are identical.
#
#   Rscript --vanilla sim/checks/check_broad_vs_nothing.R

suppressMessages({ library(dplyr); library(tibble) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")
config <- scenario_config("covid_early", inputs)

rows <- list()
for (seed in 1:60) {
  nothing <- play_bot_game(config, bot_do_nothing, seed = seed)
  broad <- play_bot_game(config, bot_broad, seed = seed)

  # first turn where the hidden outbreaks differ
  h1 <- nothing$state$truth$history
  h2 <- broad$state$truth$history
  differs <- which(!mapply(identical, h1, h2))
  rows[[seed]] <- tibble(
    seed = seed,
    ratio = broad$cumulative_infections / nothing$cumulative_infections,
    first_difference_turn = if (length(differs)) min(differs) - 1 else NA,
    isolated_any = sum(broad$state$obs$history$infectious_pos, na.rm = TRUE) > 0,
    restricted_any = any(broad$state$local_streaks > 0)
  )
}
res <- bind_rows(rows)

cat("Median infections ratio (broad / nothing):", round(median(res$ratio), 3), "\n")
cat("Share of outbreaks where broad ended >10% bigger:", round(mean(res$ratio > 1.1), 2), "\n")
cat("Share where broad ended >10% smaller:", round(mean(res$ratio < 0.9), 2), "\n")
cat("Outbreaks identical all game:", sum(is.na(res$first_difference_turn)), "\n")
cat("Median first turn the outbreaks differ:", median(res$first_difference_turn, na.rm = TRUE), "\n")
cat("Seeds that got bigger but where broad never isolated or restricted anyone:",
    sum(res$ratio > 1.0001 & !res$isolated_any & !res$restricted_any), "\n")
