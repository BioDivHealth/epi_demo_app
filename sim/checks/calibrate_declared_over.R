# When can an outbreak be declared over? ---------------------------------------
#
# The game should end by itself once the outbreak is over. It can only use
# what is reported whatever the player does: hospital admissions and deaths
# (test positives never stop while anyone tests, because of false
# positives). Rule: declare it over after N reports in a row with no
# admissions and no deaths reported.
#
# For N = 1 to 10, across every disease and every way of playing, how often
# is the rule right when it fires? Two tests:
#   no infections  - no true infections then or for the rest of a long game
#                    (strict: imports from outside keep sparking new ones)
#   never back     - no admissions or deaths reported for the rest of the
#                    game (what anyone could ever check)
# Pick the smallest N where "never back" holds at least 95% of the time.
#
#   Rscript --vanilla sim/checks/calibrate_declared_over.R

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(parallel)
})

for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())

n_seeds <- 40
long_game <- 40   # play on well past the rule firing, to see if it comes back
quiet_options <- 1:10

diseases <- setdiff(scenario_catalog()$id, "custom")
bots <- c(strategy_bots(), "Do nothing" = bot_do_nothing)
jobs <- expand.grid(disease = diseases, bot = names(bots), seed = seq_len(n_seeds), stringsAsFactors = FALSE)

# one game: per report, admissions + deaths reported, and true new infections
run_job <- function(i) {
  job <- jobs[i, ]
  config <- scenario_config(job$disease, inputs)
  bot <- c(strategy_bots(), "Do nothing" = bot_do_nothing)[[job$bot]]
  state <- new_game(config, seed = job$seed)
  memory <- list()
  reported <- numeric(long_game)
  infections <- numeric(long_game)
  agency_pos <- numeric(long_game)
  agency_tests <- numeric(long_game)
  for (t in seq_len(long_game)) {
    view <- player_view(state, config)
    choice <- bot(view, memory)
    memory <- choice$memory
    result <- play_turn(state, choice$actions, config)
    if (!result$ok) result <- play_turn(state, list(), config)
    state <- result$state
    rep <- state$reports[state$reports$turn == state$turn, ]
    reported[t] <- sum(rep$admissions) + sum(rep$deaths)
    infections[t] <- sum(state$truth$incidence)
    sen <- state$sentinel
    if (!is.null(sen) && nrow(sen) > 0) {
      agency_pos[t] <- sen$positives[nrow(sen)]
      agency_tests[t] <- sen$tests[nrow(sen)]
    }
  }
  data.frame(disease = job$disease, bot = job$bot, seed = job$seed,
             report = seq_len(long_game), reported = reported, infections = infections,
             agency_pos = agency_pos, agency_tests = agency_tests)
}

cl <- makeCluster(max(1, detectCores() - 2), rscript_args = "--vanilla")
clusterEvalQ(cl, {
  suppressMessages(library(dplyr))
  for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
  inputs <- load_inputs("data")
  NULL
})
clusterExport(cl, c("jobs", "long_game"))
games <- bind_rows(parLapply(cl, seq_len(nrow(jobs)), run_job))
stopCluster(cl)
saveRDS(games, "sim/results/declared_over_games.rds")

# for each game and each N: the first report where N quiet reports in a row
# have passed, and whether the outbreak really was over then (no infections
# that report or any later one)
calls <- games |>
  group_by(disease, bot, seed) |>
  group_modify(function(g, key) {
    quiet_run <- ave(g$reported == 0, cumsum(g$reported != 0), FUN = cumsum)
    bind_rows(lapply(quiet_options, function(n) {
      fired <- which(quiet_run >= n)[1]
      if (is.na(fired)) {
        return(data.frame(n = n, fired = FALSE, over = NA, never_back = NA))
      }
      later <- fired:nrow(g)
      data.frame(n = n, fired = TRUE, over = all(g$infections[later] == 0),
                 never_back = all(g$reported[later] == 0))
    }))
  }) |>
  ungroup()

overall <- calls |>
  filter(fired) |>
  group_by(n) |>
  summarise(no_infections = mean(over), right = mean(never_back), games_ended = n(), .groups = "drop")
by_disease <- calls |>
  filter(fired) |>
  group_by(disease, n) |>
  summarise(no_infections = mean(over), right = mean(never_back), .groups = "drop")

print(as.data.frame(overall))
chosen <- if (any(overall$right >= 0.95)) min(overall$n[overall$right >= 0.95]) else max(quiet_options)
cat("\nSmallest N right at least 95% of the time:", chosen, "\n")
print(as.data.frame(by_disease |> filter(n %in% c(chosen, 3, 6)) |> mutate(across(c(no_infections, right), ~ round(100 * .x))) |> arrange(n, disease)))

p <- ggplot(by_disease, aes(x = n, y = right, colour = disease)) +
  geom_line(alpha = 0.6) +
  geom_line(data = overall, aes(x = n, y = right), colour = "black", linewidth = 1.2, inherit.aes = FALSE) +
  geom_hline(yintercept = 0.95, linetype = "dashed") +
  geom_vline(xintercept = chosen, linetype = "dotted") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = quiet_options) +
  labs(
    title = "How often a declared-over outbreak never comes back",
    subtitle = sprintf("Rule: N reports in a row with no hospital admissions or deaths. 'Never back': none reported for the rest of a %d-report game.
Black: all diseases. Dashed: 95%%. %d outbreaks per disease and way of playing.", long_game, n_seeds),
    x = "Quiet reports in a row (N)", y = "Really over", colour = NULL
  ) +
  theme_minimal(base_size = 11)
ggsave("sim/checks/declared_over.png", p, width = 11, height = 5.5, dpi = 110)
cat("Wrote sim/checks/declared_over.png\n")
