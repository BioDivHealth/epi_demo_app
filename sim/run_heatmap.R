# Strategy heatmap ------------------------------------------------------------
#
# Plays every strategy bot against every disease preset over many random
# outbreaks, and draws one heatmap of how often each strategy "wins".
#
# The win rule is in score_heatmap.R. Each strategy is compared with doing
# nothing on the same outbreak (same seed): the hidden outbreak has its own
# random stream, so the same seed is the same outbreak for every strategy.
#
# Run from the project root:
#   Rscript sim/run_heatmap.R <label> <n_seeds>
# e.g.
#   Rscript sim/run_heatmap.R 0_baseline 100
#
# Writes sim/results/<label>.csv and sim/heatmap_<label>.png

suppressMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(parallel)
})

args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args) >= 1) args[1] else "latest"
n_seeds <- if (length(args) >= 2) as.integer(args[2]) else 50L
# "same_days": each disease plays as many days as 20 generations (the old
# game length) instead of game_turns reports, to separate the effect of the
# report cadence from the effect of the game's length
same_days <- length(args) >= 3 && identical(args[3], "same_days")


# load the game code and inputs
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")
N_TURNS <- teaching_params()$game_turns

diseases <- setdiff(scenario_catalog()$id, "custom")
disease_labels <- setNames(scenario_catalog()$label, scenario_catalog()$id)
bot_names <- names(strategy_bots())
all_bots <- c(strategy_bots(), "Do nothing" = bot_do_nothing)

# every disease x bot x seed
jobs <- expand.grid(
  disease = diseases,
  bot = names(all_bots),
  seed = seq_len(n_seeds),
  stringsAsFactors = FALSE
)

# play the games in parallel ---------------------------------------------------

run_job <- function(i) {
  job <- jobs[i, ]
  config <- scenario_config(job$disease, inputs)
  bot <- c(strategy_bots(), "Do nothing" = bot_do_nothing)[[job$bot]]

  n_turns <- N_TURNS
  if (SAME_DAYS) {
    n_turns <- max(1L, as.integer(round(N_TURNS * config$bio$gen_days / report_days(config))))
    config$tp$game_turns <- n_turns   # bots pace their budget over this
  }
  game <- play_bot_game(config, bot, seed = job$seed, n_turns = n_turns)

  tibble(
    disease = job$disease,
    bot = job$bot,
    seed = job$seed,
    cumulative_infections = game$cumulative_infections,
    final_incidence = game$final_incidence,
    deaths = game$deaths,
    deaths_eventual = game$deaths_eventual,
    extra_deaths_no_bed = game$extra_deaths_no_bed,
    peak_bed_use = game$peak_bed_use,
    spend = game$spend
  )
}

# --vanilla stops workers picking up the project .Rprofile (renv)
cl <- makeCluster(max(1, detectCores() - 2), rscript_args = "--vanilla")
clusterEvalQ(cl, {
  suppressMessages({ library(dplyr); library(tibble) })
  for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
  inputs <- load_inputs("data")
  NULL
})
SAME_DAYS <- same_days
clusterExport(cl, c("jobs", "N_TURNS", "SAME_DAYS"))

t0 <- Sys.time()
results <- bind_rows(parLapply(cl, seq_len(nrow(jobs)), run_job))
stopCluster(cl)
cat("Played", nrow(jobs), "games in", round(as.numeric(Sys.time() - t0, units = "secs")), "s\n")

dir.create("sim/results", showWarnings = FALSE)
write.csv(results, file.path("sim/results", paste0(label, ".csv")), row.names = FALSE)

# score and draw (win rule lives in score_heatmap.R) ----------------------------

source("sim/score_heatmap.R")
scored <- score_games(results, teaching_params()$budget_total)
plot_heatmap(scored, label, disease_labels, bot_names, N_TURNS)
