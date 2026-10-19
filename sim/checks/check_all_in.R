# Why does All-in early win so often? -----------------------------------------
#
# All-in early mixes two things: spending fast (the full per-turn amount from
# turn 0) and what it spends on (London-wide measures plus testing everyone
# with symptoms). Separate them on the same outbreaks:
#   - All-in early:   measures + symptomatic testing, spend everything allowed
#   - Paced all-in:   same measures + testing, spend a fair share per turn
#   - Hit hard early: measures only
#   - Testing alone:  symptomatic testing everywhere (paced), no measures
#
#   Rscript --vanilla sim/checks/check_all_in.R

suppressMessages({ library(dplyr); library(tibble); library(parallel) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
source("sim/score_heatmap.R")

# the same choices as All-in early, but spending a fair share each turn
bot_paced_all_in <- function(view, memory) {
  view$budget <- view$allowance
  bot_all_in(view, memory)
}

# symptomatic testing everywhere with a fair share, nothing else
bot_testing_alone <- function(view, memory) {
  if (view$testing$ag_available) {
    allocs <- make_allocations(view, n_ag = spread_tests(view$allowance / view$costs$ag, view$pop_tbl$pop))
  } else {
    allocs <- make_allocations(view, n_pcr = spread_tests(view$allowance / view$costs$pcr, view$pop_tbl$pop))
  }
  list(actions = list(allocations = allocs, sampling_mode = "symptomatic"), memory = memory)
}

bots <- list(
  "Do nothing" = bot_do_nothing,
  "All-in early" = bot_all_in,
  "Paced all-in" = bot_paced_all_in,
  "Hit hard early" = bot_hit_hard,
  "Testing alone" = bot_testing_alone
)
diseases <- c("measles_current", "influenza_seasonal", "covid_early", "ebola_like")
jobs <- expand.grid(disease = diseases, bot = names(bots), seed = 1:60, stringsAsFactors = FALSE)

cl <- makeCluster(max(1, detectCores() - 2), rscript_args = "--vanilla")
clusterEvalQ(cl, {
  suppressMessages({ library(dplyr); library(tibble) })
  for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
  inputs <- load_inputs("data")
  NULL
})
clusterExport(cl, c("jobs", "bots", "bot_paced_all_in", "bot_testing_alone"))

results <- bind_rows(parLapply(cl, seq_len(nrow(jobs)), function(i) {
  job <- jobs[i, ]
  config <- scenario_config(job$disease, inputs)
  g <- play_bot_game(config, bots[[job$bot]], seed = job$seed)

  # turn the money ran out (if it did): last turn with any spending
  tibble::tibble(
    disease = job$disease, bot = job$bot, seed = job$seed,
    cumulative_infections = g$cumulative_infections,
    deaths_eventual = g$deaths_eventual,
    spend = g$spend,
    final_incidence = g$final_incidence
  )
}))
stopCluster(cl)

scored <- score_games(results, teaching_params()$budget_total)
scored |>
  group_by(disease, bot) |>
  summarise(
    win_rate = round(mean(win), 2),
    median_infections_vs_nothing = round(median(relative), 2),
    median_spend_k = round(median(spend) / 1000),
    median_final_turn_cases = median(final_incidence),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()
