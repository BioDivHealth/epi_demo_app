# Lever test ---------------------------------------------------------------------
#
# What does each control do on its own? For every disease, play simple
# one-lever policies on the same outbreaks as doing nothing, and report the
# share of infections and deaths each prevents, and what it costs.
#
# A control that prevents nothing anywhere is not worth having; a control
# that wins everywhere makes the game trivial. Each disease should favour
# different levers.
#
# Run from the project root:
#   Rscript --vanilla sim/lever_test.R <label> <n_seeds>
#
# Writes sim/results/levers_<label>.csv and sim/checks/levers_<label>.png

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(parallel)
})

args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args) >= 1) args[1] else "latest"
n_seeds <- if (length(args) >= 2) as.integer(args[2]) else 40L

for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())

# The policies ---------------------------------------------------------------------
#
# Each is a bot: function(view, memory) -> list(actions, memory). Testing
# policies spend a fair share of what is left each report on PCR (antigen
# once it exists), spread by population.

fair_tests <- function(view, cost_of_measures = 0) {
  money <- max(0, (view$allowance %||% view$budget) - cost_of_measures)
  use_ag <- isTRUE(view$testing$ag_available)
  per_test <- if (use_ag) view$costs$ag else view$costs$pcr
  # never more than the labs (or kit supply) can handle: overloading the
  # labs only makes every result late
  n_total <- min(money / per_test, if (use_ag) view$testing$ag_capacity else view$testing$capacity)
  n <- spread_tests(n_total, view$pop_tbl$pop)
  if (use_ag) make_allocations(view, n_ag = n) else make_allocations(view, n_pcr = n)
}

# areas with hospital admissions in the latest report
admission_areas <- function(view) {
  rep <- view$reports
  if (is.null(rep) || nrow(rep) == 0) return(integer())
  latest <- rep[rep$turn == max(rep$turn) & rep$admissions > 0, ]
  unique(latest$borough_index)
}

policies <- list(
  "Do nothing" = function(view, memory) list(actions = list(), memory = memory),

  "Test the sick" = function(view, memory) {
    list(actions = list(allocations = fair_tests(view), sampling_mode = "symptomatic"), memory = memory)
  },

  "Test at random" = function(view, memory) {
    list(actions = list(allocations = fair_tests(view), sampling_mode = "random"), memory = memory)
  },

  "Test the sick + trace" = function(view, memory) {
    list(actions = list(allocations = fair_tests(view, view$tracing_cost), sampling_mode = "symptomatic",
                        tracing = TRUE), memory = memory)
  },

  "Develop rapid tests, then test" = function(view, memory) {
    develop <- isTRUE(view$testing$can_develop_rapid)
    cost <- if (develop || isTRUE(view$testing$developing_rapid)) view$testing$rapid_cost else 0
    list(actions = list(allocations = fair_tests(view, cost), sampling_mode = "symptomatic",
                        develop_rapid = develop), memory = memory)
  },

  "Test the sick where hospitals report" = function(view, memory) {
    # all of this report's tests go to areas with hospital cases (to everywhere
    # until there are any)
    areas <- admission_areas(view)
    allocs <- fair_tests(view)
    if (length(areas) > 0) {
      weights <- ifelse(seq_len(nrow(view$pop_tbl)) %in% areas, view$pop_tbl$pop, 0)
      total <- sum(allocs$n_ag + allocs$n_pcr)
      n <- spread_tests(total, weights)
      allocs <- if (sum(allocs$n_ag) > 0) make_allocations(view, n_ag = n) else make_allocations(view, n_pcr = n)
    }
    list(actions = list(allocations = allocs, sampling_mode = "symptomatic"), memory = memory)
  },

  "Trace + restrict hospital clusters" = function(view, memory) {
    areas <- affordable_restrictions(view, admission_areas(view), keep_for_tests = 0.5)
    list(actions = list(local_restrictions = areas, tracing = TRUE), memory = memory)
  },

  "Restrict hospital clusters" = function(view, memory) {
    areas <- affordable_restrictions(view, admission_areas(view), keep_for_tests = 0)
    list(actions = list(local_restrictions = areas), memory = memory)
  },

  "Lockdown as soon as allowed" = function(view, memory) {
    list(actions = list(interventions = "distancing"), memory = memory)
  },

  "Lockdown when beds half full" = function(view, memory) {
    on <- isTRUE(memory$locked) || view$bed_pressure >= 0.5
    memory$locked <- on
    list(actions = list(interventions = if (on) "distancing" else character()), memory = memory)
  }
)

# the strategy bots too, so one run shows every way of playing
policies <- c(policies, strategy_bots())

diseases <- setdiff(scenario_catalog()$id, "custom")
disease_labels <- setNames(scenario_catalog()$label, scenario_catalog()$id)
jobs <- expand.grid(disease = diseases, policy = names(policies), seed = seq_len(n_seeds),
                    stringsAsFactors = FALSE)

run_job <- function(i) {
  job <- jobs[i, ]
  config <- scenario_config(job$disease, inputs)
  game <- play_bot_game(config, policies[[job$policy]], seed = job$seed,
                        n_turns = config$tp$game_turns)
  data.frame(disease = job$disease, policy = job$policy, seed = job$seed,
             infections = game$cumulative_infections, deaths = game$deaths_eventual,
             spend = game$spend)
}

cl <- makeCluster(max(1, detectCores() - 2), rscript_args = "--vanilla")
clusterEvalQ(cl, {
  suppressMessages(library(dplyr))
  for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
  inputs <- load_inputs("data")
  NULL
})
clusterExport(cl, c("jobs", "policies", "fair_tests", "admission_areas"))
t0 <- Sys.time()
results <- bind_rows(parLapply(cl, seq_len(nrow(jobs)), run_job))
stopCluster(cl)
cat(sprintf("Played %d games in %.0f s\n", nrow(results), as.numeric(Sys.time() - t0, units = "secs")))

dir.create("sim/results", showWarnings = FALSE)
write.csv(results, file.path("sim/results", paste0("levers_", label, ".csv")), row.names = FALSE)

# Win rate (the heatmap's rule) and share prevented, against doing nothing ------

source("sim/score_heatmap.R")
budget_total <- teaching_params()$budget_total
scored <- score_games(
  results |> rename(bot = policy, cumulative_infections = infections, deaths_eventual = deaths),
  budget_total
)

summary_tbl <- scored |>
  mutate(prevented = ifelse(reference > 0, 1 - cumulative_infections / reference, NA)) |>
  group_by(disease, policy = bot) |>
  summarise(
    win_rate = mean(win),
    prevented = median(prevented, na.rm = TRUE),
    spend = median(spend),
    .groups = "drop"
  )

# the best way to win each disease
best <- summary_tbl |>
  group_by(disease) |>
  slice_max(win_rate, n = 2, with_ties = FALSE) |>
  ungroup() |>
  mutate(disease = disease_labels[disease], win_rate = round(100 * win_rate), prevented = round(100 * prevented))
cat("
Best two ways to win each disease (win %, median % of infections prevented):
")
print(as.data.frame(best), row.names = FALSE)

policy_order <- setdiff(names(policies), "Do nothing")
plot_tbl <- summary_tbl |>
  mutate(
    disease = factor(disease_labels[disease], levels = disease_labels[diseases]),
    policy = factor(policy, levels = policy_order)
  )

heat <- function(fill_col, text_fn, title, limits, low, mid, high, file) {
  p <- ggplot(plot_tbl, aes(x = policy, y = disease, fill = .data[[fill_col]])) +
    geom_tile(colour = "white") +
    geom_text(aes(label = text_fn(plot_tbl)), size = 2.8) +
    scale_fill_gradient2(low = low, mid = mid, high = high, midpoint = mean(limits), limits = limits,
                         labels = scales::percent) +
    labs(title = title, x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())
  ggsave(file, p, width = 16, height = 7, dpi = 110)
  cat("Wrote", file, "
")
}

heat("win_rate",
     function(d) sprintf("%d%%
%sk", round(100 * d$win_rate), format(round(d$spend / 1000), big.mark = ",")),
     sprintf("What is winnable, and how: win rate of each way of playing (%d outbreaks each; label: win rate, median spend)", n_seeds),
     c(0, 1), "#f7f7f7", "#9ecae1", "#08519c",
     file.path("sim/checks", paste0("winnable_", label, ".png")))

# What is avertable: the best any way of playing achieves for each disease ------
#
# For each disease, the way of playing that prevents the largest median share
# of infections, with the deaths it prevents and the infections averted.

avertable <- scored |>
  mutate(
    prevented = ifelse(reference > 0, 1 - cumulative_infections / reference, NA),
    deaths_prevented = ifelse(reference_deaths >= 1, 1 - deaths_eventual / reference_deaths, NA),
    averted = reference - cumulative_infections
  ) |>
  group_by(disease, policy = bot) |>
  summarise(
    prevented = median(prevented, na.rm = TRUE),
    deaths_prevented = median(deaths_prevented, na.rm = TRUE),
    averted = median(averted),
    left_alone = median(reference),
    win_rate = mean(win),
    .groups = "drop"
  ) |>
  group_by(disease) |>
  slice_max(prevented, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(disease = factor(disease_labels[disease], levels = rev(disease_labels[diseases])))

cat("
What is avertable (best way of playing for each disease):
")
print(as.data.frame(avertable |> mutate(across(c(prevented, deaths_prevented, win_rate), ~ round(100 * .x)))), row.names = FALSE)

avert_long <- dplyr::bind_rows(
  avertable |> transmute(disease, policy, averted, left_alone, what = "Infections", share = prevented),
  avertable |> transmute(disease, policy, averted, left_alone, what = "Deaths", share = deaths_prevented)
)
p_avert <- ggplot(avert_long, aes(x = share, y = disease, fill = what)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(
    data = avertable,
    aes(x = 1.02, y = disease, label = sprintf("%s  (averts %s of %s infections)", policy,
                                                format(signif(averted, 2), big.mark = ",", scientific = FALSE),
                                                format(signif(left_alone, 2), big.mark = ",", scientific = FALSE))),
    inherit.aes = FALSE, hjust = 0, size = 3
  ) +
  scale_x_continuous(labels = scales::percent, limits = c(min(0, min(avert_long$share, na.rm = TRUE)), 2.2),
                     breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(values = c(Infections = "#2b8cbe", Deaths = "#e15759")) +
  labs(
    title = "What is avertable: the most any way of playing prevents, for each disease",
    subtitle = sprintf("Median share of infections and deaths prevented compared with doing nothing on the same outbreak (%d outbreaks), by the best way of playing", n_seeds),
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())
ggsave(file.path("sim/checks", paste0("avertable_", label, ".png")), p_avert, width = 13, height = 6, dpi = 110)
cat("Wrote", file.path("sim/checks", paste0("avertable_", label, ".png")), "
")

heat("prevented",
     function(d) sprintf("%d%%", round(100 * d$prevented)),
     sprintf("Median share of infections prevented compared with doing nothing on the same outbreak (%d outbreaks each)", n_seeds),
     c(-1, 1), "#d7191c", "#f7f7f7", "#2b8cbe",
     file.path("sim/checks", paste0("levers_", label, ".png")))
