# Before and after the fixed weekly report cadence ------------------------------
#
# Before: one report per generation of infection (3-15 days, by disease).
# After:  one report every 7 days for every disease (heatmap run 17), 20
#         reports (140 days) for every disease.
# Also:   weekly reports, but each disease keeps its old game length in days
#         (run 17b), to separate the cadence from the game length.
#
# Draws two panels:
#   1. win rate per disease and strategy, before vs after
#   2. how big the outbreak gets if nobody does anything, before vs after
#
# Run from the project root:
#   Rscript --vanilla sim/checks/compare_weekly_cadence.R

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

source("R/teaching_params.R")
source("R/scenarios.R")
source("sim/score_heatmap.R")

budget_total <- teaching_params()$budget_total
disease_labels <- setNames(scenario_catalog()$label, scenario_catalog()$id)

# read and score one heatmap run
load_run <- function(label, when) {
  results <- read.csv(file.path("sim/results", paste0(label, ".csv")))
  results$bot[results$bot == "Watch and wait"] <- "Respond to hospital reports"
  results$when <- when
  results
}

before <- load_run("15_realistic_start", "Before: report every generation")
after <- load_run("17_weekly_reports", "After: report every week")
same_days <- load_run("17b_weekly_same_days", "Weekly, old game length")
run_levels <- c(before$when[1], same_days$when[1], after$when[1])

scored <- bind_rows(
  score_games(before, budget_total) |> mutate(when = before$when[1]),
  score_games(same_days, budget_total) |> mutate(when = same_days$when[1]),
  score_games(after, budget_total) |> mutate(when = after$when[1])
)

# 1. win rates ------------------------------------------------------------------

win_rates <- scored |>
  group_by(when, disease, bot) |>
  summarise(win_rate = mean(win), .groups = "drop") |>
  mutate(
    disease = disease_labels[disease],
    when = factor(when, levels = run_levels)
  )

win_plot <- ggplot(win_rates, aes(x = when, y = win_rate, group = bot, colour = bot)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~disease, nrow = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_discrete(labels = c("Every\ngeneration", "Weekly,\nsame days", "Weekly,\n20 weeks")) +
  labs(
    title = "Win rate of each strategy, before and after weekly reports",
    x = NULL, y = "Win rate (100 outbreaks)", colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("sim/checks/weekly_cadence_win_rates.png", win_plot, width = 14, height = 5.5, dpi = 110)

# 2. do-nothing outbreak size -----------------------------------------------------

do_nothing <- bind_rows(before, same_days, after) |>
  filter(bot == "Do nothing") |>
  mutate(
    disease = disease_labels[disease],
    when = factor(when, levels = run_levels)
  )

size_plot <- ggplot(do_nothing, aes(x = disease, y = cumulative_infections + 1, fill = when)) +
  geom_boxplot(outlier.size = 0.6) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Infections during the response if nobody does anything",
    subtitle = "Dashed line: 100 infections (below it, and under 20 deaths, the outbreak counts as dying out anyway)",
    x = NULL, y = "Infections + 1 (log scale)", fill = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1), legend.position = "bottom")

ggsave("sim/checks/weekly_cadence_outbreak_size.png", size_plot, width = 11, height = 5.5, dpi = 110)

# numbers behind the plots
print(
  do_nothing |>
    group_by(disease, when) |>
    summarise(median_infections = median(cumulative_infections), .groups = "drop") |>
    as.data.frame()
)
