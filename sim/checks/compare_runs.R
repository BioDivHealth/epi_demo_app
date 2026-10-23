# Compare two heatmap runs -------------------------------------------------------
#
# Win rate of every strategy on every disease, before and after a change, as
# one line per strategy. Scored with the current win rule (score_heatmap.R).
#
# Run from the project root:
#   Rscript --vanilla sim/checks/compare_runs.R <before_label> <after_label> <title>
# e.g.
#   Rscript --vanilla sim/checks/compare_runs.R 17_weekly_reports 18_distancing_tracing "Stronger distancing, tracing follows hospital cases"
#
# Writes sim/checks/compare_<before_label>_vs_<after_label>.png

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

source("R/teaching_params.R")
source("R/scenarios.R")
source("sim/score_heatmap.R")

args <- commandArgs(trailingOnly = TRUE)
before_label <- args[1]
after_label <- args[2]
title <- if (length(args) >= 3) args[3] else paste(before_label, "vs", after_label)

budget_total <- teaching_params()$budget_total
disease_labels <- setNames(scenario_catalog()$label, scenario_catalog()$id)

# read and score one heatmap run
score_run_file <- function(label) {
  results <- read.csv(file.path("sim/results", paste0(label, ".csv")))
  results$bot[results$bot == "Watch and wait"] <- "Respond to hospital reports"
  score_games(results, budget_total) |>
    mutate(run = label)
}

scored <- bind_rows(score_run_file(before_label), score_run_file(after_label))

# only diseases played in both runs (one may have been added or removed)
in_both <- intersect(scored$disease[scored$run == before_label], scored$disease[scored$run == after_label])
scored <- scored[scored$disease %in% in_both, ]

win_rates <- scored |>
  group_by(run, disease, bot) |>
  summarise(win_rate = mean(win), .groups = "drop") |>
  mutate(
    disease = disease_labels[disease],
    run = factor(run, levels = c(before_label, after_label), labels = c("Before", "After"))
  )

# the numbers behind the plot
print(
  win_rates |>
    tidyr::pivot_wider(names_from = run, values_from = win_rate) |>
    mutate(change = After - Before) |>
    arrange(disease, desc(After)) |>
    as.data.frame()
)

win_plot <- ggplot(win_rates, aes(x = run, y = win_rate, group = bot, colour = bot)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~disease, nrow = 1) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = title,
    subtitle = "Win rate per strategy (100 outbreaks each; differences under about 10 points are noise)",
    x = NULL, y = "Win rate", colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

out <- file.path("sim/checks", paste0("compare_", before_label, "_vs_", after_label, ".png"))
ggsave(out, win_plot, width = 14, height = 5.5, dpi = 110)
cat("Wrote", out, "\n")
