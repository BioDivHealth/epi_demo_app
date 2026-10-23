# Benchmarks: the most that can be prevented for each disease ------------------
#
# From a long lever-test run (sim/lever_test.R), work out for each disease
# the share of all infections and all deaths each way of playing prevents,
# pooled over every outbreak (so the big outbreaks count most, and the many
# that die out on their own count little). The best way of playing for
# deaths is the benchmark the review compares players with.
#
#   Rscript --vanilla sim/make_benchmarks.R <lever_run_label>
#
# Writes benchmarks/avertable.csv (used by the app) and
# sim/checks/benchmarks_<label>.png

suppressMessages({
  library(dplyr)
  library(ggplot2)
})

source("R/scenarios.R")
source("R/benchmarks.R")

args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args) >= 1) args[1] else "8_benchmark"

results <- read.csv(file.path("sim/results", paste0("levers_", label, ".csv")))
catalog <- scenario_catalog()
labels <- setNames(catalog$label, catalog$id)

# doing nothing on the same outbreak
nothing <- results |>
  filter(policy == "Do nothing") |>
  select(disease, seed, ref_infections = infections, ref_deaths = deaths)

pooled <- results |>
  filter(policy != "Do nothing") |>
  left_join(nothing, by = c("disease", "seed")) |>
  group_by(disease, policy) |>
  summarise(
    infections_prevented = 1 - sum(infections) / sum(ref_infections),
    deaths_prevented = 1 - sum(pmax(deaths, 0)) / max(1, sum(pmax(ref_deaths, 0))),
    deaths_left_alone = mean(pmax(ref_deaths, 0)),
    infections_left_alone = mean(ref_infections),
    .groups = "drop"
  )

# the best way of playing for each disease: most deaths prevented (most
# infections prevented where hardly anyone dies)
best <- pooled |>
  group_by(disease) |>
  mutate(score = ifelse(deaths_left_alone >= 1, deaths_prevented, infections_prevented)) |>
  slice_max(score, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-score)

# the advice for that type of disease, to check it matches what works
best$type <- vapply(best$disease, function(d) disease_advice(catalog[catalog$id == d, ])$type, character(1))

dir.create("benchmarks", showWarnings = FALSE)
out <- best |>
  transmute(
    disease, best_policy = policy,
    deaths_prevented = round(deaths_prevented, 2),
    infections_prevented = round(infections_prevented, 2),
    deaths_left_alone = round(deaths_left_alone),
    infections_left_alone = round(infections_left_alone),
    type
  )
write.csv(out, "benchmarks/avertable.csv", row.names = FALSE)
print(as.data.frame(out), row.names = FALSE)

# figure: the best share prevented per disease, and the second-best approach
second <- pooled |>
  group_by(disease) |>
  arrange(desc(deaths_prevented)) |>
  slice(2) |>
  ungroup() |>
  select(disease, second = policy, second_deaths = deaths_prevented)

plot_tbl <- out |>
  left_join(second, by = "disease") |>
  mutate(disease = factor(labels[disease], levels = rev(labels[out$disease[order(out$deaths_prevented)]])))

p <- ggplot(plot_tbl, aes(y = disease)) +
  geom_col(aes(x = deaths_prevented), fill = "#e15759", width = 0.4, position = position_nudge(y = -0.2)) +
  geom_col(aes(x = infections_prevented), fill = "#2b8cbe", width = 0.4, position = position_nudge(y = 0.2)) +
  geom_text(aes(x = 1.02, label = sprintf("%s  (next best: %s)", best_policy, second)), hjust = 0, size = 3) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 2.1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "Benchmarks: the most the best approach prevents, pooled over every outbreak",
    subtitle = sprintf("Red: deaths prevented; blue: infections prevented, compared with doing nothing on the same outbreaks (run %s)", label),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())
ggsave(file.path("sim/checks", paste0("benchmarks_", label, ".png")), p, width = 13, height = 6, dpi = 110)
cat("Wrote benchmarks/avertable.csv and", file.path("sim/checks", paste0("benchmarks_", label, ".png")), "\n")
