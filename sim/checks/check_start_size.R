# How big is the outbreak when the first patient is reported? ----------------
#
# The outbreak starts from one infected person and runs unseen until 3
# hospital admissions or the first death. Count everyone
# infected by the first report, per disease, and what triggered the report.
#
#   Rscript --vanilla sim/checks/check_start_size.R

suppressMessages({ library(dplyr); library(tibble); library(ggplot2) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")

diseases <- setdiff(scenario_catalog()$id, "custom")
labels <- setNames(scenario_catalog()$label, scenario_catalog()$id)

rows <- list()
for (d in diseases) {
  config <- scenario_config(d, inputs)
  for (seed in 1:200) {
    state <- new_game(config, seed = seed)
    rows[[length(rows) + 1]] <- tibble(
      disease = labels[[d]],
      hidden_days = state$truth_offset * report_days(config),
      infected_by_report = state$start_infections,
      current_cases = sum(state$truth$incidence),
      deaths_by_report = sum(state$truth$cumulative_deaths),
      triggered_by_death = identical(state$alert$trigger, "death")
    )
  }
}
res <- bind_rows(rows)

res |>
  group_by(disease) |>
  summarise(
    median_infected = median(infected_by_report),
    p90_infected = quantile(infected_by_report, 0.9),
    max_infected = max(infected_by_report),
    median_hidden_days = median(hidden_days),
    share_triggered_by_death = mean(triggered_by_death),
    max_deaths_by_report = max(deaths_by_report),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

p <- ggplot(res, aes(x = infected_by_report, y = disease)) +
  geom_jitter(height = 0.15, alpha = 0.4, size = 1) +
  scale_x_log10(labels = scales::label_comma()) +
  labs(
    title = "People infected by the time the first patient is reported",
    subtitle = "One infected person at the true start; unseen until 3 hospital admissions or the first death. 200 outbreaks per disease.",
    x = "Infected by the first report (log scale)", y = NULL
  ) +
  theme_minimal(base_size = 11)
ggsave("sim/checks/start_size.png", p, width = 9, height = 4.5, dpi = 110)
