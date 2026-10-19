# How full do hospitals get? --------------------------------------------------
#
# Peak bed use (share of the hospital pool) for each disease when nobody acts,
# and for each strategy. Used to choose a teaching-level bed number: severe
# diseases should fill hospitals if left alone, mild ones should not.
#
#   Rscript --vanilla sim/checks/check_beds.R

suppressMessages({ library(dplyr); library(tibble); library(ggplot2) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")
n_turns <- teaching_params()$game_turns

diseases <- setdiff(scenario_catalog()$id, "custom")
bots <- c("Do nothing" = bot_do_nothing, strategy_bots())

rows <- list()
for (d in diseases) {
  config <- scenario_config(d, inputs)
  for (b in names(bots)) {
    for (seed in 1:15) {
      g <- play_bot_game(config, bots[[b]], seed = seed, n_turns = n_turns)
      rows[[length(rows) + 1]] <- tibble(
        disease = d, bot = b, seed = seed,
        peak_bed_use = g$peak_bed_use,
        extra_deaths = g$extra_deaths_no_bed,
        deaths_eventual = g$deaths_eventual
      )
    }
  }
}
res <- bind_rows(rows)

res |>
  group_by(disease, bot) |>
  summarise(
    median_peak_bed_use = round(median(peak_bed_use), 2),
    share_games_over_capacity = round(mean(peak_bed_use > 1), 2),
    median_extra_deaths = median(extra_deaths),
    median_deaths = round(median(deaths_eventual)),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

p <- ggplot(res, aes(x = peak_bed_use, y = bot)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_jitter(height = 0.15, alpha = 0.5, size = 1) +
  facet_wrap(~disease, ncol = 2, scales = "free_x") +
  labs(
    title = "Peak hospital bed use by disease and strategy",
    subtitle = sprintf("1 = all %s beds full (dashed line). 15 outbreaks each, %d turns.", teaching_params()$hospital$beds, n_turns),
    x = "Peak share of beds in use", y = NULL
  ) +
  theme_minimal(base_size = 10)

ggsave("sim/checks/bed_use.png", p, width = 10, height = 8, dpi = 110)
