# Why does broad surveillance never win? ---------------------------------------
#
# Plays the broad-surveillance bot and records, turn by turn, what it tested,
# what it found (real vs false positives), how many cases it isolated, which
# boroughs it restricted, and the hidden truth. Compares the turn it first
# sees a real signal with the turn hospitals first report admissions.
#
# Run from the project root:
#   Rscript --vanilla sim/checks/trace_broad.R

suppressMessages({ library(dplyr); library(tibble); library(ggplot2) })
for (f in list.files("R", full.names = TRUE)) sys.source(f, envir = globalenv())
inputs <- load_inputs("data")
n_turns <- teaching_params()$game_turns

# play one game, keeping a per-turn record
trace_game <- function(disease, bot, seed) {
  config <- scenario_config(disease, inputs)
  state <- new_game(config, seed = seed)
  memory <- list()
  rows <- list()

  for (i in seq_len(n_turns)) {
    view <- player_view(state, config)
    choice <- bot(view, memory)
    memory <- choice$memory
    result <- play_turn(state, choice$actions, config)
    if (!result$ok) result <- play_turn(state, list(), config)

    avail <- result$available
    rows[[i]] <- tibble(
      disease = disease, seed = seed, turn = state$turn,
      true_new_infections = sum(state$truth$incidence),
      tests_run = sum(result$allocations$n_ag + result$allocations$n_pcr),
      results_back = sum(avail$n),
      positives = sum(avail$positives),
      true_positives = sum(avail$true_pos),
      restricted = length(result$restricted),
      test_level = state$test_level,
      admissions_reported = sum(state$reports$admissions[state$reports$turn == state$turn])
    )
    state <- result$state
  }
  bind_rows(rows)
}

# one detailed game per disease ------------------------------------------------

for (d in c("covid_early", "influenza_seasonal")) {
  cat("\n=====", d, "seed 1 =====\n")
  print(as.data.frame(trace_game(d, bot_broad, 1)))
}

# first signal vs first hospital report, over many outbreaks -------------------

first_turn <- function(x, turns) if (any(x)) min(turns[x]) else NA_integer_

timing <- bind_rows(lapply(c("covid_early", "influenza_seasonal", "ebola_like"), function(d) {
  bind_rows(lapply(1:30, function(s) {
    tr <- trace_game(d, bot_broad, s)
    tibble(
      disease = d, seed = s,
      first_real_positive = first_turn(tr$true_positives > 0, tr$turn),
      first_restriction = first_turn(tr$restricted > 0, tr$turn),
      first_admission = first_turn(tr$admissions_reported > 0, tr$turn),
      share_cases_found = sum(tr$true_positives) / sum(tr$true_new_infections)
    )
  }))
}))

print(timing |>
  group_by(disease) |>
  summarise(
    median_first_real_positive = median(first_real_positive, na.rm = TRUE),
    median_first_restriction = median(first_restriction, na.rm = TRUE),
    median_first_admission = median(first_admission, na.rm = TRUE),
    median_share_cases_found = median(share_cases_found),
    .groups = "drop"
  ) |>
  as.data.frame())

# figure: when each source first sees the outbreak ------------------------------

long <- timing |>
  tidyr::pivot_longer(c(first_real_positive, first_restriction, first_admission),
                      names_to = "signal", values_to = "turn")

p <- ggplot(long, aes(x = turn, y = signal)) +
  geom_jitter(height = 0.15, width = 0.2, alpha = 0.5) +
  facet_wrap(~disease, ncol = 1) +
  labs(
    title = "Broad surveillance: when does it first see the outbreak?",
    subtitle = "Each dot is one outbreak (30 per disease). Admissions = hospital reports anyone would get.",
    x = "Turn", y = NULL
  ) +
  theme_minimal(base_size = 11)

dir.create("sim/checks", showWarnings = FALSE)
ggsave("sim/checks/trace_broad_timing.png", p, width = 8, height = 6, dpi = 110)
