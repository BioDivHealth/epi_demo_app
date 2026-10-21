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
  n <- spread_tests(money / per_test, view$pop_tbl$pop)
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
  "Nothing" = function(view, memory) list(actions = list(), memory = memory),

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

  "Build rapid tests, then test" = function(view, memory) {
    build <- isTRUE(view$testing$can_build) && !isTRUE(memory$built)
    if (build) memory$built <- TRUE
    cost <- if (build) view$testing$build_cost else 0
    list(actions = list(allocations = fair_tests(view, cost), sampling_mode = "symptomatic",
                        build_testing = build), memory = memory)
  },

  "Restrict hospital clusters" = function(view, memory) {
    areas <- affordable_restrictions(view, admission_areas(view), keep_for_tests = 0)
    list(actions = list(local_restrictions = areas), memory = memory)
  },

  "Lockdown from the start" = function(view, memory) {
    list(actions = list(interventions = "distancing"), memory = memory)
  },

  "Lockdown when beds half full" = function(view, memory) {
    on <- isTRUE(memory$locked) || view$bed_pressure >= 0.5
    memory$locked <- on
    list(actions = list(interventions = if (on) "distancing" else character()), memory = memory)
  }
)

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

# Share prevented compared with doing nothing on the same outbreak ---------------

nothing <- results |>
  filter(policy == "Nothing") |>
  select(disease, seed, ref_infections = infections, ref_deaths = deaths)

summary_tbl <- results |>
  filter(policy != "Nothing") |>
  left_join(nothing, by = c("disease", "seed")) |>
  mutate(
    prevented = ifelse(ref_infections > 0, 1 - infections / ref_infections, NA),
    deaths_prevented = ifelse(ref_deaths >= 1, 1 - pmax(deaths, 0) / ref_deaths, NA)
  ) |>
  group_by(disease, policy) |>
  summarise(
    prevented = median(prevented, na.rm = TRUE),
    deaths_prevented = median(deaths_prevented, na.rm = TRUE),
    spend = median(spend),
    .groups = "drop"
  )

print(as.data.frame(summary_tbl |> mutate(across(c(prevented, deaths_prevented), ~ round(100 * .x)))))

plot_tbl <- summary_tbl |>
  mutate(
    disease = factor(disease_labels[disease], levels = disease_labels[diseases]),
    policy = factor(policy, levels = setdiff(names(policies), "Nothing")),
    text = sprintf("%d%%\n%sk", round(100 * prevented), format(round(spend / 1000), big.mark = ","))
  )

p <- ggplot(plot_tbl, aes(x = policy, y = disease, fill = prevented)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = text), size = 3) +
  scale_fill_gradient2(low = "#d7191c", mid = "#f7f7f7", high = "#2b8cbe", midpoint = 0,
                       limits = c(-1, 1), labels = scales::percent) +
  labs(
    title = paste("What each control does on its own:", label),
    subtitle = sprintf("Median share of infections prevented compared with doing nothing on the same outbreak (%d outbreaks), and median spend.", n_seeds),
    x = NULL, y = NULL, fill = "Prevented"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.grid = element_blank())

ggsave(file.path("sim/checks", paste0("levers_", label, ".png")), p, width = 13, height = 7, dpi = 110)
cat("Wrote", file.path("sim/checks", paste0("levers_", label, ".png")), "\n")
