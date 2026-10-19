# Score bot games and draw the heatmap -----------------------------------------
#
# The win rule, in words a student can follow:
#
#   You win if you prevented at least 70% of the infections AND at least 70%
#   of the deaths that doing nothing would have caused on the same outbreak.
#   (Deaths include those still to come from people already infected, and
#   the extra deaths when hospitals run out of beds. If doing nothing would
#   have caused almost no deaths, only infections count.)
#   OR, if doing nothing would have left the outbreak small (under 100
#   infections and under 20 deaths) because it died out on its own, you win
#   by not wasting money: spending at most 20% of the budget.
#
# Used by run_heatmap.R, or on its own to re-score saved results without
# playing the games again:
#   Rscript --vanilla sim/score_heatmap.R <results_label> [<plot_label>]

WIN_FRACTION <- 0.3     # at most this share of do-nothing infections
FIZZLE_LINE <- 100      # do-nothing outbreaks smaller than this "die out"...
FIZZLE_DEATHS <- 20     # ...but only if doing nothing also kills fewer than this
THRIFTY_SHARE <- 0.2    # ...and then you must spend at most this share of the budget
FEW_DEATHS <- 20        # do-nothing deaths below this: too few to judge (one or two deaths is noise)

score_games <- function(results, budget_total) {
  do_nothing <- results |>
    dplyr::filter(bot == "Do nothing") |>
    dplyr::select(disease, seed, reference = cumulative_infections, reference_deaths = deaths_eventual)

  results |>
    dplyr::filter(bot != "Do nothing") |>
    dplyr::left_join(do_nothing, by = c("disease", "seed")) |>
    dplyr::mutate(
      # infections and deaths are counted from the first report, so either
      # can be 0 (or, for deaths, a touch below 0: expected deaths at the
      # start vs actual deaths later). Add 1 to infections; floor deaths at 0.
      deaths_eventual = pmax(deaths_eventual, 0),
      reference_deaths = pmax(reference_deaths, 0),
      relative = (cumulative_infections + 1) / (reference + 1),
      relative_deaths = ifelse(reference_deaths > 0, deaths_eventual / reference_deaths, NA),
      deaths_ok = reference_deaths < FEW_DEATHS | deaths_eventual <= WIN_FRACTION * reference_deaths,
      # "died out anyway" needs few infections AND few deaths: a small
      # outbreak of a deadly disease is not fine just because it is small
      fizzled = reference < FIZZLE_LINE & reference_deaths < FIZZLE_DEATHS,
      win_by_control = !fizzled & relative <= WIN_FRACTION & deaths_ok,
      win_by_restraint = fizzled & spend <= THRIFTY_SHARE * budget_total,
      win = win_by_control | win_by_restraint
    )
}

plot_heatmap <- function(scored, label, disease_labels, bot_names, n_turns) {
  n_seeds <- length(unique(scored$seed))

  summary_tbl <- scored |>
    dplyr::group_by(disease, bot) |>
    dplyr::summarise(
      win_rate = mean(win),
      fizzled_share = mean(fizzled),
      median_relative = median(relative),
      median_relative_deaths = median(relative_deaths, na.rm = TRUE),
      median_spend = median(spend),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      disease_label = disease_labels[disease],
      bot = factor(bot, levels = bot_names),
      cell_text = sprintf(
        "%d%%\ninf x%.2f | deaths x%s\n%sk credits",
        round(100 * win_rate), median_relative,
        ifelse(is.na(median_relative_deaths), "-", sprintf("%.2f", median_relative_deaths)),
        format(round(median_spend / 1000), big.mark = ",")
      )
    )

  print(as.data.frame(summary_tbl |> dplyr::select(disease, bot, win_rate, fizzled_share, median_relative, median_relative_deaths, median_spend)))

  p <- ggplot2::ggplot(summary_tbl, ggplot2::aes(x = bot, y = disease_label, fill = win_rate)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = cell_text), size = 3) +
    ggplot2::scale_fill_gradient(low = "#f7f7f7", high = "#2b8cbe", limits = c(0, 1), labels = scales::percent) +
    ggplot2::labs(
      title = paste("Strategy win rates:", label),
      subtitle = sprintf(
        paste0(
          "Win = prevent at least %d%% of do-nothing infections and deaths (same outbreak), or, if it would have died out\n",
          "anyway (under %d infections and %d deaths), spend at most %d%% of the budget. Cell: win rate; median infections and\n",
          "deaths vs doing nothing; median spend. %d outbreaks per cell, %d turns. Hospital beds: %d."
        ),
        round(100 * (1 - WIN_FRACTION)), FIZZLE_LINE, FIZZLE_DEATHS, round(100 * THRIFTY_SHARE), n_seeds, n_turns,
        teaching_params()$hospital$beds
      ),
      x = NULL, y = NULL, fill = "Win rate"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )

  ggplot2::ggsave(file.path("sim", paste0("heatmap_", label, ".png")), p, width = 12, height = 7, dpi = 110)
  invisible(summary_tbl)
}

# Re-score saved results when run on its own -----------------------------------

if (sys.nframe() == 0) {
  suppressMessages({ library(dplyr); library(tibble) })
  source("R/teaching_params.R")
  source("R/scenarios.R")

  args <- commandArgs(trailingOnly = TRUE)
  results_label <- args[1]
  plot_label <- if (length(args) >= 2) args[2] else results_label

  results <- read.csv(file.path("sim/results", paste0(results_label, ".csv")))
  tp <- teaching_params()
  disease_labels <- setNames(scenario_catalog()$label, scenario_catalog()$id)
  bot_names <- c("Chase the signal", "Broad surveillance", "Test, trace, isolate",
                 "Build testing first", "Hit hard early", "All-in early", "Watch and wait")

  scored <- score_games(results, tp$budget_total)
  plot_heatmap(scored, plot_label, disease_labels, bot_names, tp$game_turns)
}
