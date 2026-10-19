# Is the Rt shown to students right? ------------------------------------------
#
# The dashboard and reveal estimate Rt with EpiEstim using a serial interval
# of mean 3.5, sd 1.5. In this game one turn is one generation (each case's
# infections appear the next turn), so the serial interval is 1 turn.
# Feed in a series that grows by exactly 1.3x per turn and compare.
#
#   Rscript --vanilla sim/checks/check_rt_estimate.R

.libPaths(c("C:/Users/a-davir/AppData/Local/Temp/rl", .libPaths()))
suppressMessages(library(EpiEstim))
source("R/scoring.R")

cases <- round(50 * 1.3^(0:12))
cat("True R per turn: 1.3\n")

as_used <- estimate_rt_series(cases, mean_si = 3.5, std_si = 1.5)
cat("Estimate as the app does it (serial interval 3.5 turns):", round(as_used$mean, 2), "\n")

one_turn <- estimate_rt_series(cases, mean_si = 1.0, std_si = 0.5)
cat("Estimate with a 1-turn serial interval:", round(one_turn$mean, 2), "\n")

cat("Simple ratio of this turn's cases to last turn's:", round(tail(cases, 1) / tail(cases, 2)[1], 2), "\n")
