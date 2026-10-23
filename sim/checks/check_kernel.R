# Is the spread kernel sensible? ----------------------------------------------
#
# Convention: row j of K = where borough j's onward infections land, and the
# truth engine spreads with mu = crossprod(K, I) * Rt. So each case in
# borough j causes Rt * rowSums(K)[j] new cases: rows must sum to 1 for R to
# mean the same everywhere.
#
# (Before the fix, the engine used K %*% I with a row-normalised,
# neighbours-only K: no within-borough spread and R from 0.57x to 1.43x.)
#
#   Rscript --vanilla sim/checks/check_kernel.R

suppressMessages({ library(sf); library(ggplot2); library(dplyr) })
source("R/util_io.R")
source("R/mod_truth_engine.R")
inputs <- load_inputs("data")
boros <- inputs$boros

describe <- function(K, label) {
  cat("\n==", label, "==\n")
  cat("Within-borough share (diagonal): min", round(min(diag(K)), 2), "max", round(max(diag(K)), 2), "\n")
  r_mult <- rowSums(K)
  cat("R multiplier per source borough (row sums): min", round(min(r_mult), 3), "max", round(max(r_mult), 3), "\n")
  cat("Weight on boroughs that do not touch:", round(sum(K[inputs$A == 0 & row(K) != col(K)]), 3), "\n")
}

describe(inputs$K, "default kernel")
describe(inputs$K_measles, "measles kernel")

# one generation from 100 cases in Camden, Rt 1.5, using the engine itself
camden <- which(boros$borough_name == "Camden")
state <- init_truth(nrow(inputs$K), seed_boroughs = camden, seed_cases = 100L)
stepped <- step_truth(state, inputs$K, Rt_global = 1.5, k_overdisp = Inf, imports_prob = 0, lambda_import = 0)
cat("\n100 cases in Camden, Rt 1.5 -> expected next generation:", round(sum(stepped$last_mu), 1),
    "of which in Camden itself:", round(stepped$last_mu[camden], 1), "\n")

# map: where 100 Camden cases' infections land
boros$expected_from_camden <- stepped$last_mu
p <- ggplot(boros) +
  geom_sf(aes(fill = expected_from_camden), colour = "white") +
  scale_fill_gradient(low = "#f7f7f7", high = "#b2182b") +
  labs(
    title = "Where 100 cases in Camden spread next turn (Rt 1.5)",
    subtitle = "70% stays in Camden, 30% is shared evenly among touching boroughs. Total = 150 wherever the cases start.",
    fill = "Expected\nnew cases"
  ) +
  theme_void(base_size = 11)

ggsave("sim/checks/kernel_spread_from_camden.png", p, width = 8, height = 6, dpi = 110)
