library(testthat)

# The game on places other than London -----------------------------------------
# A mid-sized country, a city and a big country: the kernel follows the
# engine's convention and a few turns of every bot run cleanly.

root <- testthat::test_path("..", "..")
library_dir <- file.path(root, "locations", "library")
skip_if_not(dir.exists(library_dir), "place library not present")

index <- read_place_index(library_dir)

places <- c(
  kenya = find_place("Kenya", index)$id[1],
  lagos = find_place("Lagos", index)$id[1],
  uk = find_place("United Kingdom", index)$id[1]
)

for (name in names(places)) {
  test_that(paste("the game runs on", name), {
    place <- load_place(places[[name]], index, library_dir = library_dir)
    inputs <- place$inputs
    n <- nrow(inputs$pop_tbl)

    # kernel: square, rows sum to 1 (each case causes Rt cases), aligned names
    expect_equal(dim(inputs$K), c(n, n))
    expect_equal(unname(rowSums(inputs$K)), rep(1, n), tolerance = 1e-6)
    expect_equal(inputs$pop_tbl$borough_name, inputs$boros$borough_name)

    # a few turns with every bot, on a respiratory and a measles-like disease
    for (disease in c("covid_early", "measles_current")) {
      config <- scenario_config(disease, inputs)
      expect_equal(empty_allocations(config)$borough_name, inputs$pop_tbl$borough_name)
      for (bot in strategy_bots()) {
        game <- play_bot_game(config, bot, seed = 2, n_turns = 5)
        expect_true(all(game$state$truth$incidence >= 0))
        expect_lte(game$state$spend_total, config$tp$budget_total + 1e-6)
      }
    }
  })
}

test_that("money and capacity scale with population, London unchanged", {
  tp <- teaching_params()
  expect_identical(scale_to_place(tp, tp$reference_pop), tp)

  doubled <- scale_to_place(tp, 2 * tp$reference_pop)
  expect_equal(doubled$budget_total, 2 * tp$budget_total)
  expect_equal(doubled$hospital$beds, 2 * tp$hospital$beds)
  expect_equal(doubled$tests$costs, tp$tests$costs)
})
