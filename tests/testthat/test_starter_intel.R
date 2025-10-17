library(testthat)

pop_stub <- tibble::tibble(
  borough_name = c("A", "B", "C"),
  pop = c(1000, 1000, 1000)
)

truth_stub <- init_truth(n_boroughs = 3, seed_boroughs = 2, seed_cases = 15)

set.seed(1)

test_that("alert intel targets seeded borough where possible", {
  alloc <- starter_intel_allocations("alert", truth_stub, pop_stub)
  expect_equal(sum(alloc$n_pcr), 40)
  expect_equal(which(alloc$n_pcr > 0), 2)
})

test_that("sweep intel mixes antigen and PCR tests", {
  alloc <- starter_intel_allocations("sweep", truth_stub, pop_stub)
  expect_gt(sum(alloc$n_ag), 0)
  expect_gt(sum(alloc$n_pcr), 0)
})

test_that("label helper returns descriptive text", {
  expect_equal(starter_intel_label("alert"), "Starter intel: targeted alert")
  expect_equal(starter_intel_label("none"), "")
})
