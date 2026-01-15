library(testthat)

test_that("adjacency kernels have expected dimensions", {
  # Use testthat helper to find project root
  data_dir <- testthat::test_path("..", "..", "data")
  inputs <- load_inputs(data_dir = data_dir)

  expect_equal(nrow(inputs$boros), 33)
  expect_equal(length(inputs$nb), 33)

  expect_true(is.matrix(inputs$A))
  expect_true(is.matrix(inputs$K))

  expect_equal(nrow(inputs$A), 33)
  expect_equal(ncol(inputs$A), 33)
  expect_equal(dim(inputs$A), dim(inputs$K))

  expect_equal(unname(rowSums(inputs$K)), rep(1, 33), tolerance = 1e-6)
})
