library(testthat)

test_that("adjacency kernels have expected dimensions", {
  inputs <- load_inputs()

  expect_equal(nrow(inputs$boros), 33)
  expect_equal(length(inputs$nb), 33)

  expect_true(is.matrix(inputs$A))
  expect_true(is.matrix(inputs$K))

  expect_equal(nrow(inputs$A), 33)
  expect_equal(ncol(inputs$A), 33)
  expect_equal(dim(inputs$A), dim(inputs$K))

  expect_equal(rowSums(inputs$K), rep(1, 33), tolerance = 1e-6)
})
