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

  if (!is.null(inputs$K_measles)) {
    expect_true(is.matrix(inputs$K_measles))
    expect_equal(dim(inputs$K_measles), dim(inputs$K))
    expect_equal(unname(rowSums(inputs$K_measles)), rep(1, 33), tolerance = 1e-6)
  }
})

test_that("populations, names, map and adjacency are all in the same borough order", {
  data_dir <- testthat::test_path("..", "..", "data")
  inputs <- load_inputs(data_dir = data_dir)

  # names and codes line up row by row with the map
  expect_equal(inputs$pop_tbl$borough_name, inputs$boros$borough_name)
  expect_equal(inputs$pop_tbl$gss_code, inputs$boros$gss_code)

  # adjacency rows follow the map: neighbours in A really touch on the map
  touches <- sf::st_touches(sf::st_make_valid(inputs$boros), sparse = FALSE)
  expect_true(all(inputs$A == touches))

  # a spot check by name: Camden's neighbours include Westminster
  camden <- which(inputs$pop_tbl$borough_name == "Camden")
  westminster <- which(inputs$pop_tbl$borough_name == "Westminster")
  expect_equal(unname(inputs$A[camden, westminster]), 1)
})
