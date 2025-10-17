library(testthat)

skip_if_not_installed("shinytest2")

# shinytest2 needs to run in interactive context; keep smoke test lightweight

test_that("app launches and key UI elements exist", {
  app <- shinytest2::AppDriver$new(
    app_dir = "..",
    name = "smoke",
    seed = 123,
    load_timeout = 10000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.getElementById('map-map') !== null")
  app$wait_for_js("document.getElementById('controls-advance') !== null")

  advance_count <- app$get_value(input = "controls-advance")
  expect_identical(advance_count, 0)
})
