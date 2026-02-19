library(testthat)

skip_if_not_installed("shinytest2")

# shinytest2 needs to run in interactive context; keep smoke test lightweight

test_that("app launches and key UI elements exist", {
  old_auth <- Sys.getenv("EPILONDON_AUTH_DISABLE", unset = NA)
  Sys.setenv(EPILONDON_AUTH_DISABLE = "true")
  on.exit({
    if (is.na(old_auth)) {
      Sys.unsetenv("EPILONDON_AUTH_DISABLE")
    } else {
      Sys.setenv(EPILONDON_AUTH_DISABLE = old_auth)
    }
  }, add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = "../..",
    name = "smoke",
    seed = 123,
    load_timeout = 10000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.getElementById('map-map') !== null")
  app$wait_for_js("document.getElementById('map-advance') !== null")
  app$wait_for_js("document.getElementById('controls-run_mode') !== null")
  app$wait_for_js("document.getElementById('controls-auto_turns') !== null")
  app$wait_for_js("document.getElementById('controls-allocation_policy') !== null")
  app$wait_for_js("document.getElementById('controls-start_run') !== null")

  advance_count <- app$get_value(input = "map-advance")
  expect_equal(as.integer(advance_count), 0L)
})
