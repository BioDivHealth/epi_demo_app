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
  app$wait_for_js("document.getElementById('map-run_rail') !== null")
  app$wait_for_js("document.getElementById('map-run_mode_box') !== null")
  app$wait_for_js("document.getElementById('controls-run_mode') !== null")
  app$wait_for_js("document.getElementById('controls-rng_seed') !== null")
  app$wait_for_js("document.getElementById('controls-reseed') !== null")
  app$wait_for_js("document.getElementById('controls-end') !== null")
  app$wait_for_js("document.getElementById('controls-reset') !== null")
  app$wait_for_js("document.querySelector('.bslib-sidebar-layout') === null")
  app$wait_for_js("document.getElementById('controls-auto_turns') !== null")
  app$wait_for_js("document.getElementById('controls-allocation_policy') !== null")
  app$wait_for_js("document.getElementById('controls-start_run') !== null")

  app$set_inputs(`controls-run_mode` = "manual")
  app$wait_for_js("document.getElementById('controls-auto_turns').offsetParent === null")
  app$wait_for_js("document.getElementById('controls-auto_speed').offsetParent === null")
  app$wait_for_js("document.getElementById('controls-allocation_policy').offsetParent === null")
  app$wait_for_js("document.getElementById('controls-start_run').offsetParent === null")
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent !== null")
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent !== null")

  app$set_inputs(`controls-run_mode` = "guided_auto")
  app$wait_for_js("document.getElementById('controls-auto_turns').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('controls-auto_speed').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('controls-allocation_policy').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('controls-start_run').offsetParent !== null", timeout = 8000)

  app$set_inputs(`controls-allocation_policy` = "random_population")
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent === null")
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent === null")

  app$set_inputs(`controls-allocation_policy` = "manual")
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent !== null")
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent !== null")

  advance_count <- app$get_value(input = "map-advance")
  expect_equal(as.integer(advance_count), 0L)
})

test_that("replay unlocks after reveal and supports branch switching", {
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
    name = "replay-smoke",
    seed = 123,
    load_timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.getElementById('controls-end') !== null")
  app$wait_for_js("document.getElementById('show_replay') === null")

  app$set_inputs(`controls-end` = "click")

  app$wait_for_js(
    "document.getElementById('show_replay') !== null",
    timeout = 10000
  )

  app$set_inputs(show_replay = "click")

  app$wait_for_js("document.getElementById('replay-replay_branch') !== null")
  app$wait_for_js("document.getElementById('replay-turn_slider') !== null")

  app$set_inputs(`replay-replay_branch` = "baseline")
  selected_branch <- app$get_value(input = "replay-replay_branch")
  expect_identical(selected_branch, "baseline")

  app$wait_for_js("document.getElementById('replay-map_observed') !== null")
  app$wait_for_js("document.getElementById('replay-map_truth') !== null")
})
