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
  app$wait_for_js("document.getElementById('controls-testing_pool_info') !== null")
  app$wait_for_js("document.getElementById('controls-allocation_policy_info') !== null")
  app$wait_for_js("document.getElementById('map-interventions_info') !== null")
  app$wait_for_js(
    "(() => {
      const nodes = document.querySelectorAll('#map-interventions .map-choice-with-tooltip');
      if (!nodes || nodes.length === 0) return false;
      return Array.from(nodes).every((node) => {
        return (node.textContent || '').trim().length > 0;
      });
    })();",
    timeout = 10000
  )
  app$wait_for_js("document.getElementById('controls-rng_seed') !== null")
  app$wait_for_js("document.getElementById('controls-reseed') !== null")
  app$wait_for_js("document.getElementById('controls-end') !== null")
  app$wait_for_js("document.getElementById('controls-reset') !== null")
  app$wait_for_js("document.querySelector('.bslib-sidebar-layout') === null")
  app$wait_for_js("document.querySelector('.main-header-title .scenario-label') === null")
  app$wait_for_js("document.getElementById('controls-auto_turns') !== null")
  app$wait_for_js("document.getElementById('controls-allocation_policy') !== null")
  app$wait_for_js("document.getElementById('controls-start_run') !== null")
  app$wait_for_js("document.getElementById('dashboard-signal_quality_panel') !== null")
  app$wait_for_js("document.getElementById('dashboard-budget_mix_panel') !== null")
  app$wait_for_js(
    "document.getElementById('dashboard-signal_quality_panel').textContent.includes('Recent signal')",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('dashboard-signal_quality_panel').textContent.includes('Starter alert only')",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('dashboard-signal_quality_panel').textContent.includes('This batch')",
    timeout = 10000
  )
  app$wait_for_js(
    "!document.getElementById('dashboard-signal_quality_panel').textContent.includes('Continuity')",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('dashboard-budget_mix_panel').textContent.includes('No turn plan yet')",
    timeout = 10000
  )

  app$set_inputs(`controls-run_mode` = "manual")
  app$wait_for_js("document.getElementById('controls-auto_turns').offsetParent === null")
  app$wait_for_js("document.getElementById('controls-allocation_policy').offsetParent === null")
  app$wait_for_js("document.getElementById('controls-start_run').offsetParent === null")
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent !== null", timeout = 8000)
  app$wait_for_js(
    "!document.getElementById('dashboard-signal_quality_panel').textContent.includes('This batch')",
    timeout = 10000
  )
  app$set_inputs(`map-interventions` = "distancing")
  app$wait_for_js(
    "!document.getElementById('dashboard-budget_mix_panel').textContent.includes('No turn plan yet')",
    timeout = 10000
  )

  app$set_inputs(`controls-run_mode` = "guided_auto")
  app$wait_for_js("document.getElementById('controls-auto_turns').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('controls-allocation_policy').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('controls-start_run').offsetParent !== null", timeout = 8000)
  app$wait_for_js(
    "document.getElementById('dashboard-signal_quality_panel').textContent.includes('This batch')",
    timeout = 10000
  )

  app$set_inputs(`controls-allocation_policy` = "all_boroughs", wait_ = FALSE)
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent === null")
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent === null")

  app$set_inputs(`controls-allocation_policy` = "manual", wait_ = FALSE)
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent === null")
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent === null")

  app$set_inputs(`controls-run_mode` = "manual")
  app$wait_for_js("document.querySelector('.map-selection-group-test-type').offsetParent !== null", timeout = 8000)
  app$wait_for_js("document.getElementById('map-tests_per_click').offsetParent !== null", timeout = 8000)

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

  app$set_inputs(`map-interventions` = "distancing")
  app$set_inputs(`map-advance` = "click")
  app$wait_for_js(
    "(() => {
      const node = document.getElementById('turn_display');
      if (!node) return false;
      const text = (node.textContent || '').trim();
      return /^Turn\\s+1$/.test(text);
    })();",
    timeout = 10000
  )

  app$set_inputs(`controls-end` = "click")

  app$wait_for_js(
    "document.getElementById('endgame-response_score_card') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('endgame-response_score_value') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('endgame-response_verdict') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('endgame-subscore-outbreak') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('endgame-subscore-surveillance') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('endgame-subscore-balance') !== null",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('endgame-response_score_card').textContent.includes('Response Score')",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('show_replay') !== null",
    timeout = 10000
  )

  app$set_inputs(show_replay = "click")

  app$wait_for_js("document.getElementById('replay-replay_branch') !== null")
  app$wait_for_js("document.getElementById('replay-turn_slider') !== null")
  app$wait_for_js("document.getElementById('replay-map_observed') !== null", timeout = 10000)
  app$wait_for_js("document.getElementById('replay-map_truth') !== null", timeout = 10000)

  app$set_inputs(`replay-replay_branch` = "baseline", wait_ = FALSE)
  app$wait_for_js(
    "(() => {
      const selected = document.querySelector(\"input[name='replay-replay_branch']:checked\");
      return selected && selected.value === 'baseline';
    })();",
    timeout = 10000
  )
  expect_true(TRUE)
})

test_that("admin login modal does not block ending the game", {
  old_auth <- Sys.getenv("EPILONDON_AUTH_DISABLE", unset = NA)
  Sys.unsetenv("EPILONDON_AUTH_DISABLE")
  on.exit({
    if (is.na(old_auth)) {
      Sys.unsetenv("EPILONDON_AUTH_DISABLE")
    } else {
      Sys.setenv(EPILONDON_AUTH_DISABLE = old_auth)
    }
  }, add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = "../..",
    name = "admin-login-endgame",
    seed = 123,
    load_timeout = 15000
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js("document.getElementById('admin_login') !== null", timeout = 10000)
  app$wait_for_js("document.getElementById('controls-end') !== null", timeout = 10000)

  app$set_inputs(admin_login = "click")
  app$wait_for_js("document.getElementById('admin_login_submit') !== null", timeout = 10000)

  app$set_inputs(`controls-end` = "click")
  app$wait_for_js(
    "document.getElementById('endgame-response_score_card') !== null",
    timeout = 10000
  )
  expect_true(TRUE)
})
