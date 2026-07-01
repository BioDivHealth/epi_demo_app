library(testthat)

test_that("host admin store starts from default scenario values", {
  store <- create_host_admin_store()
  snapshot <- host_admin_snapshot(store$state, store$fields)
  defaults <- host_admin_defaults()

  expect_named(snapshot, names(defaults), ignore.order = FALSE)
  expect_equal(snapshot, defaults)
  expect_identical(shiny::isolate(store$reset_counter()), 0L)
})

test_that("host admin store supports injected defaults and explicit reset signaling", {
  defaults <- host_admin_defaults()
  defaults$Rt <- 1.9
  defaults$scenario_id <- "custom"

  store <- create_host_admin_store(defaults)

  expect_equal(shiny::isolate(store$state$Rt), 1.9)
  expect_identical(shiny::isolate(store$state$scenario_id), "custom")

  trigger_host_reset(store$reset_counter)
  expect_identical(shiny::isolate(store$reset_counter()), 1L)

  store$state$show_scenario <- TRUE
  snapshot <- host_admin_snapshot(store$state, store$fields)
  expect_true(isTRUE(snapshot$show_scenario))
})
