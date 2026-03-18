library(testthat)

test_that("student scenario display hides hidden and custom scenarios", {
  hidden_display <- student_scenario_display(
    scenario_id = "measles_current",
    show_scenario = FALSE
  )
  expect_false(hidden_display$visible)
  expect_identical(hidden_display$header_text, "")

  custom_display <- student_scenario_display(
    scenario_id = "custom",
    show_scenario = TRUE
  )
  expect_false(custom_display$visible)
  expect_identical(custom_display$header_text, "")
})

test_that("student scenario display returns named preset text when visible", {
  scenario_display <- student_scenario_display(
    scenario_id = "measles_current",
    show_scenario = TRUE
  )

  expect_true(scenario_display$visible)
  expect_identical(
    scenario_display$header_text,
    "Scenario: Measles-like (current vaccination)"
  )
  expect_identical(
    scenario_display$label,
    "Measles-like (current vaccination)"
  )
})

test_that("help modal hides the scenario block when student scenario display is off", {
  help_html <- as.character(
    help_modal_body(
      scenario_id = "measles_current",
      show_scenario = FALSE
    )
  )

  expect_no_match(help_html, "Current scenario", fixed = TRUE)
  expect_no_match(help_html, "Measles-like (current vaccination)", fixed = TRUE)
})

test_that("help modal shows the named preset when student scenario display is on", {
  help_html <- as.character(
    help_modal_body(
      scenario_id = "measles_current",
      show_scenario = TRUE
    )
  )

  expect_match(help_html, "Current scenario", fixed = TRUE)
  expect_match(help_html, "Measles-like (current vaccination)", fixed = TRUE)
  expect_match(
    help_html,
    "High transmissibility with current vaccination levels and reduced mobility.",
    fixed = TRUE
  )
})
