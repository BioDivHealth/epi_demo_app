library(testthat)

test_that("score band helpers map values into expected labels and classes", {
  expect_identical(score_band_label(80, 100), "Strong")
  expect_identical(score_band_label(45, 100), "Moderate")
  expect_identical(score_band_label(10, 100), "Weak")
  expect_identical(score_band_label(NA, 100), "Unavailable")

  expect_identical(score_band_class(80, 100), "score-strong")
  expect_identical(score_band_class(45, 100), "score-moderate")
  expect_identical(score_band_class(10, 100), "score-weak")
  expect_identical(score_band_class(NA, 100), "score-unavailable")
})

test_that("score and turn formatting helpers handle values and missing data", {
  expect_identical(format_score_fraction(37, 50), "37/50")
  expect_identical(format_score_fraction(NA, 50), "N/A")
  expect_identical(format_turn_label(7), "Turn 7")
  expect_identical(format_turn_label(NA), "Never")
})

test_that("response verdict text reflects control and surveillance combinations", {
  expect_identical(
    build_response_verdict(list(
      outbreak_control_score = 42,
      surveillance_quality_score = 24,
      strategy_balance_score = 16
    )),
    "You combined strong outbreak control with a trustworthy surveillance picture. Your testing and intervention choices stayed well balanced."
  )

  expect_identical(
    build_response_verdict(list(
      outbreak_control_score = 40,
      surveillance_quality_score = 10,
      strategy_balance_score = 6
    )),
    "Your interventions reduced harm, but surveillance was too thin to support confident decisions. The testing and intervention mix was also poorly balanced."
  )

  expect_identical(
    build_response_verdict(list(
      outbreak_control_score = 12,
      surveillance_quality_score = 22,
      strategy_balance_score = 11
    )),
    "You built a good picture of the outbreak, but did not convert that visibility into enough control."
  )

  expect_identical(
    build_response_verdict(list(
      outbreak_control_score = 8,
      surveillance_quality_score = 9,
      strategy_balance_score = 5
    )),
    "The strategy was both hard to interpret and weak at reducing harm. The testing and intervention mix was also poorly balanced."
  )
})

test_that("endgame modal includes response score summary and updated surveillance labels", {
  metrics <- list(
    response_score = 74,
    outbreak_control_score = 39,
    surveillance_quality_score = 20,
    strategy_balance_score = 15,
    first_signal_turn = 0,
    player_confirmation_turn = 4,
    true_prevalence_rate = 0.0015,
    rt_observed_mean = 1.1,
    rt_observed_lower = 0.8,
    rt_observed_upper = 1.4,
    rt_truth_mean = 0.9,
    rt_truth_lower = 0.7,
    rt_truth_upper = 1.1,
    peak_true_incidence = 112,
    peak_observed_positives = 8,
    total_spend = 120000
  )

  comparison <- list(
    cases_averted = 1234,
    deaths_averted = 12,
    peak_delay_turns = -2,
    spend_delta = 120000
  )

  modal <- build_endgame_modal(metrics, comparison, detection_rate = 3.4)
  modal_html <- as.character(modal)

  expect_match(modal_html, "endgame-response_score_card", fixed = TRUE)
  expect_match(modal_html, "endgame-response_score_value", fixed = TRUE)
  expect_match(modal_html, "endgame-subscore-outbreak", fixed = TRUE)
  expect_match(modal_html, "endgame-subscore-surveillance", fixed = TRUE)
  expect_match(modal_html, "endgame-subscore-balance", fixed = TRUE)
  expect_match(modal_html, "Response Score", fixed = TRUE)
  expect_match(modal_html, "First signal", fixed = TRUE)
  expect_no_match(modal_html, "Player confirmation", fixed = TRUE)
})
