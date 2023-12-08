test_that("wt_max works", {
  indicator_data <- get_indicator_data(site, weightings, "ws")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  expect_equal(
    wt_max(indicator_data, "F44", "function"),
    max_na(
      vals$F44_1 * weights$WF44_1,
      vals$F44_2 * weights$WF44_2,
      vals$F44_3 * weights$WF44_3,
      vals$F44_4 * weights$WF44_4
    ) / max_na(
      weights$WF44_1,
      weights$WF44_2,
      weights$WF44_3,
      weights$WF44_4
    )
  )
})
