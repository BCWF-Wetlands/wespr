test_that("print method works", {
  site <- make_test_site()
  expect_snapshot(print(site))
  expect_snapshot(print(calc_indicators(site)))
})

test_that("get_indicator_scores() works", {
  site <- make_test_site()
  expect_s3_class(get_indicator_scores(site), "data.frame")
  expect_equal(nrow(get_indicator_scores(site)), 0)

  site <- calc_indicators(site)
  expect_s3_class(get_indicator_scores(site), "data.frame")
  expect_equal(nrow(get_indicator_scores(site)), 3)
  expect_equal(ncol(get_indicator_scores(site)), 4)
})

test_that("get_indicator_scores() fails with wrong input", {
  expect_error(get_indicator_scores(list(), "No method"))
})
