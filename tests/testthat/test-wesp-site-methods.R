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
  expect_equal(nrow(get_indicator_scores(site)), 18)
  expect_equal(ncol(get_indicator_scores(site)), 4)
})

test_that("get_indicator_scores() fails with wrong input", {
  expect_error(get_indicator_scores(list(), "No method"))
})

test_that("get_responses() works", {
  site <- make_test_site()
  expect_s3_class(get_responses(site), "data.frame")
  expect_equal(nrow(get_responses(site)), 488)
  expect_equal(ncol(get_responses(site)), 4)
})

test_that("get_responses() fails with wrong input", {
  expect_error(get_responses(list(), "No method"))
})

test_that("get_derived_values() works", {
  site <- make_test_site()
  expect_s3_class(get_derived_values(site), "data.frame")
  expect_equal(nrow(get_derived_values(site)), 41)
  expect_equal(ncol(get_derived_values(site)), 2)
})

test_that("get_derived_values() fails with wrong input", {
  expect_error(get_derived_values(list(), "No method"))
})
