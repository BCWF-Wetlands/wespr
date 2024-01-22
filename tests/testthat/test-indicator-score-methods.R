test_that("as.indicator_score works", {
  expect_s3_class(
    as.indicator_score(score = 5, subscores = c(foo = 1, bar = 2)),
    c("with_subscores", "indicator_score")
  )
  expect_s3_class(
    as.indicator_score(score = 5),
    "indicator_score"
  )
})

test_that("as.indicator_score fails correctly", {
  expect_error(as.indicator_score(score = 5, subscores = "hi"))
  expect_error(as.indicator_score(score = 5, subscores = 1:2))
  expect_error(as.indicator_score(score = "hi"))
})

test_that("print method works", {
  site <- make_test_site()
  site <- calc_indicators(site)
  expect_snapshot(site$indicators)
})
