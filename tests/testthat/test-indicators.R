# These values aren't necessarily the "Truth", but are a baseline to compare
# against during refactoring. It's ok to change them if you knowingly fix
# a bug or change functionality in the indicator calculations

test_that("indicator calculations work (site 1)", {
  site <- make_test_site()
  expect_type(ws_fun(site), "list")
  expect_type(ws_ben(site), "double")
  expect_type(sr_fun(site), "list")
  expect_type(sr_ben(site), "double")
  expect_type(cp_fun(site), "double")
})


test_that("indicator calculations work (site 2)", {
  site <- make_test_site(site = 2)
  expect_type(ws_fun(site), "list")
  expect_type(ws_ben(site), "double")
  expect_type(sr_fun(site), "list")
  expect_type(sr_ben(site), "double")
  expect_type(cp_fun(site), "double")
})

test_that("indicator calculations work (site 3)", {
  site <- make_test_site(site = "site_3")
  expect_type(ws_fun(site), "list")
  expect_type(ws_ben(site), "double")
  expect_type(sr_fun(site), "list")
  expect_type(sr_ben(site), "double")
  expect_type(cp_fun(site), "double")
})

test_that("updating a site with indicator value works", {
  site <- make_test_site()
  ref <- ws_fun(site)
  site <- update_site_indicator(site, "ws", "fun")
  expect_s3_class(site, "wesp_site")
  expect_equal(site$indicators$ws$fun, ref)
})

test_that("update_site_indicator errors correctly", {
  site <- make_test_site()
  expect_snapshot(update_site_indicator(list(), "ws"), error = TRUE)
  expect_snapshot(update_site_indicator(site, "PEANUT BUTTER COW"), error = TRUE)
  expect_snapshot(update_site_indicator(site, "cp", "ben"), error = TRUE)
})

test_that("update_site_indicator warns when exisiting value", {
  site <- make_test_site()
  site$indicators$ws$ben <- as.indicator_score(1.0)
  expect_warning(
    update_site_indicator(site, "ws", "ben"),
    "has exisiting value"
  )
})

test_that("calc_indicators calculates all indicators", {
  site <- make_test_site()
  site <- calc_indicators(site)
  expect_s3_class(site$indicators$ws$fun, c("with_subscores", "indicator_score"))
  expect_s3_class(site$indicators$ws$ben, "indicator_score")
  expect_s3_class(site$indicators$sr$fun, c("with_subscores", "indicator_score"))
  expect_s3_class(site$indicators$sr$ben, "indicator_score")
  expect_s3_class(site$indicators$cp$fun, "indicator_score")
})
