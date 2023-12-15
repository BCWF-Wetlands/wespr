# These values aren't necessarily the "Truth", but are a baseline to compare
# against during refactoring. It's ok to change them if you knowingly fix
# a bug or change functionality in the indicator calculations

test_that("indicator calculations work (site 1)", {
  site <- make_test_site()
  expect_equal(round(ws_function(site), 2), 7.02)
  expect_equal(round(ws_benefit(site), 2), 5.89)
  expect_equal(round(sr_function(site), 2), 10)
  expect_equal(round(sr_benefit(site), 2), 3.67)
  expect_equal(round(cs_function(site), 2), 8.72)
})


test_that("indicator calculations work (site 2)", {
  site <- make_test_site(site = 2)
  expect_equal(round(ws_function(site), 2), 4.49)
  expect_equal(round(ws_benefit(site), 2), 10)
  expect_equal(round(sr_function(site), 2), 1.02)
  expect_equal(round(sr_benefit(site), 2), 3.71)
  expect_equal(round(cs_function(site), 2), 6.77)
})

test_that("indicator calculations work (site 3)", {
  site <- make_test_site(site = "site_3")
  expect_equal(round(ws_function(site), 2), 6.76)
  expect_equal(round(ws_benefit(site), 2), 4.56)
  expect_equal(round(sr_function(site), 2), 4)
  expect_equal(round(sr_benefit(site), 2), 3.62)
  expect_equal(round(cs_function(site), 2), 9.09)
})

test_that("updating a site with indicator value works", {
  site <- make_test_site()
  ref <- ws_function(site)
  site <- update_site_indicator(site, "ws", "function")
  expect_s3_class(site, "wesp_site")
  expect_equal(site$indicators$ws[["function"]], ref)
})

test_that("update_site_indicator errors correctly", {
  site <- make_test_site()
  expect_snapshot(update_site_indicator(list(), "ws"), error = TRUE)
  expect_snapshot(update_site_indicator(site, "blah"), error = TRUE)
  expect_snapshot(update_site_indicator(site, "cs", "benefit"), error = TRUE)
})

test_that("update_site_indicator warns when exisiting value", {
  site <- make_test_site()
  site$indicators$ws$benefit <- 1.0
  expect_warning(
    update_site_indicator(site, "ws", "benefit"),
    "has exisiting value"
  )
})

test_that("calc_indicators calculates all indicators", {
  site <- make_test_site()
  # expect_snapshot_value(calc_indicators(site)$indicators, style = "json2")
  site <- calc_indicators(site)
  expect_type(site$indicators$ws[["function"]], "double")
  expect_type(site$indicators$ws$benefit, "double")
  expect_type(site$indicators$sr[["function"]], "double")
  expect_type(site$indicators$sr$benefit, "double")
  expect_type(site$indicators$cs[["function"]], "double")
})
