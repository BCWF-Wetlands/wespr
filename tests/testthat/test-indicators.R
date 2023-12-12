# These values aren't necessarily the "Truth", but are a baseline to compare
# against during refactoring. It's ok to change them if you knowingly fix
# a bug or change functionality in the indicator calculations

test_that("cs_f works", {
  expect_equal(round(cs_func(site), 2), 8.72)
})

test_that("ws_f works", {
  expect_equal(round(ws_func(site), 2), 8.16)
})

test_that("ws_b works", {
  expect_equal(round(ws_benefit(site), 2), 5.89)
})

test_that("updating a site with indicator value works", {
  ref <- ws_func(site)
  site <- update_site_indicator(site, "ws", "func")
  expect_s3_class(site, "wesp_site")
  expect_equal(site$indicators$ws$func, ref)
})

test_that("update_site_indicator errors correctly", {
  expect_snapshot(update_site_indicator(list(), "ws"), error = TRUE)
  expect_snapshot(update_site_indicator(site, "blah"), error = TRUE)
  expect_snapshot(update_site_indicator(site, "cs", "benefit"), error = TRUE)
})
