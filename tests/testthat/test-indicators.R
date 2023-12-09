# These values aren't necessarily the "Truth", but are a baseline to compare
# against during refactoring. It's ok to change them if you knowingly fix
# a bug or change functionality in the indicator calculations

test_that("cs_f works", {
  expect_equal(round(cs_f(site, weightings), 2), 8.72)
})

test_that("ws_f works", {
  expect_equal(round(ws_f(site, weightings), 2), 8.16)
})

test_that("ws_b works", {
  expect_equal(round(ws_f(site, weightings), 2), 5.89)
})
