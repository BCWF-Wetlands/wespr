test_that("cs_f works", {
  expect_equal(round(cs_f(site, weightings), 2), 8.72)
})

test_that("ws_f works", {
  expect_equal(round(ws_f(site, weightings), 2), 8.16)
})
