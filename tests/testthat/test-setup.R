test_that("as.wesp_site works", {
  data <- load_wesp_data(test_path("test-wetflat.csv"))
  expect_s3_class(data, "data.frame")

  suppressWarnings(
    site <- as.wesp_site(data)
  )
  expect_s3_class(site, "wesp_site")
  expect_equal(names(site), c("site_name", "questions", "derived_values", "indicators"))
  expect_equal(site$site_name, "site_1")
})

test_that("site setup works", {
  data <- load_wesp_data(test_path("test-wetflat.csv"))
  expect_s3_class(data, "data.frame")

  suppressWarnings(
    site2 <- as.wesp_site(data, site = "site_2")
  )
  expect_s3_class(site2, "wesp_site")
  expect_equal(site2$site_name, "site_2")

  suppressWarnings(
    site3 <- as.wesp_site(data, site = 3)
  )
  expect_s3_class(site3, "wesp_site")
  expect_equal(site3$site_name, "site_3")
})
