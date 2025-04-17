make_test_site <- function(site = NULL, quiet = TRUE) {
  #data <- load_wesp_data(testthat::test_path("test-wetflat.csv"))
  data <- load_wesp_data(testthat::test_path("test-wetflat_20250417.csv"))

  f <- if (quiet) suppressMessages else identity
  f(
    as.wesp_site(data, site = site)
  )
}
