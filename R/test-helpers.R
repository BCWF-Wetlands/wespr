make_test_site <- function(site = NULL) {
  data <- load_wesp_data(testthat::test_path("test-wetflat.csv"))
  suppressWarnings(as.wesp_site(data, site = site))
}
