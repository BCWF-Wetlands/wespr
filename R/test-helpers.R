make_test_site <- function() {
  data <- load_wesp_data(testthat::test_path("test-wetflat.csv"))
  suppressWarnings(as.wesp_site(data))
}
