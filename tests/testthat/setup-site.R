library(readr)
library(dplyr)

# Run devtools::load_all(), or in RStudio Ctrl+Shift+L to load the package
# during development, or:
# install_github("BCWF-wetlands/wespr)
# library(wespr)

# get weights table
weightings <- read_csv(system.file("input_data/weightings.csv", package = "wespr"))

# read in data and filter to questions we have implemented, and just one site:
data <- load_wesp_data("wetFlat.csv") |>
  select(q_no, response_no, site_1)

# temporary hack to remove extra calculated rows in some of the S questions in
# wetFlat.csv, and add missing rows
data <- data |>
  filter(grepl("^S[1-5]", q_no)) |>
  group_by(q_no) |>
  slice_head(n = -2) |>
  bind_rows(
    filter(data, !grepl("^S[1-5]", q_no)),
    tibble(
      q_no = c("S6", "S6"),
      response_no = c("5","6"),
      site_1 = c("0","0")
    )
  ) |>
  arrange(q_no)

site <- as.wesp_site(data)
