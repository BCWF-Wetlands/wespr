library(readr)
library(dplyr)

# Run devtools::load_all(), or in RStudio Ctrl+Shift+L to load the package
# during development, or:
# install_github("BCWF-wetlands/wespr)
# library(wespr)

# get weights table
# weights <- read_csv("inst/input_data/weights.csv")

# read in data and filter to questions we have implemented, and just one site:
data <- load_wesp_data(system.file("input_data/wetflat.csv", package = "wespr")) |>
  select(q_no, response_no, site_1)

core_questions <- record_values(data)

derived_values <- make_derived_values(core_questions)

fr_qs <- fn(core_questions, weights, fn = "FR")

# Next:
# - Add questions that create derived values (always_water etc) and generate
#   "derived_values" list
# - For ecosystem service calculation:
#     - filter questions list by used_by
#     - filter derived_values list by used_by
#     - subset and match weights from weights table
#     - ... magic
# - For multi-site, I think each question in the core_questions object can have
#   a list of values one for each site (rather than a questions object for each site)
