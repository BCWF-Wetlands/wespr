library(readr)
library(dplyr)
library(stringr)
library(tidyr)

source("R/setup.R")

# get weights table
weights <- read_csv("input_data/weights.csv")

# read in data and filter to questions we have implemented, and just one site:
data <- load_wesp_data("input_data/wetflat.csv") |> 
  select(q_no, response_no, site_1) |> 
  filter(
    q_no %in% names(core_questions)
  )

core_questions <- record_values(core_questions, data)

derived_values <- derive_values(core_questions)

fr_qs <- fn(core_questions, weights, fn = "FR")

# Next: 
# - Check valid values against template in list elements.
#     - This should happen inside `record_values()`
# - Add questions that create derived values (always_water etc) and generate
#   "derived_values" list
# - For ecosystem service calculation:
#     - filter questions list by used_by
#     - filter derived_values list by used_by
#     - subset and match weights from weights table
#     - ... magic
# - For multi-site, I think each question in the core_questions object can have 
#   a list of values one for each site (rather than a questions object for each site)
