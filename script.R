library(readr)
library(dplyr)
library(stringr)

source("R/objects.R")

# get weights table
weights <- read_csv("input_data/weights.csv")

# read in data and get in shape. This will go in a read_questions() function
data <- read_csv("input_data/wetflat.csv", name_repair = "universal") |> 
  rename_with(
    \(x) gsub("...", "site_", x), 
    starts_with("...")
    ) |> 
  select(1:2) |> 
  rename(
    response_no = Question,
  ) |> 
  mutate(
   q_no = str_split_i(response_no, "_", 1)
  ) |> 
  select(q_no, everything()) |> 
  filter(
    q_no %in% names(core_questions)
  )

core_questions <- record_values(core_questions, data)

# Next: 
# - Check valid values against template in list elements.
#     - This should happen inside `record_values()`
# - Add questions that create derived values (always_water etc) and generate
#   "derived_values" list
# - look at purrr function that captures names
# - For ecosystem service calculation:
#     - filter questions list by used_by
#     - filter derived_values list by used_by
#     - subset and match weights from weights table
#     - ... magic
# - For multi-site, I think each question in the core_questions object can have 
#   a list of values one for each site (rather than a questions object for each site)
