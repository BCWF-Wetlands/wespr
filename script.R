library(readr)
library(dplyr)
library(stringr)

source("office.R")

weights <- read_csv("input_data/weights.csv")
data <- read_csv("input_data/wetflat.csv", name_repair = "universal") |> 
  # TODO: rename(across(starts_with("..."), function) and add site_
  select(1:2) |> 
  rename(
    response_no = Question,
  ) |> 
  mutate(
   q_no = str_split_i(response_no, "_", 1)
  ) |> 
  select(q_no, everything()) |> 
  filter(
    q_no %in% names(questions)
  )

lapply(questions, function(x) {
  values <- data[data$q_no == x$name, "...1"]
  x$value <- values
  x
})

# Next: 
# - Check valid values against template in list elements
# - Add questions that create derived values (always_water etc) and generate
#   "derived_values" list
# - look at purrr function that captures names
# - For ecosystem service calculation:
#     - filter questions list by used_by
#     - filter derived_values list by used_by
#     - subset and match weights from weights table
#     - ... magic
