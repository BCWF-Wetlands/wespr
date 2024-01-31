library(googlesheets4)
library(dplyr)
library(readr)
library(janitor)

# Edit spreadsheet at:
# https://docs.google.com/spreadsheets/d/1l2h7Z65H5z0cKv_gvorxkT6k9LC7ZKLS/edit#gid=924841838
#
# Then run this script to update the internal datasets `question_metadata` and
# `indicator_weightings`
#
# To authorize the googlesheets download, set your google auth email with:
# options(
#   gargle_oauth_email = "email.which.gives.you.access.to.these.files@gmail.com"
# )
# If this is different from your normal google auth email you can add this to a
# project-specific .Rprofile file to cache

gs_id <- "1kk_RT7_cz6yT6hBYx6Es5ZIie3V9IU85ZkraJBDHTj8"

question_metadata <- read_sheet(gs_id, sheet = "all_indicators", col_types = "c",
                             .name_repair = make_clean_names) |>
  filter(!is.na(no), no != "score") |>
  mutate(n_responses = as.integer(n_responses)) |>
  select(-starts_with("x"), -no_indicators)

indicator_weightings <- read_sheet(
  gs_id,
  sheet = "weightings",
  .name_repair = make_clean_names,
  col_types = "_ccccncccncc__"
) |>
  mutate(type_f_b = tolower(substr(type_f_b, 1, 3)))

# write_csv(all_indicators, "inst/input_data/all_indicators.csv")
# write_csv(weightings, "inst/input_data/weightings.csv")

usethis::use_data(
  question_metadata,
  indicator_weightings,
  internal = TRUE,
  overwrite = TRUE
)
