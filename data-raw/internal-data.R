library(googledrive)
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

xl <- drive_find("WSP_calculations.xlsx")

# Since it's an excel file, make a temporary copy as a google sheet so we can
# use `read_sheet()`
tmp_xl_gs <- drive_cp(xl, name = "tmp-wsp-calcs", mime_type = drive_mime_type("spreadsheet"))

question_metadata <- read_sheet(tmp_xl_gs, sheet = "all_indicators", col_types = "c",
                             .name_repair = make_clean_names) |>
  filter(!is.na(no), no != "score") |>
  mutate(n_responses = as.integer(n_responses)) |>
  select(-starts_with("x"), -no_indicators)

indicator_weightings <- read_sheet(
  tmp_xl_gs,
  sheet = "weightings",
  .name_repair = make_clean_names,
  col_types = "_ccccncccncc__"
)

# Discard the temporary google sheet
drive_rm(tmp_xl_gs)

# write_csv(all_indicators, "inst/input_data/all_indicators.csv")
# write_csv(weightings, "inst/input_data/weightings.csv")

usethis::use_data(
  question_metadata,
  indicator_weightings,
  internal = TRUE,
  overwrite = TRUE
)
