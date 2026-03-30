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


# march 2026 add temp fix to update the ecoprovince number - this does not seem to be working anymore?
question_metadata <- question_metadata |>
  mutate(n_responses = case_when(
    no == "OF44" ~ 8,
    TRUE ~ n_responses
  )) |>  mutate(unique_values  = case_when(
    no == "OF44" ~ "GDeco;CMeco;SIMeco;BTPeco;CIeco;NBMeco;SBIeco;SIeco",
    TRUE ~ unique_values
  ))  |>  mutate(unique_value_response = case_when(
    no == "OF44" ~ "OF44_1==1;OF44_2==1;OF44_3==1;OF44_4==1;OF44_5==1;OF44_6==1;OF44_7==1;OF44_8==1",
    TRUE ~ unique_value_response
  ))


indicator_weightings <- read_sheet(
  gs_id,
  sheet = "weightings",
  .name_repair = make_clean_names,
  col_types = "_ccccncccncc__"
) |>
  mutate(type_f_b = tolower(substr(type_f_b, 1, 3)))

# write_csv(all_indicators, "inst/input_data/all_indicators.csv")
# write_csv(weightings, "inst/input_data/weightings.csv")


##################################################################

# get the base data information for mapping report

library(bcdata)

ecoprovince_sp <-  bcdc_query_geodata('WHSE_TERRESTRIAL_ECOLOGY.ERC_ECOPROVINCES_SP') |>
  bcdata::collect() |>
  select(ECOPROVINCE_CODE, ECOPROVINCE_NAME, geometry) |>
  filter(!ECOPROVINCE_CODE %in% c("SAL","NEP" )) |>
  mutate(eco_code = dplyr::case_when(
    ECOPROVINCE_CODE == "NBM" ~ "NBM",
    ECOPROVINCE_CODE == "TAP" ~ "BTP",
    ECOPROVINCE_CODE == "BOP" ~ "BTP",
    ECOPROVINCE_CODE == "SBI" ~ "SBI",
    ECOPROVINCE_CODE == "SIM" ~ "SIM",
    ECOPROVINCE_CODE == "SOI"  ~ "SI",
    ECOPROVINCE_CODE == "COM" ~ "CM",
    ECOPROVINCE_CODE == "GED" ~ "GD",
    ECOPROVINCE_CODE == "CEI" ~ "CI"
  ))


#ecoprovince_sp <- vctrs::vec_proxy(ecoprovince_sp)
#vctrs::vec_restore(ecoprovince_sp)

calibration_scores <- read.csv(fs::path("temp","wesp_scores_all.csv")) |>
  dplyr::select(-X)

usethis::use_data(
  question_metadata,
  indicator_weightings,
  ecoprovince_sp,
  calibration_scores,
  internal = TRUE,
  overwrite = TRUE
)

