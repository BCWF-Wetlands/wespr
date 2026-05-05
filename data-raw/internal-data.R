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
  dplyr::select(ECOPROVINCE_CODE, ECOPROVINCE_NAME, geometry) |>
  dplyr::filter(!ECOPROVINCE_CODE %in% c("SAL","NEP" )) |>
  dplyr::mutate(eco_code = dplyr::case_when(
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

##TODO - still need to run this one

calibration_scores <- read.csv(fs::path("temp","wesp_scores_all_20260504.csv"))


## build a name key

service_app <-  c("STR", "SENS", "CRI","FR", "CP","POL", "PD",
                      "RSB", "KMH","AM", "WB", "FH", "APP","OE",
                      "SFTS", "NR","PR", "SR" ,"WS")
service_name <- c("Wetland Stressors (STR)",
                      "Wetland Sensitivity (SENS)",
                      "Cultural Recreational Importance (CRI)",
                      "Fire Resistance (FR)",
                      "Carbon Preservation (CP)",
                      "Pollinator Habitat (POL)",
                      "Native Plant Diversity (PD)",
                      "Raptor and Wetland Songbird Habitat (RSB)",
                      "Keystone Mammal Habitat (KMH)",
                      "Amphibian Habitat (AM)",
                      "Waterbird Habitat (WB)",
                      "Fish Habitat (FH)",
                      "Aquatic Primary Productivity (APP)",
                      "Organic Matter Export (OE)",
                      "Stream Flow and Temperature Support (SFTS)",
                      "Nitrate Removal and Retention (NR)",
                      "Phosphorus Retention (PR)",
                      "Sediment Retention and stabilization (SR)",
                      "Water Storage and Delay (WS)")
ind_key <- tibble(service_app, service_name)


## Generate a weights table for users to review - This is purely for info,
# no used in any calculations

wt_table <- indicator_weightings |>
  dplyr::select(q_no, response_no , q_text, q_responses,q_weighting,indicator,type_f_b ) |>
  mutate(indicator = ifelse(indicator == "Sens", "SENS", indicator)) |>
  mutate(type_f_b = case_when(
    type_f_b == "fun" ~ "function",
    type_f_b == "ben" ~ "benefit",
  ))

wt_table <- left_join(wt_table, ind_key, by = join_by(indicator ==service_app))  |>
  mutate(response_no = ifelse(response_no %in% c("????", "?????"), "Not Applicable",response_no ))




usethis::use_data(
  question_metadata,
  indicator_weightings,
  ecoprovince_sp,
  calibration_scores,
  ind_key,
  wt_table,
  internal = TRUE,
  overwrite = TRUE
)

