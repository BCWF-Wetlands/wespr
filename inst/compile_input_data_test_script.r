## TESTING AND DEVELOPMENT SCRIPT

# This is the testing and development script to use to test the combine_rawdat function.
# Use is for internal and development only and not for pacakge use.

# load libraries
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(dplyr)
library(readr)
library(openxlsx)

load_all()

#Preparing the raw data
#field_data <- system.file("extdata/field_survey123_edited.xls", package = "wespr")
field_data <- system.file("extdata/field_survey123_edited_04.14.2025.xls", package = "wespr")

field_data <- fs::path("inst" , "input_data", "raw", "survey_123_raw_outputs","test_field.xls")
#list.files(data_raw)


#office_data <- system.file("extdata/scripted_office.xlsx", package = "wespr")
office_data <- system.file("extdata/GD_OF_Answers.data.xlsx", package = "wespr")
office_data <- system.file("extdata/SIM_OF_Answers.data.xlsx", package = "wespr")


ww <- combine_rawdata(
    field_data <-  field_data ,
    office_data <- office_data,
    EcoP = "SIM",
    write_subfiles = FALSE,
    out_dir <- "inst/input_data/processed1",
    overwrite = TRUE
)

ww$Question

write.csv(ww, fs::path("inst/input_data/reference_SIM_20250620.csv"), row.names=FALSE)
write.csv(ww, fs::path("inst/input_data/reference_GD_20250620.csv"), row.names=FALSE)



#indata <- fs::path("inst/input_data/wetFlat_20250417.csv")
indata <- fs::path("inst/input_data/reference_SIM_20250620.csv")
indata <- fs::path("inst/input_data/reference_GD_20250620.csv")

#check_indata(indata)

wesp_data <- load_wesp_data(indata)
site <- as.wesp_site(wesp_data)


calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_GD.csv")
calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_SIM.csv")





# check old file
#wesp_file <- system.file("input_data/wetFlat_20240130.csv", package = "wespr")
#wesp_data <- load_wesp_data(wesp_file)
#head(wesp_data)

#wesp_file <- read.csv("inst/input_data/wetFlat_20240130.csv")
#setdiff(names(wesp), names(wesp_file))
#setdiff(wesp$Question, wesp_file$Question)
#[1] "F4_4" (error fix)
# "F57_7"  (error fix)
 #"OF15"       "OF16"       "OF17"       "OF21"       "OF22"  (were previously _1)

#setdiff(wesp_file$Question, wesp$Question)
#[1] "F22_1"  "F23_1"  "F42_1"
#"F49_1"  #"OF15_1" "OF16_1" "OF17_1" "OF21_1" "OF22_1"




#
# # run the data with new data
# wesp <- fs::path("inst/input_data/wetFlat_202503241.csv")
# wesp_data <- load_wesp_data(wesp)
#
# # 2)  run all sites
# site <- as.wesp_site(wesp_data, site = 1)
# calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_test1.csv")
#
# #Write out the data frame
# write.csv(wespEcoS, fs::path(out_dir, out_name),row.names=FALSE)


### QA data
# #format to long form
#
# norm_long <- pivot_longer(wespNorm, -site, names_to = "metric", values_to = "value") |>
#   mutate(type = "normalised") |>
#   mutate(name = gsub( "_norm", "", metric))
#
# norm_raw <- pivot_longer( wespRaw, -site, names_to = "metric", values_to = "value") |>
#   mutate(type = "raw")|>
#   mutate(name = gsub( "_raw", "", metric))
#
# all <- rbind(norm_long, norm_raw)
#
# ggplot( norm_raw, aes(value, fill = type)) +
#    geom_density(alpha = 0.2) +
#    facet_wrap(~name, scales = "free") +
#    theme_bw()
#

###########################################################################

library(wespr)
library (fs)
library(dplyr)

#field_data <- fs::path("inst",'input_data','raw',"20251217", "field_assessment_by_class_20Nov2025.csv")
#desktop_data <- fs::path("inst",'input_data','raw', "20251217", "office_data_by_class_formatted_for_R_12Dec2025.csv")

field_data <- fs::path("inst",'input_data','raw',"20251204", "WESP_FIELD_MA.csv")
desktop_data <- fs::path("inst",'input_data','raw', "20251204", "WESP_DESKTOP_MA.csv")

field_data <- fs::path("inst",'input_data','raw',"20251222", "WESP_Field_AE2.csv")
desktop_data <- fs::path("inst",'input_data','raw', "20251222", "WESP_Desktop_AE2.csv")

ww <- format_rawinputs(
  field_data <- field_data,
  desktop_data <- desktop_data,
  write_subfiles = FALSE,
  out_dir = "temp",
  overwrite = TRUE
)

write.csv(ww, fs::path("inst",'input_data','raw',"20260105","wesp_input_20260105.csv"), row.names=FALSE)

indata <- fs::path("inst",'input_data','raw',"20260105","wesp_input_20260105.csv")

check_indata(indata)


wesp_file <- indata
wesp_data <- load_wesp_data(wesp_file)

head(wesp_data)

# generate a key for site names
wespkey <- wesp_data |>
  dplyr::filter(.data$q_no == "Wetland" ) |>
  tidyr::pivot_longer(cols = -c(.data$response_no),
                      names_to = "site_no",
                      values_to = "wetland_id"
  ) |>
  dplyr::select(-response_no) |>
  dplyr::filter(site_no != "q_no") |>
  dplyr::rename("site" = .data$site_no)




site_overall <- purrr::map(c(1:4), function(x){

  x <- c(1:4)[1]
  site <- as.wesp_site(wesp_data, site = x)

  site

  site <- calc_indicators(site)

  ind_scores <- get_indicator_scores(site)

  # add site specific names
  ind_scores <- dplyr::left_join(wespkey, ind_scores, by = "site")

  ind_scores

  get_responses(site)
  get_derived_values(site)

  out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "GD", report = TRUE, output_dir = "temp")

  out

}) |> bind_rows()


site_overall <- left_join(site_overall, wespkey)


write.csv(site_overall, fs::path("inst",'input_data','raw',"20260105","wesp_output_20260109.csv"), row.names=FALSE)





#Preparing the raw data
# check old data format from contractors

field_data <- fs::path("inst",'input_data','raw',"20251217", "field_assessment_by_class_20Nov2025.xls")
office_data <- fs::path("inst",'input_data','raw', "20251217", "office_data_by_class_formatted_for_R_12Dec2025.xlsx")


ww <- combine_rawdata(
  field_data <-  field_data ,
  office_data <- office_data,
  EcoP = "GD",
  write_subfiles = FALSE,
  out_dir <- "inst/input_data/raw/20251217",
  overwrite = TRUE
)

ww$Question

write.csv(ww, fs::path("inst/input_data/reference_SIM_20250620.csv"), row.names=FALSE)
write.csv(ww, fs::path("inst/input_data/reference_GD_20250620.csv"), row.names=FALSE)



#indata <- fs::path("inst/input_data/wetFlat_20250417.csv")
indata <- fs::path("inst/input_data/reference_SIM_20250620.csv")
indata <- fs::path("inst/input_data/reference_GD_20250620.csv")

#check_indata(indata)

wesp_data <- load_wesp_data(indata)
site <- as.wesp_site(wesp_data)


calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_GD.csv")
calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_SIM.csv")





# check old file
wesp_file <- system.file("input_data/wetFlat_20240130.csv", package = "wespr")
wesp_data <- load_wesp_data(wesp_file)
head(wesp_data)

wesp_file <- read.csv("inst/input_data/wetFlat_20240130.csv")
#setdiff(names(wesp), names(wesp_file))
setdiff(wesp$Question, wesp_file$Question)
#[1] "F4_4" (error fix)
# "F57_7"  (error fix)
#"OF15"       "OF16"       "OF17"       "OF21"       "OF22"  (were previously _1)

setdiff(wesp_file$Question, wesp$Question)
#[1] "F22_1"  "F23_1"  "F42_1"
#"F49_1"  #"OF15_1" "OF16_1" "OF17_1" "OF21_1" "OF22_1"





# run the data with new data
wesp <- fs::path("inst/input_data/wetFlat_202503241.csv")
wesp_data <- load_wesp_data(wesp)

# 2)  run all sites
site <- as.wesp_site(wesp_data, site = 1)
calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_test1.csv")

#Write out the data frame
write.csv(wespEcoS, fs::path(out_dir, out_name),row.names=FALSE)



