## TESTING AND DEVELOPMENT SCRIPT

# This is the testing and development script to use to test the combine_rawdat function.
# Use is for internal and develpoment only and not for pacakge use.

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
field_data <- system.file("extdata/field_survey123_edited.xls", package = "wespr")
office_data <- system.file("extdata/scripted_office.xlsx", package = "wespr")

ww <- combine_rawdata(
    field_data <-  field_data ,
    office_data <- office_data,
    EcoP = "GD",
    write_subfiles = FALSE,
    out_dir <- "inst/input_data/processed",
    overwrite = TRUE
)

write.csv(ww, fs::path("inst/input_data/wetFlat_20250325.csv"), row.names=FALSE)


indata <- fs::path("inst/input_data/wetFlat_20250325.csv")

check_indata(indata)






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






