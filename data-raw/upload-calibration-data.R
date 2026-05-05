# upload-calibration-data

# this script provides the process to upload calibration data for a given ecoprovince.
# Note this is expected to be an internal admin process, not for general users, however the following steps can be followed to update the internal datasets


# 1. Load the required libraries

library(wespr)
library(readxl)
library(dplyr)


# 2. read in the prepossessed file.
# Note this assumes the data has been prepossessed into the standard format to be
# uploaded into the wespr calculator.

# See https://bcwf-wetlands.github.io/wespr/articles/prepare-rawdata.html for details on this
# step.

# update this line to point to specific file location

indata <- fs::path("inst/input_data/reference_SIM_20260331.csv"); ecop <- "SIM"
indata <- fs::path("inst/input_data/reference_GD_20250620.csv"); ecop <- "GD"
indata <- fs::path("inst/input_data/reference_SI_20260319.csv"); ecop <- "SI"
indata <- fs::path("inst/input_data/reference_SBI_20260319.csv"); ecop <- "SBI"
indata <- fs::path("inst/input_data/reference_TBP_20260319.csv"); ecop <- "TBP"
indata <- fs::path("inst/input_data/reference_CI_20260319.csv");  ecop <- "CI"


# Read in data into wesp format
wesp_data <- load_wesp_data(indata)
# generate a site key
wespkey <- wespr::generate_ids(wesp_data)

# calculate the scores
wespclass <- calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = paste0("wesp_scores_", ecop, ".csv"))

# get list of csvs
lst <- list.files(fs::path("temp"), pattern = "wesp_score*")
lst <- lst[3:8]

cal_combined <- purrr::map(lst, function(i){

  #i <- lst[1]
  ecopr <- gsub("wesp_scores_", "", i)
  ecopr <- gsub(".csv", "", ecopr)

  fl <- read.csv(fs::path("temp",i))
  fl <- fl |>
    dplyr::mutate(ecoprovince = ecopr)

  fl
}) |> bind_rows()

unique(cal_combined$ecoprovince)


write.csv(cal_combined, fs::path("temp", "wesp_scores_all_20260504.csv"), row.names = FALSE)

## once this is generated it needs to be run through the "update_calibrarion_data_internal.R script"

# note you need to output this updated version by rerunning this https://github.com/BCWF-Wetlands/wespr/blob/main/data-raw/upload-calibration-data.R
# with all the internal data in one hit.

# # convert to internal data
# usethis::use_data(
#   calibration_scores,
#   internal = TRUE,
#   overwrite = TRUE
# )
