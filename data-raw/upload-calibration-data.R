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

#indata <- fs::path("inst/input_data/reference_SIM_20250620.csv"); ecop <- "SIM"
#indata <- fs::path("inst/input_data/reference_GD_20250620.csv"); ecop <- "GD"
#indata <- fs::path("inst/input_data/reference_SI_20260319.csv"); ecop <- "SI"
indata <- fs::path("inst/input_data/reference_SBI_20260319.csv"); ecop <- "SBI"
#indata <- fs::path("inst/input_data/reference_TBP_20260319.csv"); ecop <- "TBP"
#indata <- fs::path("inst/input_data/reference_CI_20260319.csv");  ecop <- "CI"


# Read in data into wesp format
wesp_data <- load_wesp_data(indata)

# generate a site key
wespkey <- wespr::generate_ids(wesp_data)

# calculate the scores
# note this script generates the jenks breaks on the normalised scores based on the 100 or so
# reference site

wespclass <- calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = paste0("wesp_scores_", ecop, ".csv"))


# add the Eco province name
calibration_scores_new <- wespclass |>
  dplyr::mutate(ecoprovince = ecop) #|>
 # dplyr::select(-wetland_id)

 calibration_scores_ci <- calibration_scores_new
#calibration_scores_tbp <- calibration_scores_new
# calibration_scores_sbi <- calibration_scores_new
#calibration_scores_si <- calibration_scores_new
#calibration_scores_gd <- calibration_scores_new
#calibration_scores_sim <- calibration_scores_new


calibration_scores <- dplyr::bind_rows(calibration_scores_gd, calibration_scores_sim)
calibration_scores <- dplyr::bind_rows(calibration_scores, calibration_scores_si)
calibration_scores <- dplyr::bind_rows(calibration_scores, calibration_scores_sbi)
calibration_scores <- dplyr::bind_rows(calibration_scores, calibration_scores_tbp)
calibration_scores <- dplyr::bind_rows(calibration_scores, calibration_scores_ci)


check <- calibration_scores |>
  group_by(ecoprovince) |>
             count()
calibration_scores <- calibration_scores |>
  filter(ecoprovince !=  "SBI")

calibration_scores <- rbind(calibration_scores, calibration_scores_new)

write.csv(calibration_scores, fs::path("temp", "wesp_scores_all.csv"))


# if data already exists then read in first and merge or updata

if(!missing(calibration_scores)){
  # read in the existing calibration scores
  calibration_scores <- wespr::calibration_scores
  # check if the new data is already in the existing data
  if (any(calibration_scores$ecoprovince %in% ecop)) {

    cli::cli_alert_warning("Calibration scores for {ecop} already exist. Updating existing data.")

    calibration_scores_to_update <- calibration_scores |>
      dplyr::filter(.data$ecoprovince != ecop)

    calibration_scores <- bind_rows(calibration_scores_to_update, calibration_scores_new)

    # update the existing data with the new data
  } else {
    # add the new data to the existing data
    calibration_scores <- dplyr::bind_rows(calibration_scores, calibration_scores_new)
  }
} else {
  # if no existing data then just use the new data
  calibration_scores <- calibration_scores_new
}

#calibration_scores = NA
# export data as part of the package

calibration_scores

# convert to internal data
usethis::use_data(
  calibration_scores,
  internal = TRUE,
  overwrite = TRUE
)
