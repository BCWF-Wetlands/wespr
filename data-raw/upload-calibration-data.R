# upload-calibration-data

# this script provides the process to upload calibration data for a given ecoprovince.
# Note this is expected to be an internal admin process, not for general users, however the following steps can be followed to update the internal datasets


# 1. Load the required libraries

library(wespr)
library(readxl)
library(dplyr)



# 2. read in the preprocessed file.
# Note this assumes the data has been preprossed into the standard format to be
# uploaded into the wespr calculator.

# See https://bcwf-wetlands.github.io/wespr/articles/prepare-rawdata.html for details on this
# step.

# update this line to point to specific file location
#indata <- fs::path("inst/input_data/input_refsites_GD.csv")
indata <- fs::path("inst/input_data/reference_SIM_20250620.csv")
indata <- fs::path("inst/input_data/reference_GD_20250620.csv")

ecop <- "SIM"



# load in the data
wesp_data <- load_wesp_data(indata)
site <- as.wesp_site(wesp_data)

# calculate the scores
# note this script generates the jenks breaks on the normalised scores based on the 100 or so
# reference site

wespclass <- calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = paste0("wesp_scores_", ecop, ".csv"))


#export as full dataset - testing this option


# add the Eco province name
calibration_scores_new <- wespclass |>
  dplyr::mutate(ecoprovince = ecop) |>
  dplyr::select(-wetland_id)





# format output data to get the range of each category per service

#wcols <- names(wespclass)
## remove the site column
#wcols <- wcols[!wcols %in% c("site", "wetland_id")]
#wcols <- unique(sub("^([^_]*_[^_]*).*", "\\1", wcols))

# loop through the data and get the min and max values of the raw scores

# outsum <- purrr::map(wcols, function(x) {
#   # get the columns for each service
#  # x <- wcols[1]
#
#   tw <- wespclass |>
#     dplyr::select(dplyr::starts_with(x)) |>
#     dplyr::select(-dplyr::ends_with("_norm"))
#   names(tw) <- c("jenks", "raw" )
#
#   tww <- tw |>
#     dplyr::group_by(jenks) |>
#     dplyr::summarise(min = min(raw),
#                      max = max(raw)) |>
#     dplyr::mutate(service = x,
#            service_name = unique(sub("^([^_]*).*", "\\1", service)),
#            service_type = unique(sub("^[^_]*_", "", service)))
#   tww
#
# }) |>  dplyr::bind_rows()

# add the Eco province name
#calibration_scores_new <- outsum |>
#  dplyr::mutate(ecoprovince = ecop) |>
#  dplyr::select(ecoprovince, service, everything())


# write out (temp fix while testing)
#write.csv(calibration_scores_new, "temp/sim_jenks_breaks.csv", row.names = FALSE)


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

# convert to internal data
usethis::use_data(
  calibration_scores,
  internal = FALSE,
  overwrite = TRUE
)
