# upload-calibration-data

# this script provides the process to upload calibration data for a given ecoprovince.
# Note this is expected to be an internal admin process, not for general users, however the following steps can be followed to update the internal datasets


# 1. Load the required libraries

library(wespr)
library(readxl)


# 2. read in the preprocessed file.
# Note this assumes the data has been preprossed into the standard format to be
# uploaded into the wespr calculator.

# See https://bcwf-wetlands.github.io/wespr/articles/prepare-rawdata.html for details on this
# step.

# update this line to point to specific file location
#indata <- fs::path("inst/input_data/input_refsites_GD.csv")
indata <- fs::path("inst/input_data/reference_SIM_20250620.csv")
indata <- fs::path("inst/input_data/reference_GD_20250620.csv")


# check data if needed
#check_indata(indata)

# load in the data
wesp_data <- load_wesp_data(indata)
site <- as.wesp_site(wesp_data)

# calculate the scores
# note this script generates the jenks breaks on the normalised scores based on the 100 or so
# reference site

wespclass <- calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_GD.csv")

# format output data to get the range of each catergory per service

wcols <- names(wespclass)
# remove the site column
wcols <- wcols[!wcols %in% c("site")]
wcols <- unique(sub("^([^_]*_[^_]*).*", "\\1", wcols))

# loop through the data and get the min and max values of the raw scores

outsum <- purrr::map(wcols, function(x) {
  # get the columns for each service
 # x <- wcols[1]

  tw <- wespclass |>
    dplyr::select(dplyr::starts_with(x)) |>
    dplyr::select(-dplyr::ends_with("_norm"))
  names(tw) <- c("jenks", "raw" )

  tww <- tw |>
    dplyr::group_by(jenks) |>
    dplyr::summarise(min = min(raw),
                     max = max(raw)) |>
    dplyr::mutate(service = x,
           service_name = unique(sub("^([^_]*).*", "\\1", service)),
           service_type = unique(sub("^[^_]*_", "", service)))
  tww

}) |>  dplyr::bind_rows()

# add the Eco province name

calibration_scores_new <- outsum |>
  dplyr::mutate(ecoprovince = "GD") |>
  dplyr::select(ecoprovince, service, everything())


# write out (temp fix while testing)
write.csv(calibration_scores, "temp/gd_jenks_breaks.csv", row.names = FALSE)


# if data already exists then read in first and merge or updata


if(calibration_scores)

#TODO: add calibration_scores read in check add new data to existing



# export data as part of the package

# convert to internal data
usethis::use_data(
  calibration_scores,
  internal = FALSE,
  overwrite = TRUE
)
