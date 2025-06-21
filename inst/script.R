# Run
# devtools::load_all()#, or in RStudio Ctrl+Shift+L to load the package
# during development, or:
# install_github("BCWF-wetlands/wespr)
# library(wespr)



# download and run libraries for required processing

install_github("BCWF-wetlands/wespr")
library(wespr)

library(readr)
library(dplyr)



# read in data and filter to questions we have implemented, and just one site:
#data <- load_wesp_data(system.file("input_data/wetFlat_20250417.csv", package = "wespr"))
data <- load_wesp_data(system.file("input_data/reference_SIM_20250620.csv", package = "wespr"))


head(data)

# convert the first site to wesp format
wespsite <- as.wesp_site(data)

# convert a single site to wesp format
wespsite_specific <- as.wesp_site(data,21)

site <- wespsite_specific



# calculate the indicators
site_calcs <- calc_indicators(wespsite)


# review the outputs

## we can also pull specific values
get_q(site_calcs, "F1_1")
get_q(site_calcs, "NeverWater")


# get the responses from the site
get_responses(site_calcs)

# get the derived values
get_derived_values(site_calcs)




# convert to a data frame indicator data into a easy to work with format
site_calcs_summ = get_indicator_scores(site_calcs)




# read in data and calculate the values for all sites

data <- load_wesp_data(system.file("input_data/wetFlat_20250417.csv", package = "wespr"))

allsites <- calculate_multi_site(data)


allsites



# import a site and compare a single site to the calibration dataset


data <- load_wesp_data(system.file("input_data/wetFlat_20250417.csv", package = "wespr"))
data <- load_wesp_data(system.file("input_data/reference_SIM_20250620.csv", package = "wespr"))


wespkey <- data |>
  dplyr::filter(.data$q_no == "Wetland" ) |>
  tidyr::pivot_longer(cols = -c(.data$response_no),
                      names_to = "site",
                      values_to = "wetland_id"
  ) |>
  dplyr::select(-response_no) |>
  dplyr::filter(site != "q_no")

site <- as.wesp_site(data, 1)
site <- calc_indicators(site)
ind_scores <- get_indicator_scores(site)


out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "SIM")

out <- out |>
  dplyr::left_join(wespkey, by = "site") |>
  select(wetland_id, site, indicator,value, service_type, calibration_scores_eco)

head(out)


# Some ecoprovinces are yet to have calibration data. For example

out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "SK")


# make report? template





###################################################################


# advanced topics


# generate jenks breaks for a new ecoprovince calibration dataset

wesp_data <- load_wesp_data(system.file("input_data/wetFlat_20250417.csv", package = "wespr"))

# run the base scores for comparison
base_score <- calculate_jenks_score(wesp_data, out_dir = "temp", out_name = "wesp_scores_base.csv")


# view output
head(base_score)


