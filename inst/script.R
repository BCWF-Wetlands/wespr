library(readr)
library(dplyr)

# Run
#devtools::load_all()#, or in RStudio Ctrl+Shift+L to load the package
# during development, or:
# install_github("BCWF-wetlands/wespr)
# library(wespr)

# read in data and filter to questions we have implemented, and just one site:
data <- load_wesp_data(system.file("input_data/wetFlat_20240130.csv", package = "wespr"))

# gen's input line
data_test <- file.path("C:/Users/genev/OneDrive/Documents/02.Contracts/2023_BCWF_wetlands/04.Data/testing_don/wesp.csv")

data <- load_wesp_data(data_test)


#path <- data_test

#data1 <- load_wesp_data(system.file("input_data/wetFlat.csv", package = "wespr"))
sites <- unique(names(data)[grepl("site", names(data))])

sitelist <- seq(1,length(sites), 1)
sitelist <- seq(1, 4, 1)

 for(i in sitelist){
   print(i)
   site <- as.wesp_site(data, i)
   site <- calc_indicators(site)
   #aa = get_derived_values(site)
   aa = get_indicator_scores(site)
   print(aa)
 }


site <- as.wesp_site(data, 16)
#data
#site = 1

site <- as.wesp_site(data)

site <- calc_indicators(site)

site

get_indicator_scores(site)

#site <- calc_indicators(site)
# cp_f(site)
# ws_f(site)
# ws_b(site)

get_q(site, "F1_1")
get_q(site, "NeverWater")

# Next:
# - For ecosystem service calculation:
#     - filter questions list by used_by
#     - filter derived_values list by used_by
#     - subset and match weights from weights table
#     - ... magic
# - For multi-site, I think each question in the core_questions object can have
#   a list of values one for each site (rather than a questions object for each site)


ind_scores <- get_indicator_scores(site)
resp <- get_responses(site)
get_derived_values(site)


### Example to check F46 change

wesp_file <- system.file("input_data/wetFlat_20250325.csv", package = "wespr")
data150 <- readr::read_csv(wesp_file)


# run the base scores for comparison
wesp_data <- load_wesp_data(wesp_file)
base_score <- calculate_jenks_score(wesp_data, out_dir = "temp", out_name = "wesp_scores_base.csv")
base_score <- readr::read_csv("temp/wesp_scores_base.csv")


###################################################################



# import a single site and then compare against the calibration sites

# assuming the calibration scores =

library(dplyr)

# import a single site and then compare against the calibration sites
calibration_scores <- read.csv("temp/gd_jenks_breaks.csv")


# Create a single site from data and export
#wesp <- fs::path("inst/input_data/wetFlat_20250417.csv")
#wesp <- read.csv(wesp)
#wesp <- wesp[,c("Question", "X10")]
#write.csv(wesp , fs::path("inst/input_data/wetFlat_20250427_single.csv"))

# read in single site
wesp <- fs::path("inst/input_data/wetFlat_20250427_single.csv")


# option 1) # wide format
wesp_data <- load_wesp_data(wesp)
#wespRaw <- calculate_multi_site(wesp_data)

# option 2)
site <- as.wesp_site(wesp_data)
site <- calc_indicators(site)
ind_scores <- get_indicator_scores(site)


out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "GD")


#######################################################

ind_scores

calibration_scores


# check against site 1
# reform data to long format to match the calibration data (may not be needed)
ind <- ind_scores |>
  select(site, indicator, fun) |>
  mutate(service_type = "f") |>
  rename("value" = fun)

indb <- ind_scores |>
  select(site, indicator, ben) |>
  mutate(service_type = "b") |>
  rename("value" = ben)

ind <- rbind(ind, indb)


#head(ind)
#head(calibration_scores)


classed_df <- lapply(1:nrow(ind), function(i) {

  #print(i)
  #i <- 19
  trow <- ind[i,]

  # filter for the service and f/b
  calr <- calibration_scores |>
    filter(service_name  == trow$indicator) |>
    filter(service_type == trow$service_type)

  my_min <- trow$value
  my_max <- trow$value

  # is na then assign to NA
  if(is.na(my_min)){

    cal_val = NA

  }else{

  my_df_filtered <- calr |>
    dplyr::rowwise() |>
    dplyr::filter(my_min >= min & my_max <= max)

  if(nrow(my_df_filtered)== 0){
    # check if value is higher than H max or lower than min for L

    calr_h <- calr |>
      dplyr::filter(jenks  == "H") |>
      select(max) |>
      pull()

    if(my_max>calr_h){
      cal_val = "H"

    } else {
      calr_l <- calr |>
        dplyr::filter(jenks  == "L")

      cal_val = "L"
    }

     cli::cli_alert_warning("Value {round(trow$value,2)} for {trow$indicator} ({trow$service_type}) is outside the calibration range")

     } else {

  cal_val <- my_df_filtered$jenks
  }

}

    trow <- trow |>
      dplyr::mutate(calibrated_score = cal_val)

    trow
}) |> dplyr::bind_rows()







