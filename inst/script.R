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


# import a single site and then compare against the calibration sites
calibration_scores <- read.csv("temp/gd_jenks_breaks.csv")

# Create a single site from data and export
wesp <- fs::path("inst/input_data/wetFlat_20250417.csv")
wesp <- read.csv(wesp)
wesp <- wesp[,c("Question", "X10")]
write.csv(wesp , fs::path("inst/input_data/wetFlat_20250427_single.csv"))

# read in single site
wesp <- fs::path("inst/input_data/wetFlat_20250427_single.csv")

wesp_data <- load_wesp_data(wesp)

# 2)  run all sites
site <- as.wesp_site(wesp_data, 2)

site <- calc_indicators(site)
ind_scores <- get_indicator_scores(site)


site <- as.wesp_site(wesp_data, 2)
wespRaw <- calculate_multi_site(site)

#######################################################
# up to here


# run multi-site analysis
wespRaw <- calculate_multi_site(site)
#wespRaw <- calculate_multi_site(wesp_data, sites = NULL)

# check out dir exists and if not create it
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# Calculate Jenks breaks and add to data.frame

# 1) First normalize the service and add to data.frame

min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# apply Min-Max normalization
wespNorm <- as.data.frame(lapply(wespRaw[, -1], min_max_norm)) %>%
  dplyr::mutate(site = as.numeric(rownames(.)), .before = 1)
wespNorm <- dplyr::rename_with(wespNorm, ~ gsub("_raw", "_norm", .x, fixed = TRUE))


#format to long form

norm_long <- tidyr::pivot_longer(wespNorm, -.data$site, names_to = "metric", values_to = "value") |>
  dplyr::mutate(type = "normalised") |>
  dplyr::mutate(name = gsub( "_norm", "", .data$metric))

norm_raw <- tidyr::pivot_longer( wespRaw, -.data$site, names_to = "metric", values_to = "value") |>
  dplyr::mutate(type = "raw")|>
  dplyr::mutate(name = gsub( "_raw", "", .data$metric))

all <- rbind(norm_long, norm_raw)

#   ggplot2::ggplot(norm_raw, ggplot2::aes(.data$value, fill = .data$type)) +
#     ggplot2::geom_density(alpha = 0.2) +
#     ggplot2::facet_wrap(~.data$name, scales = "free") +
#     ggplot2::theme_bw()


# 2) Calculate Jenks brakes
wesp_breaks_raw <- purrr::map(names(wespNorm)[-1], function(x) {
  # x <- names(wespNorm)[-1][16]
  if (all(is.na(wespNorm[[x]]) == TRUE)) {
    cli::cli_alert_warning("skipping calculation of jenks breask for {x} as all values are NA")
    return(NA)
  } else {
    jen_breaks <- BAMMtools::getJenksBreaks(wespNorm[[x]], 4, subset = NULL)
    .bincode(wespNorm[[x]], sort(jen_breaks), include.lowest = TRUE)
  }
})

names(wesp_breaks_raw) <- names(wespNorm)[-1]

# Change numeric to character High, Medium, Low
wesp_breaks_cat <- lapply(wesp_breaks_raw[1:length(wesp_breaks_raw)], function(x) {
  dplyr::case_when(
    x == 1 ~ "L",
    x == 2 ~ "M",
    x == 3 ~ "H"
  )
})

# Change list to data frame
wespBreaks <- as.data.frame(do.call(cbind, wesp_breaks_cat))
wespBreaks <- wespBreaks |>
  dplyr::mutate(site = as.numeric(rownames(wespBreaks)), .before = 1)
wespBreaks <- dplyr::rename_with(wespBreaks, ~ gsub("_norm", "_jenks", .x, fixed = TRUE))

# Make a single data frame that includes the raw, normalized and Jenks values
wespEcoS <- list(wespRaw, wespNorm, wespBreaks) %>%
  purrr::reduce(dplyr::full_join, by = "site") %>%
  dplyr::select(.data$site, sort(names(.)))

# wespEcoS <-data.frame(Wetland_Co=wetLUT,wespEcoS.1)

# #   # generate a histograph per metrics
#    library(ggplot2)
#    library(tidyr)
#    ggplot(gather(wespEcoS), aes(value)) +
#     geom_histogram(bins = 10) +
#      facet_wrap(~key, scales = 'free_x')
#
#    #wespNorm %>% gather() %>% head()


# Write out the data frame
utils::write.csv(wespEcoS, fs::path(out_dir, out_name), row.names = FALSE)
cli::cli_alert_success("WESP scores calculated and saved to {fs::path(out_dir, out_name)}")

return(wespEcoS)

}







