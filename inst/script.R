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

