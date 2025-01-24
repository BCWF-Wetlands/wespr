
#Prepare for and install wespr
#devtools::install_github("BCWF-Wetlands/wespr")
library(wespr)
load_all()

#read wesp data into wespr
data_test <- file.path("C:/Users/genev/OneDrive/Documents/02.Contracts/2023_BCWF_wetlands/04.Data/testing_don/wesp.csv")

wesp_data <- load_wesp_data(data_test)

# 2)  run all sites

calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_test.csv")


#Write out the data frame
write.csv(wespEcoS, fs::path(out_dir, out_name),row.names=FALSE)


#wespEcoS.csv

