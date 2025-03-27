
#Prepare for and install wespr
#devtools::install_github("BCWF-Wetlands/wespr")
#library(wespr)
load_all()
library(dplyr)

#read wesp data into wespr

wesp_file <- system.file("input_data/wetFlat_20250325.csv", package = "wespr")
data150 <- readr::read_csv(wesp_file)


# take a subset of the data
data150 <- data150[,1:28]
write.csv(data150, "temp/sensitivity_test.csv", row.names = FALSE)
wesp_file <- "temp/sensitivity_test.csv"


# run the base scores for comparison
wesp_data <- load_wesp_data(wesp_file)
base_score <- calculate_jenks_score(wesp_data, out_dir = "temp", out_name = "wesp_scores_base.csv")


base_score <- readr::read_csv("temp/wesp_scores_base.csv")


# read in the base data
question_metadata
indicator_weightings

# examples OF5  - numeric
# OF6 - binery
# OF7

OF6_type <- question_metadata[question_metadata$no == "OF6",]

OF6_response <- indicator_weightings[indicator_weightings$q_no == "OF6",] |>
  dplyr::select(q_no, response_no) |>
  distinct()


# take and example dataset

base_raw <- base_score |>
  select(site, any_of(ends_with("_raw")))

data150

# create a point plot per question
library(ggplot2)

ggplot(base_raw, aes(x = site, y = AM_b_raw )) +
  geom_point() +
  geom_jitter() +
  theme_minimal()












