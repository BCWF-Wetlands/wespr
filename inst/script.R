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



## Update the raw survey123 data to fix issue with the

#For the SpeciesPres1-11, SpeciesPres4 (one or small mammals of conservation concern (such as muskrat, rodents etc).
# Was added at the end of 2023, should have been added as SpeciesPres11, but
# was added as SpeciesPres4. This shifted SpeciesPres4-10 to be +1 (eg was 4-10, now 5-11) in the 2024 data.

#In 2024 - SpeciesPres4 to be-11 in the 2024 data.
# Speciespres5-11 should be 4-10




field_data <- system.file("extdata/field_survey123_edited.xls", package = "wespr")

indata <- readxl::read_xls(field_data,
                           col_names = TRUE, sheet = 1,
                           col_types = c(rep("text", 2), "date", rep("text", 117))
)


indata <- indata |>
  #select(objectid, globalid, datetime, F58_0)|>
  mutate(year = year(datetime))


indata23 <- indata |>
  filter(year <2024) |>
  mutate(F58_0_fix = F58_0)


indata24 <- indata |>
  filter(year == 2024) |>
  mutate(F58_0_fix = str_replace_all(F58_0, "5", "4"),
         F58_0_fix = str_replace_all(F58_0_fix , "6", "5"),
         F58_0_fix = str_replace_all(F58_0_fix , "7", "6"),
         F58_0_fix = str_replace_all(F58_0_fix , "8", "7"),
         F58_0_fix = str_replace_all(F58_0_fix , "9", "8"),
         F58_0_fix = str_replace_all(F58_0_fix , "10", "9"),
         F58_0_fix = str_replace_all(F58_0_fix , "11", "10"))

out <- bind_rows(indata23, indata24)


xx <- merge(indata, out, by = c(colnames(indata)))

openxlsx::write.xlsx(xx, fs::path("temp",  "field_survey123_edited_fix.xls"),
                     overwrite = overwrite, rowNames = FALSE, colNames = TRUE
)
