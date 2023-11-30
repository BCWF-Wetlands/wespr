library(googledrive)
library(googlesheets4)
library(dplyr)
library(readr)
library(janitor)

xl <- drive_find("WSP_calculations.xlsx")

tmp_xl_gs <- drive_cp(xl, name = "tmp-wsp-calcs", mime_type = drive_mime_type("spreadsheet"))

all_indicators <- read_sheet(tmp_xl_gs, sheet = "all_indicators", col_types = "c",
                             .name_repair = make_clean_names) |>
  filter(!is.na(no)) |>
  select(-starts_with("x"))

weightings <- read_sheet(tmp_xl_gs, sheet = "weightings",
                         .name_repair = make_clean_names) |>
  select(-starts_with("x"))

drive_rm(tmp_xl_gs)

write_csv(all_indicators, "inst/input_data/all_indicators.csv")
write_csv(weightings, "inst/input_data/weightings.csv")

#
# usethis::use_data(wsp_calculations, overwrite = TRUE)
