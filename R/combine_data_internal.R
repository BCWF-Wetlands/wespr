## processing Field data - internal function

processing_fielddata <- function(indata = indata) {

   fdata <- indata |>
    dplyr::select(.data$Wetland_Co, dplyr::starts_with("F")) |>
    dplyr::distinct(.data$Wetland_Co, .keep_all = TRUE)

  # make a list of id numbers.
  WForm_Wetland_Co <- dplyr::select(fdata, .data$Wetland_Co)

  # Case 1 :  Split F2 into c(F2_A1, F2_A2, F2_B1, F2_B2)

  fdata <- fdata %>% dplyr::mutate(
    F2_A1 = dplyr::case_when(
      .data$F2_0 == "A1" ~ 1,
      TRUE ~ 0
    ),
    F2_A2 = dplyr::case_when(
      .data$F2_0 == "A2" ~ 1,
      TRUE ~ 0
    ),
    F2_B1 = dplyr::case_when(
      .data$F2_0 == "B1" ~ 1,
      TRUE ~ 0
    ),
    F2_B2 = dplyr::case_when(
      F2_0 == "B2" ~ 1,
      TRUE ~ 0
    )
  )

  fdata <- fdata |>
    dplyr::select(-c("F2_0")) |>
    dplyr::mutate(dplyr::across(c(.data$F22_0, .data$F23_0, .data$F42_0, .data$F49_0), ~ stringr::str_replace(., "N/A", "0")))

  # updated the ph columns

  phdf <- fdata |>
    dplyr::select(.data$Wetland_Co, .data$F46_0, .data$F46_1) |>
    dplyr::mutate(F46_10 = ifelse(.data$F46_0 == "Cond", .data$F46_1, NA)) |>
    dplyr::mutate(F46_20 = ifelse(.data$F46_0 == "TDS", .data$F46_1, NA)) |>
    dplyr::select(-c(.data$F46_0, .data$F46_1)) |>
    dplyr::rename(
      "F46_1" = .data$F46_10,
      "F46_2" = .data$F46_20
    )

  fdata <- fdata |>
    dplyr::select(-c(.data$F46_0, .data$F46_1)) |>
    dplyr::left_join(phdf, by = "Wetland_Co")


  # Case 2 :  Make list of variables that require parsing

  # update #F58 response 11 is merged with response 8, They are seperated in survey123 data,
  # combined for calculations so will be merged here

  fdataX <- fdata |>
    dplyr::mutate(F58_0 = stringr::str_replace_all(.data$F58_0, "11", "8"))

  # TODO;note this might be at risk if there are two incidents of SpeciesPres8 but not sure in this dataset
  # requires more testing


  ParseVars <- c("F3_0", "F56_0", "F57_0", "F58_0")

  # Number of sub-categories for each variable
  NparseVars <- c(8, 3, 7, 10)

  # Function to split a Form variable that has multiple entries into separate variables
  SplitFn1 <- function(i, df) {
    purrr::map_dfc(1:NparseVars[i], function(j) {
      FormVName <- sub("_0", paste0("_", j), ParseVars[i])
      df %>%
        dplyr::transmute(!!FormVName := dplyr::if_else(j %in% VpartsN, 1, 0))
    })
  }

  # Loop through each Variable to split out and call the function
  # that splits it into separate variables
  df3 <- purrr::map(1:length(ParseVars), function(x) {
    df1 <- fdata %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Vparts = stringr::str_split(!!rlang::sym(ParseVars[x]), ",")) %>%
      dplyr::mutate(VpartsN = list(readr::parse_number(Vparts))) %>%
      dplyr::select(dplyr::all_of(ParseVars[x]), Vparts, VpartsN)
    SplitFn1(x, df1)
  })

  # Combine generated form sub-variables with original data.frame
  fdata2 <- dplyr::bind_cols(fdata, dplyr::bind_cols(df3)) |>
    dplyr::mutate(dplyr::across(c(.data$F22_0, .data$F23_0, .data$F42_0, .data$F49_0), ~ stringr::str_replace(., "N/A", "0")))

  # rename columns in F3 to match survey123

  fdata2 <- fdata2 |>
    dplyr::rename(
      F3_20 = .data$F3_2,
      F3_30 = .data$F3_3,
      F3_40 = .data$F3_4,
      F3_50 = .data$F3_5,
      F3_60 = .data$F3_6,
      F3_70 = .data$F3_7,
    ) |>
    dplyr::rename(
      F3_2 = .data$F3_50,
      F3_3 = .data$F3_20,
      F3_4 = .data$F3_60,
      F3_5 = .data$F3_30,
      F3_6 = .data$F3_70,
      F3_7 = .data$F3_40
    )


  # Case 3
  # Split out form binary variables that are contained in 1 variable
  ParseVars <- c(
    "F4_0", "F5_0", "F6_0", "F7_0", "F8_0", "F9_0", "F10_0", "F11_0", "F12_0", "F13_0",
    "F14_0", "F15_0", "F16_0", "F17_0", "F18_0", "F19_0", "F20_0", "F21_0", "F24_0", "F25_0",
    "F26_0", "F27_0", "F28_0", "F29_0", "F30_0", "F31_0", "F32_0", "F33_0", "F34_0", "F35_0",
    "F36_0", "F37_0", "F38_0", "F39_0", "F40_0", "F41_0", "F43_0", "F44_0", "F45_0", "F47_0", "F48_0",
    "F50_0", "F51_0", "F52_0", "F53_0", "F54_0", "F55_0", "F59_0"
  )
  # Number of sub-categories for each variable
  NparseVars <- c(
    4, 5, 5, 5, 3, 4, 5, 5, 4, 5,
    5, 5, 5, 5, 3, 6, 5, 6, 5, 5,
    5, 6, 3, 6, 6, 6, 6, 6, 6, 4,
    5, 6, 3, 4, 5, 4, 5, 4, 3, 3, 3,
    5, 3, 4, 6, 6, 7, 5
  )

  df4 <- lapply(1:length(ParseVars), function(x) {
    df1 <- fdata2 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(VpartsN = readr::parse_number(!!rlang::sym(ParseVars[x]))) %>%
      dplyr::select(ParseVars[x], VpartsN)
    SplitFn1(x, df1)
  })

  WForm3.1 <- cbind(WForm_Wetland_Co, do.call(cbind, df4))
  WForm3 <- dplyr::mutate(fdata2, WForm3.1)


  # drop the shrub_1 and rename values to F4_1 to F4_3
  WForm3 <- WForm3 |>
    dplyr::select(-c(.data$F4_1)) |>
    dplyr::rename(
      F4_1 = .data$F4_2,
      F4_2 = .data$F4_3,
      F4_3 = .data$F4_4
    )

  # Case 4
  # Modify y/n to 1/0 and set
  WForm4.1 <- WForm3 |>
    dplyr::mutate(dplyr::across(c(.data$F22_0, .data$F23_0, .data$F42_0, .data$F49_0), ~ dplyr::case_when(. == "yes" ~ "1", TRUE ~ "0"))) # |>
  # dplyr::mutate(
  #    F2_A0 = 0,
  #    F2_B0 = 0)


  # Special default cases, default to a specific (not 1) case
  specialCaseMissing <- c("F27", "F33", "F35", "F38", "F39", "F40", "F53", "F55")
  specialCaseMissingValues <- c(6, 6, 4, 3, 4, 5, 6, 7)

  # Fill in default or 0 cases - survey 123 data passing 0 instead of populated field
  caseMissing <- c(
    "F12", "F13", "F16", "F18", "F20", "F21", "F24", "F25", "F26", "F28", "F29", "F30", "F31", "F32", "F34",
    "F36", "F37", "F44", "F41", "F43", "F50", "F52", "F59"
  )
  caseMissingValues <- sapply(1:length(caseMissing), function(j) 1)

  MissCase <- c(caseMissing, specialCaseMissing)
  MissValue <- c(caseMissingValues, specialCaseMissingValues)

  df5 <- lapply(1:length(MissCase), function(x) {
    VName0 <- paste0(MissCase[x], "_0")
    VNameN <- paste0(MissCase[x], "_", MissValue[[x]])
    WForm4.1 %>%
      dplyr::mutate(!!VNameN := dplyr::if_else(!!rlang::sym(VName0) == "0", "1", as.character(!!rlang::sym(VNameN)))) %>%
      dplyr::select(!!rlang::sym(VName0), !!rlang::sym(VNameN))
  })

  df6 <- cbind(WForm_Wetland_Co, do.call(cbind, df5)) %>%
    dplyr::select(-c(paste0(MissCase, "_0")))

  WForm4 <- WForm4.1 %>%
    dplyr::select(-c(paste0(c(MissCase), "_", MissValue))) %>%
    dplyr::left_join(df6, by = "Wetland_Co")


  # remove F58 Alpha cols as these are descriptive and not used in calculations
  # convert F22_0, F23_0, F42_0, to 1 and 0 and added _1

  WForm4 <- WForm4 |>
    dplyr::select(-c("F58_A", "F58_B", "F58_C", "F58_D", "F58_E", "F58_F")) |>
    dplyr::mutate(F22_1 = dplyr::case_when(
      .data$F22_0 == "1" ~ 1,
      TRUE ~ 0
    )) |>
    dplyr::mutate(F23_1 = dplyr::case_when(
      .data$F23_0 == "1" ~ 1,
      TRUE ~ 0
    )) |>
    dplyr::mutate(F42_1 = dplyr::case_when(
      .data$F42_0 == "1" ~ 1,
      TRUE ~ 0
    )) |>
    dplyr::mutate(F49_1 = dplyr::case_when(
      .data$F49_0 == "1" ~ 1,
      TRUE ~ 0
    ))

  # currently dropping the F57_7 as this consume7 and is not applicable response.
  WForm4 <- WForm4 |>
    dplyr::select(-.data$F57_7) |>
    dplyr::select(-.data$F1)

  return(WForm4)
}







## processing Stressor data - internal function

processing_stressordata <- function(indata = indata) {

   WFormS <- indata |>
    dplyr::select(.data$Wetland_Co, dplyr::starts_with("S")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::distinct(.data$Wetland_Co, .keep_all = TRUE) |>
    # need to relabel S6_3 to S6_5 and S6_4 to S6_6
    dplyr::rename(
      S6_5 = .data$S6_3,
      S6_6 = .data$S6_4
    )

  WForm_Wetland_Co <- WFormS |> dplyr::select(.data$Wetland_Co)

  # Make list of variables that require parsing - Careful if changing these as it impacts the flow on question numbers
  ParseVars <- c("S1", "S2", "S3", "S4", "S5", "S6")
  NparseVars <- c(10, 5, 9, 8, 8, 4)


  # Function to split a Form variable that has multiple entries into separate variables
  SplitFn2 <- function(i, df) {
    df2 <- lapply(1:NparseVars[i], function(j) {
      FormVName <- paste0(ParseVars[i], "_", j)
      df %>%
        dplyr::mutate(!!FormVName := dplyr::case_when(
          is.element(j, VpartsN) ~ 1,
          TRUE ~ 0
        )) %>%
        dplyr::select(!!rlang::sym(FormVName))
    })
    do.call(cbind, df2)
  }

  # Loop through each Variable to split out and call the function
  # that splits it into separate variables
  df3 <- lapply(1:length(ParseVars), function(x) {
    df1 <- WFormS %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Vparts = (strsplit(!!rlang::sym(ParseVars[x]), ","))) %>%
      dplyr::mutate(VpartsN = list(readr::parse_number(Vparts))) %>%
      dplyr::select((ParseVars[x]), Vparts, VpartsN)
    SplitFn2(x, df1)
  })

  # Combine generated form sub-variables with original data.frame
  WFormS2.1 <- cbind(WForm_Wetland_Co, do.call(cbind, df3))
  df3 <- NA
  WFormS2 <- dplyr::mutate(WFormS, WFormS2.1) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # Split out form binary variables that are contained in 1 variable
  ParseVars <- c(
    "S1_11", "S1_12", "S1_13", "S1_14", "S2_6", "S2_7", "S2_8", "S3_10", "S3_11", "S3_12",
    "S4_9", "S4_10", "S4_11", "S4_12", "S5_9", "S5_10", "S5_11", "S5_12", "S6_5", "S6_6"
  )

  WFormS3 <- WFormS2 |>
    dplyr::mutate(dplyr::across(tidyr::all_of(ParseVars), readr::parse_number)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))


  # calculate subscores for the stressors

  # s1_15 = S1_11 + S1_12 + S1_13 + S1_14
  # s1_16 = S1_15/12
  # S2_9 = S2_6 + S2_7 + S2_8
  # S2_10 = S2_9/9
  # S3_13 = S3_10 + S3_11 + S3_12
  # S3_14 = S3_13/9
  # S4_13 = S4_9 + S4_10 + S4_11 + S4_12
  # S4_14 = S4_13/12
  # S5_13 = S5_9 + S5_10 + S5_11 + S5_12
  # S5_14 = S5_13/12
  # S6_7 = S6_5 + S6_6
  # S6_8 = S6_7/6

  WFormS3[is.na(WFormS3)] <- "0"

  WFormS4 <- WFormS3 |>
    dplyr::mutate(
      S1_15 = as.numeric(.data$S1_11) + as.numeric(.data$S1_12) + as.numeric(.data$S1_13) + as.numeric(.data$S1_14),
      S1_16 = round(.data$S1_15 / 12, 2),
      S2_9 = as.numeric(.data$S2_6) + as.numeric(.data$S2_7) + as.numeric(.data$S2_8),
      S2_10 = round(.data$S2_9 / 9, 2),
      S3_13 = as.numeric(.data$S3_10) + as.numeric(.data$S3_11) + as.numeric(.data$S3_12),
      S3_14 = round(.data$S3_13 / 9, 2),
      S4_13 = as.numeric(.data$S4_9) + as.numeric(.data$S4_10) + as.numeric(.data$S4_11) + as.numeric(.data$S4_12),
      S4_14 = round(.data$S4_13 / 12, 2),
      S5_13 = as.numeric(.data$S5_9) + as.numeric(.data$S5_10) + as.numeric(.data$S5_11) + as.numeric(.data$S5_12),
      S5_14 = round(.data$S5_13 / 12, 2),
      S6_7 = as.numeric(.data$S6_5) + as.numeric(.data$S6_6),
      S6_8 = round(.data$S6_7 / 6, 2)
    )

  WFormS4 <- WFormS4 |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(
      .data$Wetland_Co,
      dplyr::starts_with("S1_"), dplyr::starts_with("S2_"),
      dplyr::starts_with("S3_"), dplyr::starts_with("S4_"),
      dplyr::starts_with("S5_"), dplyr::starts_with("S6_")
    )


  return(WFormS4)
}



# ##########################################################
## processing officedata - internal function

processing_officedata <- function(indata = indata) {

# testing line
 #indata <- ofdata

  on <- indata |>
    dplyr::rename(
      "objectid" = .data$ObjectID,
      "globalid" = .data$GlobalID,
       "Wetland_Co" = .data$`Site ID (should match Field Assessment Form)`,
      #"Wetland_Co" = .data$`Preassigned wetland ID number (region initials followed by a number ie SI_1234)`,
      "Assessors" = .data$`Name of Assessor`,
      "OF1_0" = .data$`OF1. Distance to Community`,
      "OF2_0" = .data$`OF 2. Distance to Frequently Traveled Road`,
      "OF3_0" = .data$`OF 3. Distance to Ponded Water`,
      "OF4_0" = .data$`OF 4. Distance to Lakes`,
      "OF5_1" = .data$`OF 5. Relative Elevations in Watershed`,
      "OF6_1" = .data$`OF 6. Stream Intersect`,
      "OF7_0" = .data$`OF 7. Aspect`,
      "OF8_0" = .data$`OF 8. Glacier Influence`,
      "OF9_0" = .data$`OF 9. Floodable Infrastructure`,
      "OF10_0" = .data$`OF 10. Internal Flow Distance`,
      "OF11_0" = .data$`OF 11. Wetland as a % of its Contributing Area (Catchment)`,
      "OF12_0" = .data$`OF 12. Unvegetated Surface in the Wetland's WAU`,
      "OF13_1" = .data$`OF 13. Conservation Investment`,
      "OF14_1" = .data$`OF 14. Sustained Scientific Use`,
      "OF15_1" = .data$`OF 15. Burned`,
      "OF16_1" = .data$`OF 16. Karst Geology`,
      "OF17_1" = .data$`OF 17. Geologic Faults`,
      "OF18_0" = .data$`OF 18. Lakes within 2 km`,
      "OF19_0" = .data$`OF 19. Wetlands and Lakes within 2 km`,
      "OF20_0" = .data$`OF 20. Fish Occurrence`,
      "OF21_1" = .data$`OF 21. Ecological Designation`,
      "OF22_1" = .data$`OF 22. Protection from Intensive Uses`,
      "OF23_0" = .data$`OF 23. BGC Protection Percentage`,
      "OF24_0" = .data$`OF 24. Select all the species of conservation concern (in the list below) that have been observed within a 500m buffer of the AA?`,
      "x11" = .data$`If any species of conservation were observed, list them in the text fields below.`,
      "x1" = .data$`Any plant species or community of conservation concern`,
      "x2" = .data$`Any amphibian listed as being of conservation concern`,
      "x3" = .data$`Either of these waterbird species of conservation concern: American Bittern, Eared Grebe`,
      "x4" = .data$`Raptor or wetland songbird species of conservation concern: Broad-winged Hawk, Swainson's Hawk, Northern Goshawk, Peregrine Falcon, Prairie Falcon, Long-billed Curlew, Western Screech-owl, Short-eared Owl, Black Swift, Olive-sided Flycatcher, Barn Swallow, Cape May Warbler, and/or Rusty Blackbird`,
      "x5" = .data$`Caribou`,
      "OF25_1" = .data$`OF 25. Local Moisture Deficit`,
      "OF26_1" = .data$`OF 26. Degree Days Index`,
      "OF27_1" = .data$`OF 27. Local Solar Input`,
      "OF28_0" = .data$`OF 28. Site Index (Soil Nutrients)`,
      "OF29_1" = .data$`OF 29. Topographic Position`,
      "OF30_0" = .data$`OF 30. Road Density within AA's Buffer`,
      "OF31_0" = .data$`OF 31. Road Density within 2 km of the AA`,
      "OF32_0" = .data$`OF 32. Intactness of Landscape within 2 km`,
      "OF33_0" = .data$`OF 33. Mature & Old Growth Forest within 2 km`,
      "OF34_0" = .data$`OF 34. Land Cover Type Uniqueness`,
      "OF35_1" = .data$`OF 35. Maximum Dominance of a Land Cover Type`,
      "OF36_0" = .data$`OF 36. Number of Land Cover Types in the AA and 100 m Buffer`,
      "OF37_0" = .data$`OF 37. Number of Land Cover Types within 2 km`,
      "OF38_0" = .data$`OF 38. Deciduous Land Cover within the AA and 100 m Buffer`,
      "OF39_0" = .data$`OF 39. Closed Coniferous Land Cover within the AA and 100 m Buffer`,
      "OF40_0" = .data$`OF 40. Non-tree Vegetation within the AA and 100 m Buffer`,
      "OF41_0" = .data$`OF 41. Disturbed Area Percentage in the WAU`,
      "OF42_0" = .data$`OF 42. Road Density in the WAU`,
      "OF43_0" = .data$`OF 43. Wetland Density in the WAU`,
      "OF44_0" = .data$`OF 44. Ecoprovince`,
      "CreationDate" = .data$`CreationDate`,
      "Creator" = .data$`Creator`,
      "EditDate" = .data$`EditDate`,
      "Editor" = .data$`Editor`,
      "x" = .data$`x`,
      "y" = .data$`y`
    )

  odata <- on |>
    dplyr::select(.data$Wetland_Co, dplyr::starts_with("O")) |>
    dplyr::distinct(.data$Wetland_Co, .keep_all = TRUE)

  OF_Wetland_Co <- odata %>%
    dplyr::select(.data$Wetland_Co)

  # Case 1: Only a single response from multiple choices
  ParseVars <- c(
    "OF1_0", "OF2_0", "OF3_0", "OF4_0", "OF7_0", "OF8_0", "OF9_0", "OF10_0",
    "OF11_0", "OF12_0", "OF18_0", "OF19_0", "OF23_0", "OF28_0", "OF30_0", "OF31_0",
    "OF32_0", "OF33_0", "OF34_0", "OF36_0", "OF37_0", "OF38_0", "OF39_0", "OF40_0",
    "OF41_0", "OF42_0", "OF43_0", "OF44_0"
  )

  # Number of sub-categories for each variable -

  #TODO: Need to update the OF44 values to 9 options

  NparseVars <- c(
    5, 6, 7, 6, 3, 3, 4, 6,
    4, 3, 5, 5, 6, 5, 3, 3,
    5, 5, 3, 4, 5, 5, 5, 5,
    5, 3, 5, 5
  )

  df1 <- odata %>%
    dplyr::select(c(.data$Wetland_Co, tidyr::all_of(ParseVars))) |>
    dplyr::mutate(dplyr::across(tidyr::all_of(ParseVars), ~ stringr::str_split_i(.x, "_", 2)))

  SplitFn1 <- function(i, df) {
    df2 <- lapply(1:NparseVars[i], function(j) {
      FormVName <- sub("_0", paste0("_", j), ParseVars[i])
      df %>%
        dplyr::mutate(!!FormVName := dplyr::if_else(!!rlang::sym(ParseVars[i]) == j, 1, 0)) %>%
        dplyr::select(!!rlang::sym(FormVName))
    })
    do.call(cbind, df2)
  }

  # Loop through each Variable to split out and call the function that splits it into separate variables
  df3 <- lapply(1:length(ParseVars), function(x) {
    df2 <- df1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(VpartsN = NparseVars[x]) %>%
      dplyr::select((ParseVars[x]), VpartsN)
    SplitFn1(x, df2)
  })

  # Combine generated form sub-variables with original data.frame
  OF_manual.1 <- cbind(OF_Wetland_Co, do.call(cbind, df3))

  # update NAs to 0 if all values missing
  OF_manual.1 <- OF_manual.1 |>
    tidyr::replace_na(list(
      OF38_1 = 0, OF38_2 = 0, OF38_3 = 0, OF38_4 = 0, OF38_5 = 0,
      OF41_1 = 0, OF41_2 = 0, OF41_3 = 0, OF41_4 = 0, OF41_5 = 0
    ))

  # Case 2: Numeric values - leave as is.
  num_cols <- c("OF5_1", "OF25_1", "OF26_1", "OF27_1", "OF29_1", "OF35_1")

  od2 <- odata %>%
    dplyr::select(c(.data$Wetland_Co, tidyr::all_of(num_cols))) |>
    dplyr::mutate(dplyr::across(tidyr::all_of(num_cols), ~ gsub("OF29_", "", .x)))

  OF_manual.1 <- dplyr::left_join(OF_manual.1, od2, by = "Wetland_Co")

  # Case 3: Numeric values - leave as is.

  # binary (0/1)
  binvars <- c("OF6_1", "OF13_1", "OF14_1", "OF15_1", "OF16_1", "OF17_1", "OF21_1", "OF22_1")

  od3 <- odata %>%
    dplyr::select(c(.data$Wetland_Co, tidyr::all_of(binvars))) |>
    dplyr::mutate(dplyr::across(tidyr::all_of(binvars), ~ stringr::str_split_i(.x, "_", 2)))

  OF_manual.1 <- dplyr::left_join(OF_manual.1, od3, by = "Wetland_Co")

  #################################################################################

  # case4: multi responses - still to fix this one ###########################

  # TODO: update these once OF20CK_ changed to OF+


  multiresp <- c("OF24_0")
  multirespN <- c(5)
  od4 <- odata %>%
    dplyr::select(c(.data$Wetland_Co, tidyr::all_of(multiresp))) |>
    dplyr::mutate(dplyr::across(tidyr::all_of(multiresp), ~ gsub("OF24_", "", .x)))

  ParseVars <- c("OF24_0")
  NparseVars <- c(5)

  # Function to split a Form variable that has multiple entries into separate variables
  SplitFn1 <- function(i, df) {
    purrr::map_dfc(1:NparseVars[i], function(j) {
      FormVName <- sub("_0", paste0("_", j), ParseVars[i])
      df %>%
        dplyr::transmute(!!FormVName := dplyr::if_else(j %in% VpartsN, 1, 0))
    })
  }

  # Loop through each Variable to split out and call the function
  # that splits it into separate variables
  df3 <- purrr::map(1:length(ParseVars), function(x) {
    df1 <- od4 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Vparts = stringr::str_split(!!rlang::sym(ParseVars[x]), ",")) %>%
      dplyr::mutate(VpartsN = list(readr::parse_number(Vparts))) %>%
      dplyr::select(tidyr::all_of(ParseVars[x]), Vparts, VpartsN)
    SplitFn1(x, df1)
  })

  # WForm3.1 <- cbind(WForm_Wetland_Co, do.call(cbind, df4))
  OF_manual.2 <- cbind(OF_Wetland_Co, do.call(cbind, df3))
  OF_manual.2 <- dplyr::left_join(OF_manual.1, OF_manual.2, by = "Wetland_Co")


  # case 5: OF20 adjustments - this is a special case

  od5 <- odata %>%
    dplyr::select(c(.data$Wetland_Co, "OF20_0")) #|>
   # mutate(across(all_of(multiresp), ~ gsub("OF20", "", .x)))

  ## IF multiple sites this needs to be updated...# do this per row
  fish_values <- od5$OF20_0

  od5 <- od5 |>
    dplyr::mutate(OF20_1 = dplyr::case_when(
      stringr::str_detect(fish_values, "CK3") == TRUE ~ 3,
      stringr::str_detect(fish_values, "CK2") == TRUE ~ 2,
      stringr::str_detect(fish_values, "CK1") == TRUE ~ 1,
      .default = 0
    )) |>
    dplyr::mutate(OF20_2 = dplyr::case_when(
      stringr::str_detect(fish_values, "CM3") == TRUE ~ 3,
      stringr::str_detect(fish_values, "CM2") == TRUE ~ 2,
      stringr::str_detect(fish_values, "CM1") == TRUE ~ 1,
      .default = 0
    )) |>
    dplyr::mutate(OF20_3 = dplyr::case_when(
      stringr::str_detect(fish_values, "CO3") == TRUE ~ 3,
      stringr::str_detect(fish_values, "CO2") == TRUE ~ 2,
      stringr::str_detect(fish_values, "CO1") == TRUE ~ 1,
      .default = 0
    )) |>
    dplyr::mutate(OF20_4 = dplyr::case_when(
      stringr::str_detect(fish_values, "FH") == TRUE ~ 1,
      .default = 0
    )) |>
    dplyr::mutate(OF20_5 = dplyr::case_when(
      stringr::str_detect(fish_values, "FS") ~ 1,
      .default = 0
    ))


  of20 <- od5 |>
    dplyr::select(.data$Wetland_Co, .data$OF20_1, .data$OF20_2, .data$OF20_3, .data$OF20_4, .data$OF20_5)

  OF_manual.2 <- dplyr::left_join(OF_manual.2, of20, by = "Wetland_Co")

  cli::cli_alert_success("Desktop data sucessfully processed")

  return(OF_manual.2)
}



# this portion of the function is not needed as it is incorpotated into Don's workflow
# not sure where. For now we will simply imput the office outputs, which contains both
#
#
#
#
#
# cli::cli_alert("Processing manual office data")
#
#
# WetPlotMOFDataIn <- read_xls(manual_office_data,
#   sheet = 1, col_names = TRUE) |>
#   dplyr::rename(Wetland_Co = Wtlnd_C)
#
# OF_manual <- WetPlotMOFDataIn |>
#   dplyr::rename(OF6_1 = Stream_Intersect) |>
#   dplyr::rename(OF8_0 = GlacialInfluence) |>  # 3
#   dplyr::rename(OF9_0 = Flood_Infastructure) |> # 4
#   dplyr::rename(OF10_0 = Internal_Flow_dist) |>
#   dplyr::mutate(OF10_1 = if_else(OF10_0 > 0 & OF10_0 < 10, 1, 0)) |>
#   dplyr::mutate(OF10_2 = if_else(OF10_0 >= 10 & OF10_0 < 50, 1, 0)) |>
#   dplyr::mutate(OF10_3 = if_else(OF10_0 >= 50 & OF10_0 < 100, 1, 0)) |>
#   dplyr::mutate(OF10_4 = if_else(OF10_0 >= 100 & OF10_0 < 1000, 1, 0)) |>
#   dplyr::mutate(OF10_5 = if_else(OF10_0 >= 1000 & OF10_0 < 2000, 1, 0)) |>
#   dplyr::mutate(OF10_6 = if_else(OF10_0 >= 2000 | OF10_0 == 0, 1, 0)) |>
#   dplyr::rename(OF11_0 = Percent_of_catchament) |>
#   dplyr::mutate(OF11_1 = if_else(OF11_0 < 0.01, 1, 0)) |>
#   dplyr::mutate(OF11_2 = if_else(OF11_0 >= 0.01 & OF11_0 < 0.1, 1, 0)) |>
#   dplyr::mutate(OF11_3 = if_else(OF11_0 >= 0.1 & OF11_0 < 1, 1, 0)) |>
#   dplyr::mutate(OF11_4 = if_else(OF11_0 >= 1, 1, 0)) |>
#   dplyr::mutate(OF13_1 = if_else(ConservationInvestment == "0", 0, 1)) |>
#   dplyr::rename(OF14_1 = Sustained_Sci_Use) |>
#   dplyr::mutate(OF24_1 = 0,
#          OF24_2 = 0,
#          OF24_3 = 0,
#          OF24_4 = 0,
#          OF44_1 = 1,  # why is this one? This isi the ecoprovince question - gd = 1?
#          OF44_2 = 0,
#          OF44_3 = 0,
#          OF44_4 = 0,
#          OF44_5 = 0) |>
#   dplyr::select(
#     Wetland_Co, OF6_1, OF8_0, OF9_0, OF10_1, OF10_2, OF10_3, OF10_4, OF10_5, OF10_6,
#     OF11_1, OF11_2, OF11_3, OF11_4, OF13_1, OF14_1, OF24_1, OF24_2, OF24_3, OF24_4,
#     OF44_1, OF44_2, OF44_3, OF44_4, OF44_5
#   ) #%>%
#   #dplyr::select(WTLND_ID, (contains("OF")))
#
# OF_manual_Wetland_Co <- OF_manual |>  dplyr::select(Wetland_Co)
#
# # Make list of manual variables that require parsing
# ParseVars <- c("OF8_0", "OF9_0")
# # Number of sub-categories for each variable
# NparseVars <- c(3, 4)
# # drop geometry
#
# SplitFn1 <- function(i, df) {
#   df2 <- lapply(1:NparseVars[i], function(j) {
#     # FormVName<-paste0(ParseVars[i],"_",j)
#     FormVName <- sub("_0", paste0("_", j), ParseVars[i])
#     df |>
#       mutate(!!FormVName := if_else(!!rlang::sym(ParseVars[i]) == j, 1, 0)) |>
#       dplyr::select(!!rlang::sym(FormVName))
#   })
#   do.call(cbind, df2)
# }
#
# # Loop through each Variable to split out and call the function
# # that splits it into separate variables
# df3 <- lapply(1:length(ParseVars), function(x) {
#   df1 <- OF_manual |>
#     rowwise() |>
#     mutate(VpartsN = NparseVars[x]) |>
#     dplyr::select((ParseVars[x]), VpartsN)
#   SplitFn1(x, df1)
# })
#
# # Combine generated form sub-variables with original data.frame
# OF_manual.1 <- cbind(OF_manual_Wetland_Co, do.call(cbind, df3))
# wesp_M_OF <- merge(OF_manual, OF_manual.1, by = "Wetland_Co") %>%
#   dplyr::select(-c(OF8_0, OF9_0)) %>%
#  # dplyr::mutate_all(str_replace(., "N/A", "0")) %>%
#   replace(is.na(.), 0) %>%
#   dplyr::select(Wetland_Co, order(colnames(.)))
#
#
#
# #wesp.5 <- setNames(as.data.frame(t(wesp.2)),row.names(wesp.2))
# #colOrder <- str_sort(rownames(wesp.5), numeric = TRUE)
# #wesp.5 <- wesp.5[match(colOrder, rownames(wesp.5)), ]
# #wesp.5 <- tibble::rownames_to_column(wesp.5, var = "Question")
#
# #wesp_M_OF <- wesp.5
#
#
# #StressCols <- colnames(WFormS3)
#
# if(write_subfiles){
#
#   openxlsx::write.xlsx(wesp_M_OF, fs::path(out_dir, "wesp_m_of.xlsx"),
#                        overwrite = overwrite, rowNames = FALSE, colNames = TRUE)
# }
#
# cli::cli_alert_success("Manual Office data sucessfully processed")
#
# ##################################################################
