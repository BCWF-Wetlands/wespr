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
    dplyr::rename("F46_1" = .data$F46_10,
      "F46_2" = .data$F46_20
    )

  fdata <- fdata |>
    dplyr::select(-c( .data$F46_0, .data$F46_1)) |>
    dplyr::left_join(phdf, by = "Wetland_Co")


  # Case 2 :  Make list of variables that require parsing

  # update #F58 response 11 is merged with response 8, They are seperated in survey123 data,
  #combined for calculations so will be merged here

  fdataX <- fdata |>
    dplyr::mutate(F58_0 = stringr::str_replace_all(.data$F58_0, "11", "8"))

  #TODO;note this might be at risk if there are two incidents of SpeciesPres8 but not sure in this dataset
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
    dplyr::mutate(dplyr::across(c(.data$F22_0, .data$F23_0, .data$F42_0, .data$F49_0), ~ str_replace(., "N/A", "0")))

  #rename columns in F3 to match survey123

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
      dplyr::mutate(VpartsN = parse_number(!!rlang::sym(ParseVars[x]))) %>%
      dplyr::select(ParseVars[x], VpartsN)
    SplitFn1(x, df1)
  })

  WForm3.1 <- cbind(WForm_Wetland_Co, do.call(cbind, df4))
  WForm3 <- dplyr::mutate(fdata2, WForm3.1)


  # drop the shrub_1 and rename values to F4_1 to F4_3
  WForm3 <-  WForm3 |>
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
    dplyr::mutate(F22_1 =  dplyr::case_when(
      .data$F22_0 == "1" ~ 1,
      TRUE ~ 0
    )) |>
    dplyr::mutate(F23_1 =  dplyr::case_when(
      .data$F23_0 == "1" ~ 1,
      TRUE ~ 0
    )) |>
    dplyr::mutate(F42_1 =  dplyr::case_when(
      .data$F42_0 == "1" ~ 1,
      TRUE ~ 0
    )) |>
    dplyr::mutate(F49_1 =  dplyr::case_when(
      .data$F49_0 == "1" ~ 1,
      TRUE ~ 0
    ))

  # currently dropping the F57_7 as this consume7 and is not applicable response.
  WForm4 <- WForm4 |>
    dplyr::select(-.data$F57_7)

  return(WForm4)
}




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

  # Make list of variables that require parsing - Careful if changing these as it impacts the flowon question numbers
  ParseVars <- c("S1", "S2", "S3", "S4", "S5", "S6")
  NparseVars <- c(10,     5,   9,   8,   8,     4)


  # Function to split a Form variable that has multiple entries into separate variables
  SplitFn2 <- function(i, df) {
    df2 <- lapply(1:NparseVars[i], function(j) {
      FormVName <- paste0(ParseVars[i], "_", j)
      df %>%
        dplyr::mutate(!!FormVName := dplyr::case_when(
          # is.element(j, VpN) ~ 1,
          is.element(j, VpartsN) ~ 1,
          TRUE ~ 0
        )) %>%
        dplyr::select(!!rlang::sym(FormVName))
      # dplyr::select(Wetland_Co,!!rlang::sym(FormVName))
    })
    do.call(cbind, df2)
  }

  # Loop through each Variable to split out and call the function
  # that splits it into separate variables
  df3 <- lapply(1:length(ParseVars), function(x) {
    df1 <- WFormS %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Vparts = (strsplit(!!rlang::sym(ParseVars[x]), ","))) %>%
      dplyr::mutate(VpartsN = list(parse_number(Vparts))) %>%
      dplyr::select((ParseVars[x]), Vparts, VpartsN)
    SplitFn2(x, df1)
  })

  # Combine generated form sub-variables with original data.frame
  WFormS2.1 <- cbind(WForm_Wetland_Co, do.call(cbind, df3))
  df3 = NA
  WFormS2 <- dplyr::mutate(WFormS, WFormS2.1) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  # I dont fully comprehend these values - need to review in detail to make sure they
  # make sense
  # Split out form binary variables that are contained in 1 variable
  ParseVars <- c(
    "S1_11", "S1_12", "S1_13", "S1_14", "S2_6", "S2_7", "S2_8", "S3_10", "S3_11", "S3_12",
    "S4_10", "S4_11", "S4_12", "S5_9", "S5_10", "S5_11", "S5_12", "S6_5", "S6_6"
  )

  WFormS3 <- WFormS2 |>
    dplyr::mutate(dplyr::across(dplyr::all_of(ParseVars), parse_number)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
    dplyr::select(
      .data$Wetland_Co,
      dplyr::starts_with("S1_"), dplyr::starts_with("S2_"),
      dplyr::starts_with("S3_"), dplyr::starts_with("S4_"),
      dplyr::starts_with("S5_"), dplyr::starts_with("S6_")
    )# |>
    #dplyr::mutate(
    #  S1_15 = 0,
    #  S1_16 = 0,
    #  S2_8 = 0,
    #  S2_9 = 0,
    #  S2_10 = 0,
    #  S3_13 = 0,
    #  S3_14 = 0,
    #  S4_13 = 0,
    #  S4_14 = 0,
    #  S5_13 = 0,
    #  S5_14 = 0,
    #  S6_5 = 0,
    #  S6_6 = 0
    #)

  WFormS3[is.na(WFormS3)] <- "0"


  return(WFormS3)
}



# ##########################################################
# ## import manual office questions.
#
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
# # GP note:
# ## output file for field survey 123 stressors = WFormS3
#
#
# ##################################################################
