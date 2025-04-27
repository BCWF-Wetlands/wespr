#' Combine all types of raw data
#'
#' @param field_data A path to the field data
#' @param office_data A path to the office data
#' @param EcoP A character string specifying the region. Default = 'GD'
#' @param write_subfiles A logical value specifying whether to write the subfiles. Default = TRUE
#' @param out_dir A character string specifying the output directory.
#' @param overwrite A logical value specifying whether to overwrite the output files. Default = FALSE
#'
#' @returns A tibble with the combined data
#' @export
#'
#' @examples
#' \dontrun{
#' combine_rawdata(
#'   field_data = fs::path("inst/input_data/raw", "field_survey123_edited.xls"),
#'   office_data = fs::path("inst/input_data/raw", "scripted_office.xlsx"),
#'   EcoP = "GD",
#'   write_subfiles = FALSE,
#'   out_dir = "temp",
#'   overwrite = TRUE
#' )
#' }
combine_rawdata <- function(field_data,
                            office_data,
                            EcoP = "GD",
                            write_subfiles = TRUE,
                            out_dir = "temp",
                            overwrite = FALSE) {

# check if the output directory exists and if not create it.

## testing rows
   # field_data <- system.file("extdata/field_survey123_edited_04.14.2025.xls", package = "wespr")
   # office_data <- system.file("extdata/scripted_office.xlsx", package = "wespr")
   # EcoP = "GD"
   # write_subfiles = FALSE
   # out_dir <- "inst/input_data/processed"
   # overwrite = TRUE

  if (!exists(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE)
    cli::cli_alert_success("Output directory created")
  }


  cli::cli_alert("Processing field data")

  indata <- readxl::read_xls(field_data,
    col_names = TRUE, sheet = 1,
    col_types = c(rep("text", 2), "date", rep("text", 117))
  )

  # check if date is read in correctly
  if (anyNA(indata$datetime)) {
    cli::cli_alert_danger("Some dates are incompatible formate, please check datetime
                        column and ensure all date is in the format dd/mm/yyyy,
                        rows which do not have this format will be removed")

    indata <- indata |>
      dplyr::filter(!is.na(.data$datetime))
  }

  indata <- indata  |>
    dplyr::filter(.data$region == EcoP) |>
    dplyr::mutate(date = format(as.POSIXct(.data$datetime, format = "%m/%d/%Y %H:%M:%S"), format = "%m/%d/%Y")) %>%
    dplyr::rename("Wetland_Co" = .data$Wetland_ID) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_trim(.))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ dplyr::na_if(., "")))

  # check for duplicate id numbers
  ids <- indata |>
    dplyr::group_by(.data$Wetland_Co) |>
    dplyr::count() |>
    dplyr::filter(.data$n > 1)

  if (nrow(ids) > 0) {
    cli::cli_abort("There are duplicate Wetland_Co in the data, please check the data")
    print(ids)
  }

  # Missing fields check
  indata <- indata |>
    dplyr::select(.data$Wetland_Co, dplyr::starts_with(c("F", "S"))) |>
    dplyr::select(-c("surveyors"))

  # check the fields are all present.
  standard_cols <- c(
    "Wetland_Co", "F1_1", "F1_2", "F1_3", "F1_4", "F1_5",
    "F1_6", "F2_0", "F3_0", "F4_0", "F5_0", "F6_0", "F7_0",
    "F8_0", "F9_0", "F10_0", "F11_0", "F12_0",
    "F13_0", "F14_0", "F15_0", "F16_0", "F17_0", "F18_0",
    "F19_0", "F20_0", "F21_0", "F22_0", "F23_0", "F24_0",
    "F25_0", "F26_0", "F27_0", "F28_0", "F29_0", "F30_0",
    "F31_0", "F32_0", "F33_0", "F34_0", "F35_0", "F36_0",
    "F37_0", "F38_0", "F39_0", "F40_0", "F41_0", "F42_0",
    "F43_0", "F44_0", "F45_0", "F45_1", "F46_1", "F46_0", "F47_0",
    "F48_0", "F49_0", "F50_0", "F51_0", "F52_0", "F53_0",
    "F54_0", "F55_0", "F56_0", "F57_0", "F58_0", "F59_0",
    "S1", "S1_11", "S1_12", "S1_13", "S1_14", "S2",
    "S2_6", "S2_7", "S2_8", "S3", "S3_10", "S3_11",
    "S3_12", "S4", "S4_9", "S4_10", "S4_11", "S4_12",
    "S5", "S5_9", "S5_10", "S5_11", "S5_12", "S6",
    "S6_3", "S6_4"
  )


  missing_cols <- dplyr::setdiff(standard_cols, colnames(indata))

  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("The following columns are missing from the data : {missing_cols}}")
    cli::cli_alert_danger("Please check the input data and ensure all columns are present")
  }


  #############################################################
  # Format Field data

  WForm4 <- processing_fielddata(indata = indata)

  if (write_subfiles) {
    openxlsx::write.xlsx(WForm4, fs::path(out_dir, "wesp_f.xlsx"),
      overwrite = overwrite, rowNames = FALSE, colNames = TRUE
    )
  }

  cli::cli_alert_success("Field data sucessfully processed")


  #############################################################
  # 3) Format stressor data:

  cli::cli_alert("Processing stressor data")

  WFormS3 <- processing_stressordata(indata = indata)

  if (write_subfiles) {
    openxlsx::write.xlsx(WFormS3, fs::path(out_dir, "wesp_s.xlsx"),
      overwrite = overwrite, rowNames = FALSE, colNames = TRUE
    )
  }

  cli::cli_alert_success("Stressor data sucessfully processed")

  #############################################################
  # 3) Format office data:


  cli::cli_alert("Processing scripted office data")

  # Note if showing error in opening make sure you dont have the file open

  ofdata <- readxl::read_xlsx(office_data) |>
    dplyr::rename(Wetland_Co = .data$WTLND_ID) |>
    dplyr::filter(.data$Wetland_Co %in% WForm4$Wetland_Co) |>
    dplyr::rename("OF15_1" = .data$OF15) |>
    dplyr::rename("OF16_1" = .data$OF16) |>
    dplyr::rename("OF17_1" = .data$OF17) |>
    dplyr::rename("OF21_1" = .data$OF21) |>
    dplyr::rename("OF22_1" = .data$OF22)

  fd <- WForm4
  sd <- WFormS3
  osd <- ofdata

  # check length of outputs

  if (nrow(fd) != nrow(sd)) {
    cli::cli_alert_danger("Field and stressor data do not have the same number of rows")
  }
  if (nrow(fd) != nrow(osd)) {
    cli::cli_alert_danger("Field and office data do not have the same number of rows")
  }

  wespF <- list(fd, osd, sd)

  wesp.1 <- wespF %>%
    purrr::reduce(dplyr::full_join, by = "Wetland_Co") %>%
    dplyr::select(-dplyr::contains("_0"))

  wesp <- stats::setNames(as.data.frame(t(wesp.1)), row.names(wesp.1))
  colOrder <- stringr::str_sort(rownames(wesp), numeric = TRUE)
  wesp <- wesp[match(colOrder, rownames(wesp)), ]
  wesp <- tibble::rownames_to_column(wesp, var = "Question")

  # convert NAs to 0
  wesp[is.na(wesp)] <- as.character(0)


  return(wesp)
}
