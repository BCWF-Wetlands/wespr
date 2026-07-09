#' Run the entire wespr workflow
#'
#' @param desktop_data A file path to survey123 csv file for office data collected
#' @param field_data A file path to survey123 csv file for field data collected
#' @param out_dir A character string specifying the output directory.
#' @param EcoP A character string specifying the region. Default = 'GD'
#' @param EcoP_calibration A character string specifying the region in which will be calibrated against. Default = N.
#' This should only be used where no calibration data is available for the ecoprovince where the wetland was sampled.
#' @param report_dir A character string specifying the output directory for the report document
#'
#' @returns A tibble with the combined data
#' @export
#'
#' @examples
#' \dontrun{
#' wespr_workflow(
#'     field_data = system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr"),
#'     desktop_data = system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr"),
#'     out_dir = "temp",
#'     EcoP = "GD",
#'     EcoP_calibration = "SIM",
#'     report_dir = "temp")
#' }
wespr_workflow <- function(desktop_data,
                           field_data,
                           out_dir,
                           EcoP,
                           EcoP_calibration = NA,
                           report_dir){
  # # # Test outputs
  #  out_dir = "outputs"
  #  field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr")
  #  desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr")
  #  EcoP = "GD"
  #  report_dir  = "temp"
  #  EcoP_calibration = "SIM"
   #   output_dir = "temp"


  # format raw data
  ww <- format_rawinputs(
    field_data <- field_data,
    desktop_data <- desktop_data,
    write_subfiles = FALSE,
    out_dir = out_dir,
    overwrite = TRUE
  )

  # get list of X and Y

  xy <- readr::read_csv(field_data,  name_repair = "minimal", show_col_types = FALSE)
  xy = xy |>
    dplyr::select(.data$`Preassigned wetland ID number (region initials followed by a number ie SI_1234)`,.data$x,.data$y) |>
    dplyr::rename("wetland_id" = .data$`Preassigned wetland ID number (region initials followed by a number ie SI_1234)`)


  # write out the file so you can review
  utils::write.csv(ww, fs::path(out_dir, "raw_wespr_input.csv"), row.names = FALSE)
  wesp_file <- fs::path(out_dir, "raw_wespr_input.csv")

  # Read in data into wesp format
  wesp_data <- load_wesp_data(wesp_file)

  # generate a site key
  wespkey <- generate_ids(wesp_data)

  # join x and y values
  wespkey <- dplyr::left_join(wespkey, xy, by = "wetland_id")


  # calculate scores and choose long format
  allsites_long <- calculate_multi_site(wesp_data, format = "long")
  allsites_long <- dplyr::left_join(wespkey, allsites_long, by = "site")

  # make a list of all unique sites
  usites <- unique(allsites_long$site)

  # loop through all
  site_overall <- purrr::map(usites, function(x) {
    #soi <- usites[1]
    soi <- x
    #print(soi)
    ind_scores <- allsites_long |> dplyr::filter(.data$site == soi)
    xy_vals = wespkey|> dplyr::filter(.data$site == soi)
    x_loc = xy_vals$x
    y_loc = xy_vals$y
    #print(xy_vals)
    out <- assign_jenks_score(ind_scores, wespr::calibration_scores, EcoP = EcoP)
    build_report(ind_scores, calibration_scores, EcoP = EcoP, output_dir = report_dir,
                 EcoP_calibration = EcoP_calibration,  x_loc = x_loc,y_loc = y_loc)

    out
  }) |> dplyr::bind_rows()


  # add site specific names
  all_jenks <- dplyr::left_join(wespkey, site_overall, by = "site")

  return(all_jenks)
}
