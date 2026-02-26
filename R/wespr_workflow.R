
#' Title
#'
#' @param desktop_data A file path to survey123 csv file for office data collected
#' @param field_data A file path to survey123 csv file for field data collected
#' @param out_dir A character string specifying the output directory.
#' @param EcoP A character string specifying the region. Default = 'GD'
#'
#' @returns A tibble with the combined data
#' @export
#'
#' @examples
#' \dontrun{
#' wespr_workflow(
#'   field_data = fs::path("inst/input_data/raw", "field_survey123_edited.xls"),
#'   office_data = fs::path("inst/input_data/raw", "scripted_office.xlsx"),
#'   EcoP = "GD",
#'   write_subfiles = FALSE,
#'   out_dir = "temp",
#'   overwrite = TRUE)
#' }
wespr_workflow <- function(desktop_data,
                           field_data,
                           out_dir,
                           EcoP){
  # Test outputs
  # out_dir = "outputs"
  # field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr")
  # desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr")
  # EcoP = "GD"

  # format raw data
  ww <- format_rawinputs(
    field_data <- field_data,
    desktop_data <- desktop_data,
    write_subfiles = FALSE,
    out_dir = out_dir,
    overwrite = TRUE
  )

  # write out the file so you can review
  write.csv(ww, fs::path(output_dir, "raw_wespr_input.csv"), row.names = FALSE)
  wesp_file <- fs::path(output_dir, "raw_wespr_input.csv")

  # Read in data into wesp format
  wesp_data <- load_wesp_data(wesp_file)

  # generate a site key
  wespkey <- generate_ids(wesp_data)

  # calculate scores and choose long format
  allsites_long <- calculate_multi_site(wesp_data, format = "long")

  # make a list of all unique sites
  usites <- unique(allsites_long$site)

  # loop through all
  site_overall <- purrr::map(usites, function(x) {
    soi <- x

    ind_scores <- allsites_long |> dplyr::filter(site == soi)

    out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = EcoP)

    out
  }) |> dplyr::bind_rows()


  # add site specific names
  all_jenks <- dplyr::left_join(wespkey, site_overall, by = "site")

  return(all_jenks)
}
