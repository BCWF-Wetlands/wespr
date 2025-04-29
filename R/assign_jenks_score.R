#' Assign Jenks score based on calibration dataset
#'
#' @param ind_scores A data.frame of indicator scores. The output of get_indicator_scores().
#' @param calibration_scores an internal dataset containing the calibration data for all sites. This can be updated by admin.
#' @param EcoP A character string specifying the region. Default = 'GD'
#'
#' @returns A data.frame with indicator scores and jenks classification score (Low, Medium, High (L, M, H)).
#' @export
#' @examples
#' \dontrun{
#' wesp <- fs::path("inst/input_data/wetFlat_20250427_single.csv")
#' wesp_data <- load_wesp_data(wesp)
#' site <- as.wesp_site(wesp_data)
#' site <- calc_indicators(site)
#' ind_scores <- get_indicator_scores(site)
#' out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "GD")
#' }

assign_jenks_score <- function(ind_scores, calibration_scores, EcoP) {

  # testing lines
  #ind_scores
  #calibration_scores
  #EcoP <- "GD"
  # end testing lines

  # check calibration data contains ecoprovince
  if (!unique(calibration_scores$ecoprovince) %in% EcoP) {
    cli::cli_abort("No calibration data is currently stored for the selected Eco Province.
                   Please check your ecoprovince name or select from the following:
                   {unique(calibration_scores$ecoprovince)}")
  }

  # format ind_scores to long format

  ind <- ind_scores |>
    dplyr::select(.data$site, .data$indicator, .data$fun) |>
    dplyr::mutate(service_type = "f") |>
    dplyr::rename("value" = .data$fun)

  indb <- ind_scores |>
    dplyr::select(.data$site, .data$indicator, .data$ben) |>
    dplyr::mutate(service_type = "b") |>
    dplyr::rename("value" = .data$ben)

  ind <- rbind(ind, indb)

  # loop through the ind_score data and find match for each row that corresponds with
  # either L, M, H class in calibration value

  classed_df <- lapply(1:nrow(ind), function(i) {
    # print(i)
    # i <- 19
    trow <- ind[i, ]

    # filter for the service and f/b
    calr <- calibration_scores |>
      dplyr::filter(.data$service_name == trow$indicator) |>
      dplyr::filter(.data$service_type == trow$service_type)

    my_min <- trow$value
    my_max <- trow$value

    # is na then assign to NA
    if (is.na(my_min)) {
      cal_val <- NA
    } else {
      my_df_filtered <- calr |>
        dplyr::rowwise() |>
        dplyr::filter(my_min >= .data$min & my_max <= .data$max)

      if (nrow(my_df_filtered) == 0) {
        # check if value is higher than H max or lower than min for L

        calr_h <- calr |>
          dplyr::filter(.data$jenks == "H") |>
          dplyr::select(.data$max) |>
          dplyr::pull()

        if (my_max > calr_h) {
          cal_val <- "H"
        } else {
          calr_l <- calr |>
            dplyr::filter(.data$jenks == "L")

          cal_val <- "L"
        }

        cli::cli_alert_warning("Value {round(trow$value,2)} for {trow$indicator} ({trow$service_type}) is outside the calibration range, assign to closest match")
      } else {
        cal_val <- my_df_filtered$jenks
      }
    }

    trow |> dplyr::mutate(calibrated_score = cal_val)
  }) |> dplyr::bind_rows()


  return(classed_df)
}
