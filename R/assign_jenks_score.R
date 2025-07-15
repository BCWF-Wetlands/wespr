#' Assign Jenks score based on calibration dataset
#'
#' @param ind_scores A data.frame of indicator scores. The output of get_indicator_scores().
#' @param calibration_scores an internal dataset containing the calibration data for all sites. This can be updated by admin.
#' @param EcoP A character string specifying the region. Default = 'GD'
#' @param report A logical indicating whether to generate a report. Default = TRUE.
#' @param output_dir A character string specifying the directory to save the report. Default = NULL.
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

assign_jenks_score <- function(ind_scores, calibration_scores, EcoP, report = TRUE, output_dir) {

  # testing lines
  ind_scores
  calibration_scores
  EcoP <- "GD"
  report = TRUE
  output_dir = fs::path("temp")
  # end testing lines

  # check calibration data contains ecoprovince
  if (!EcoP %in% unique(calibration_scores$ecoprovince)) {
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

  calibration_scores_eco <- calibration_scores |>
    dplyr::filter(.data$ecoprovince == EcoP)

  # format the calibration scores to summarise to get the range of each category per service

  wcols <- names(calibration_scores_eco)
  ## remove the site column
  wcols <- wcols[!wcols %in% c("site", "wetland_id","ecoprovince")]
  wcols <- unique(sub("^([^_]*_[^_]*).*", "\\1", wcols))

  # loop through the data and get the min and max values of the raw scores

   outsum <- purrr::map(wcols, function(x) {
     # get the columns for each service
     #x <- wcols[1]

     tw <- calibration_scores_eco |>
       dplyr::select(dplyr::starts_with(x)) |>
       dplyr::select(-dplyr::ends_with("_norm"))
     names(tw) <- c("jenks", "raw" )

     tww <- tw |>
       dplyr::group_by(jenks) |>
       dplyr::summarise(n = dplyr::n(),
                        min = min(raw),
                        max = max(raw)) |>
      dplyr::mutate(service = x,
              service_name = unique(sub("^([^_]*).*", "\\1", service)),
              service_type = unique(sub("^[^_]*_", "", service)))
     tww

   }) |>  dplyr::bind_rows()

  # add the Eco province name
  calibration_scores_summary <- outsum |>
    dplyr::mutate(ecoprovince =  EcoP ) |>
    dplyr::select(ecoprovince, service, everything())


  # write out (temp fix while testing)
  #write.csv(calibration_scores_new, "temp/sim_jenks_breaks.csv", row.names = FALSE)



  # loop through the ind_score data and find match for each row that corresponds with
  # either L, M, H class in calibration value

  classed_df <- lapply(1:nrow(ind), function(i) {
    # print(i)
     #i <- 20
    trow <- ind[i, ]

    # filter for the service and f/b
    calr <- calibration_scores_summary  |>
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

    trow |> dplyr::mutate(calibration_scores_summary  = cal_val)

  }) |> dplyr::bind_rows()



  if(report == TRUE) {

    cli::cli_alert_info("Generating a site report")

    RMD <- fs::path_package("wespr", "extdata/site_report.rmd")

    rmarkdown::render(RMD,
                      params = list(calibration_scores_eco = calibration_scores_eco,
                                    calibration_scores_summary = calibration_scores_summary,
                                   # outsum = outsum,
                                    classed_df = classed_df),
                      #final_model = final_model,
                                    #out_bgc_dir = out_bgc_dir,
                                    #extra_pts = extra_pts),
                      output_dir = output_dir)                ## where to save the report



  }




  return(classed_df)
}
