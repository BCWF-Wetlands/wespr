#' Calculate indicators for multiple sites
#'
#' @param wespdata A data frame containing the formatted wesp data. This is the output of load_wesp_data()
#' @param sites A numeric with the number of sites if specific sites are to be calculated. The default is NULL,
#' which will include all sites in the calculation
#'
#' @returns a dataframe with raw values for each site and each ecosystem service or benefit.
#' @export
#'
#' @examples
#'\dontrun{
#' wespdata <- load_wesp_data(system.file("input_data/wetFlat_20240130.csv", package = "wespr"))
#' calculate_multi_site(wespdata)
#'}
calculate_multi_site <- function(wespdata, sites = NULL) {
  # testing
  # wespdata <- load_wesp_data(system.file("input_data/wetFlat_20250325.csv", package = "wespr"))
  # sites <- NULL
  # end testing

  # check if wespdata is a data frame
  if (!is.data.frame(wespdata)) {
    cli::cli_abort("wespdata must be a data frame")
  }

  # # check if sites is numeric? or correct format
  # if(!is.numeric(sites)) {
  #   cli::cli_abort("sites must be numeric")
  # }

  if (is.null(sites)) {
    cli::cli_alert_info("Processing all sites")
    allsites <- unique(names(wespdata)[grepl("site", names(wespdata))])
    sitelist <- seq(1, length(allsites), 1)
  } else {
    cli::cli_alert_info("Processing sites {sites}")

    # still to add
  }

  # run through all the sites and calculate values

  #sitelist <- sitelist[1:109]
  #i = 43

  cdat <- purrr::map(sitelist, function(i) {
    cli::cli_alert_info(" processsing {allsites[i]}")
    site <- as.wesp_site(wespdata, i)
    site <- calc_indicators(site)
    aa <- get_indicator_scores(site)
    aa
  }) |> dplyr::bind_rows()


  # reformat into widetable for Raw data

  cdatf <- cdat |>
    dplyr::select(-"ben") |>
    tidyr::pivot_wider(names_from = "indicator", values_from = "fun", names_glue = "{indicator}_f_raw", ) |>
    dplyr::select(.data$site, dplyr::everything()) |>
    dplyr::mutate(site = as.numeric(sub("site_", "", .data$site)))

  cdatb <- cdat |>
    dplyr::select(-"fun") |>
    tidyr::pivot_wider(names_from = "indicator", values_from = "ben", names_glue = "{indicator}_b_raw", ) |>
    dplyr::select(.data$site, dplyr::everything()) |>
    dplyr::mutate(site = as.numeric(sub("site_", "", .data$site)))

  wespRaw <- dplyr::left_join(cdatf, cdatb, by = "site")

  # drop all NA cols
  wespRaw <- wespRaw[colSums(!is.na(wespRaw)) > 0]

  # check each column for NA values and set up a flag
  na_count <- sapply(wespRaw, function(y) sum(length(which(is.na(y)))))
  na_count <- na_count[na_count > 0]

  nas_check <- wespRaw |>
    dplyr::select(.data$site, dplyr::all_of(names(na_count)))

  nas_check <- nas_check[!stats::complete.cases(nas_check), ]

  if (nrow(nas_check) == 0) {
    cli::cli_alert_success("No NA values found")
  } else {
    # output a warning and file?
    cli::cli_alert_warning("NAs found for the following ecosystem function: { names(nas_check)[-1]},")
    cli::cli_alert_warning("at these sites: {nas_check$site }")
  }

  return(wespRaw)
}
