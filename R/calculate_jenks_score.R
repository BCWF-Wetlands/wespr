#' Calculate Jenks Breaks
#'
#' @param wespdata A data frame containing the formatted wesp data. This is the output
#' of load_wesp_data()
#' @param sites A numeric with the number of sites if specific sites are to be calculated.
#' The default is NULL,
#' which will include all sites in the calculation
#' @param out_dir a character string or path to location where output file is to be saved
#' @param out_name a character string with the name of the output file.
#' Default values is "wesp_scores.csv"
#'
#' @returns a dateframe with compiled raw, normalised and jenks break values for
#' each site and each ecosystem function or benefit.
#' @export
#'
#' @examples
#' \dontrun{
#' wespdata <- load_wesp_data(system.file("input_data/wetFlat_20240130.csv", package = "wespr"))
#' calculate_jenks_score(wespdata, out_dir = "temp",  out_name = "wesp_scores.csv")
#'}
calculate_jenks_score <- function(wespdata, sites = NULL, out_dir, out_name = "wesp_scores.csv") {

  #testing lines
 # wespdata <- wesp_data
#  sites = NULL
##  out_dir = "temp"
#  out_name = "wesp_scores_test1.csv"

  # run multi-site analysis
  wespRaw <- calculate_multi_site(wespdata, sites = sites)

  # check out dir exists and if not create it
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  # Calculate Jenks breaks and add to data.frame

  # 1) First normalize the service and add to data.frame

  min_max_norm <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }

  # apply Min-Max normalization
  wespNorm <- as.data.frame(lapply(wespRaw[, -1], min_max_norm)) %>%
    dplyr::mutate(site = as.numeric(rownames(.)), .before = 1)
  wespNorm <- dplyr::rename_with(wespNorm, ~ gsub("_raw", "_norm", .x, fixed = TRUE))


  # #format to long form
  #
  # norm_long <- pivot_longer(wespNorm, -site, names_to = "metric", values_to = "value") |>
  #   mutate(type = "normalised") |>
  #   mutate(name = gsub( "_norm", "", metric))
  #
  # norm_raw <- pivot_longer( wespRaw, -site, names_to = "metric", values_to = "value") |>
  #   mutate(type = "raw")|>
  #   mutate(name = gsub( "_raw", "", metric))
  #
  # all <- rbind(norm_long, norm_raw)
  #
  # ggplot( norm_raw, aes(value, fill = type)) +
  #    geom_density(alpha = 0.2) +
  #    facet_wrap(~name, scales = "free") +
  #    theme_bw()
  #



  # 2) Calculate Jenks brakes
  wesp_breaks_raw <- purrr::map(names(wespNorm)[-1], function(x) {
    # x <- names(wespNorm)[-1][15]
    if (all(is.na(wespNorm[[x]]) == TRUE)) {
      cli::cli_alert_warning("skipping calculation of jenks breask for {x} as all values are NA")
      return(NA)
    } else {
      jen_breaks <- BAMMtools::getJenksBreaks(wespNorm[[x]], 4, subset = NULL)
      .bincode(wespNorm[[x]], sort(jen_breaks), include.lowest = TRUE)
    }
  })

  names(wesp_breaks_raw) <- names(wespNorm)[-1]

  # Change numeric to character High, Medium, Low
  wesp_breaks_cat <- lapply(wesp_breaks_raw[1:length(wesp_breaks_raw)], function(x) {
    dplyr::case_when(
      x == 1 ~ "L",
      x == 2 ~ "M",
      x == 3 ~ "H"
    )
  })

  # Change list to data frame
  wespBreaks <- as.data.frame(do.call(cbind, wesp_breaks_cat))
  wespBreaks <- wespBreaks |>
    dplyr::mutate(site = as.numeric(rownames(wespBreaks)), .before = 1)
  wespBreaks <- dplyr::rename_with(wespBreaks, ~ gsub("_norm", "_jenks", .x, fixed = TRUE))

  # Make a single data frame that includes the raw, normalized and Jenks values
  wespEcoS <- list(wespRaw, wespNorm, wespBreaks) %>%
    purrr::reduce(dplyr::full_join, by = "site") %>%
    dplyr::select(site, sort(names(.)))

  # wespEcoS <-data.frame(Wetland_Co=wetLUT,wespEcoS.1)

  # #   # generate a histograph per metrics
  #    library(ggplot2)
  #    library(tidyr)
  #    ggplot(gather(wespEcoS), aes(value)) +
  #     geom_histogram(bins = 10) +
  #      facet_wrap(~key, scales = 'free_x')
  #
  #    #wespNorm %>% gather() %>% head()


  # Write out the data frame
  utils::write.csv(wespEcoS, fs::path(out_dir, out_name), row.names = FALSE)
  cli::cli_alert_success("WESP scores calculated and saved to {fs::path(out_dir, out_name)}")
}
