#' WESP calibrated site values
#'
#' This dataset is created for the purpose of calibrating the WESP model.
#' The script to process this data is available : `data-raw/upload-calibration-data.R`.
#'
#' @format ## `calibration_scores`
#' A data frame with 99 rows and 7 columns:
#' \describe{
#'   \item{ecoprovince}{ecoprovince abbreviation}
#'   \item{service}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#' @source see developers
"calibration_scores"


#' #' WESP weights table
#' #'
#' #' This dataset is created for the purpose of reviewing weightings used within wespr
#' #' The script to process this data is available : `internal-data.R`.
#' #'
#' #' @format ## `wt_table`
#' #' A data frame with 2083 rows and 8 columns:
#' #' \describe{
#' #'   \item{ecoprovince}{ecoprovince abbreviation}
#' #'   \item{service}{2 & 3 letter ISO country codes}
#' #'   \item{year}{Year}
#' #'   ...
#' #' }
#' #' @source see developers
#' "wt_table"
