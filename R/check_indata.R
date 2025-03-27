#' Check the input data for basic QA
#'
#' @param indata A path to the input data
#'
#' @returns messages to the console
#' @export
#'
#' @examples
#' \dontrun{
#' indata <- fs::path("inst/input_data/wetFlat_20250325.csv")
#' check_indata(indata)
#' }
check_indata <- function(indata){

  # testing
  #indata <- fs::path("inst/input_data/wetFlat_20250325.csv")

  wcsv <- read.csv(indata)

  # 1) check broadly the data is complete:

  cli::cli_alert_info("checking input data is complete")

  wdf <- wcsv |>
    dplyr::mutate(type = dplyr::case_when(
      startsWith(.data$Question, "F") ~ "Field",
      startsWith(.data$Question, "OF") ~ "OF",
      startsWith(.data$Question, "S") ~ "S",
      TRUE ~ "id"
    )) |>
    dplyr::filter(.data$Question != "Wetland_Co")

  wdf[wdf=="T"]<-5
  wdf[wdf=="L"]<-4
  wdf[wdf=="D"]<-3
  wdf[wdf=="M"]<-2
  wdf[wdf=="U"]<-0
  wdf[wdf=="C"]<-0
  wdf[wdf=="F"]<-0


  # check field data
  fd <- wdf %>%
    dplyr::filter(.data$type == "Field")

  wf <- fd|>
    dplyr::select(-.data$Question, -.data$type) |> # this removes the alpha column if all your character columns need converted to numeric
    dplyr::mutate_if(is.character,as.double)

  # check office data
  of <- wdf %>%
    dplyr::filter(.data$type == "OF")

  odd <- of |>
    dplyr::select(-.data$Question, -.data$type) |> # this removes the alpha column if all your character columns need converted to numeric
    dplyr::mutate_if(is.character,as.double)   #|>

  #aa <- cbind(fd$Question, wf)

  if(any(is.na(colSums(odd)))) {
    na_sites <- colnames(odd)[colSums(is.na(odd)) > 0]

    cli::cli_alert("Office data is not complete: check the following sites : {na_sites}")
    #get column names where values are NA

  } else {
    cli::cli_alert_success("Office data is complete")
  }

  # check if field data is entered:
  if(any(is.na(colSums(wf)))) {
    na_sites <- colnames(wf)[colSums(is.na(wf)) > 0]

    cli::cli_alert("Field data is not complete: check the following sites : {na_sites}")
    #get column names where values are NA

  } else {
    cli::cli_alert_success("Field data is complete")
  }



  # 2) check numeric values do no contain -ve values

  cli::cli_alert_info("checking for negative values")


  if(any(wf < 0)) {
    # find column and row in which value is negative
    neg_val <- which(wf < 0, arr.ind = TRUE)

    cli::cli_alert("Field data contains negative values")
  } else {
    cli::cli_alert_success("Field data does not contain negative values")
  }


  if(any(odd < 0)) {
    # find column and row in which value is negative
    neg_val <- as.data.frame(which(odd < 0, arr.ind = TRUE))

    #odd[neg_val$row, neg_val$col]
    sites <- neg_val$col
    question <- unique(of$Question[neg_val$row])

    cli::cli_alert("Office data contains negative values : check sites {sites} for question {question}")
  } else {
    cli::cli_alert_success("Office data does not contain negative values")
  }

  # detailed site check

  cli::cli_alert_info("completing detailed site check ")

  wespdata <- load_wesp_data(indata)
  #check number of sites
  allsites <- unique(names(wespdata)[grepl("site", names(wespdata))])
  sitelist <- seq(1, length(allsites), 1)
  #sitelist <- sitelist[1:109]

  # loop through and validate the data
  cdat <- purrr::map(sitelist, function(i) {
    cli::cli_alert_info("checking site inputdata: {allsites[i]}")
    site <- as.wesp_site(wespdata, i)
  })


  cli::cli_alert_success("All sites have been checked : please review the flagged data in detail")

}
