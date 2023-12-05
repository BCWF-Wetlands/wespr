#' Load the data containing the responses
#'
#' This file is usually an output from the R script at
#' https://github.com/BCWF-Wetlands/WESP_Calculator
#'
#' @param path path to csv
#'
#' @return a `data.frame` of the responses
#' @export
load_wesp_data <- function(path) {
  readr::read_csv(path, name_repair = "universal") |>
    dplyr::rename_with(
      \(x) gsub("...", "site_", x),
      dplyr::starts_with("...")
    ) |>
    dplyr::rename(
      response_no = .data$Question,
    ) |>
    dplyr::mutate(
      response_no = dplyr::case_when(
        .data$response_no == "F46_1" ~ "F46a_1",
        .data$response_no == "F46_2" ~ "F46b_1",
        .default = .data$response_no
      ),
      q_no = stringr::str_split_i(.data$response_no, "_", 1),
      response_no = stringr::str_split_i(.data$response_no, "_", 2)
    ) |>
    dplyr::select("q_no", "response_no", dplyr::everything())
}

#' Validate and record responses into a standard object
#'
#' This currently is only implemented for a single site
#'
#' @param data a `data.frame`, the output of [load_wesp_data()]
#'
#' @return a `list` object containing validated responses and question metadata
#' @export
record_values <- function(data) {
  questions <- make_core_questions()
  lapply(questions, function(question) {
    values <- data$site_1[data$q_no == question$no]
    question$value <- as.list(question$validator(values))
    if (!exists("response_no", question)) {
      # This only applies to Q F2
      question$response_no <- paste(question$no, seq(1, length(values)), sep = "_")
    }
    names(question$value) <- question$response_no
    question
  })
}

#' Calculate derived values from question responses
#'
#' Examples are `all_water`, `never_water`, etc.
#'
#' @param questions an object resulting from running [record_values()]
#'
#' @return A `list` object containing derived values and their metadata.
#' @export
derive_values <- function(questions) {
  lapply(empty_derived_values(), function(item) {
    item$value <- item$generator(questions)
    item
  })
}
