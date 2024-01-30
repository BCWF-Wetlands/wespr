#' Load the data containing the responses to WESP form
#'
#' This file is usually an output from the R script at
#' https://github.com/BCWF-Wetlands/WESP_Calculator, and includes
#' answers to Office (OF), Field (F), and Stressor (S) questions.
#'
#' @param path path to csv
#'
#' @return a `data.frame` of the responses
#' @export
load_wesp_data <- function(path) {

  opts <- options(
    rlib_name_repair_verbosity = "quiet"
  )
  on.exit(options(opts))

  readr::read_csv(
    path,
    col_types = "c",
    name_repair = "universal"
  ) |>
    dplyr::rename_with(
      \(x) gsub("[.]{3}", "site_", x)
    ) |>
    dplyr::rename(
      response_no = "Question",
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
#' @inheritParams as.wesp_site
#'
#' @return a `list` object containing validated responses and question metadata
#' @noRd
record_values <- function(data, site) {
  questions <- make_core_questions()
  lapply(questions, function(question) {
    values <- data[[site]][data$q_no == question$no]
    question <- validate(question, values)
    if (!exists("response_no", question)) {
      # This only applies to Q F2
      question$response_no <- paste(question$no, seq(1, length(values)), sep = "_")
    }
    names(question$value) <- question$response_no
    question
  })
}

validate <- function(question, values) {
  question$value <- question$validator(values)
  question <- check_incomplete(question)
  if (question$no == "OF29") {
    # Convert topo position character to numeric
    question$value <- topo_position(question$value)
  }
  question$value <- as.list(question$value)
  question
}

check_incomplete <- function(q) {
  if (isTRUE(attr(q$value, "incomplete"))) {
    q$incomplete <- TRUE
  }
  q
}

topo_position <- function(val) {
  switch(
    val,
    "T" = 5,
    "L" = 4,
    "D" = 3,
    "M" = 2,
    `NA_character_` = NA_character_,
    0
  )
}

