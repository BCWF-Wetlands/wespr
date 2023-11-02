load_wesp_data <- function(path) {
  readr::read_csv("input_data/wetflat.csv", name_repair = "universal") |>
    dplyr::rename_with(
      \(x) gsub("...", "site_", x),
      dplyr::starts_with("...")
    ) |>
    dplyr::rename(
      response_no = Question,
    ) |>
    dplyr::mutate(
      q_no = stringr::str_split_i(response_no, "_", 1),
      response_no = stringr::str_split_i(response_no, "_", 2)
    ) |>
    dplyr::select(q_no, response_no, dplyr::everything())
}

record_values <- function(questions, data) {
  lapply(questions, function(question) {
    values <- data$site_1[data$q_no == question$q_no]
    question$value <- question$validator(values)
    if (!exists("response_no", question)) {
      # This only applies to Q F2
      question$response_no <- paste(question$q_no, seq(1, length(values)), sep = "_")
    }
    question
  })
}

derive_values <- function(questions) {
  lapply(empty_derived_values(), function(item) {
    item$value <- item$generator(questions)
    item
  })
}
