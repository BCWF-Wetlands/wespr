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
      q_no = stringr::str_split_i(response_no, "_", 1)
    ) |> 
    dplyr::select(q_no, dplyr::everything())
}

check_numeric <- function(x) {
  valid <- !is.na(x) && x >= 0 && x <= 1
  if (!valid) warning("Help!")
}

names_from_value <- function(x, value) {
  if (!is.list(x)) stop("x must be a list", call. = FALSE)
  names(x) <- vapply(x, `[[`, value, FUN.VALUE = "")
  x
}

record_values <- function(questions, data) {
  lapply(questions, function(question) {
    values <- data$site_1[data$q_no == question$name]
    question$value <- question$validator(values)
    question
  })
}

## Validators 
## These ensure that the values being input from the questions are of
## the right type and length, and within the right range. They are used in the
## `record_values()` function

multi_choice <- function(n) {
  function(x) {
    x <- as.numeric(x)
    valid <- all(!is.na(x)) && 
      all(x %in% 0:1) && 
      sum(x) == 1 && 
      length(x) == n
    if (!valid) stop("Value must be length ", n, " and be all 0s and one 1")
    as.logical(x)
  }
}

multiresponse_binary <- function(n) {
  function(x) {
    x <- as.numeric(x)
    valid <- all(!is.na(x)) && 
      all(x %in% 0:1) && 
      length(x) == n
    if (!valid) stop("Value must be length ", n, " and be all 0s and 1s")
    as.logical(x)
  }
}

multiresponse_category <- function(n, min = -Inf, max = Inf) {
  function(x) {
    x <- as.numeric(x)
    valid <- all(!is.na(x)) && 
      all(x %in% min:max) && 
      length(x) == n
    if (!valid) stop("Value must be length ", n, " and be an integer from ", min, " to ", max)
    x
  }
}

numeric_value <- function(min, max) {
  function(x) {
    x <- as.numeric(x)
    valid <- length(x) == 1 &&
      !is.na(x) && 
      x >= min && x <= max
    if (!valid) stop("Value must be a single number between ", min, " and ", max)
    x
  }
}

logical_value <- function(x) {
  x <- as.numeric(x)
  valid <- length(x) == 1 &&
    !is.na(x) &&
    x %in% 0:1
  if (!valid) stop("Value must be a single logical")
  as.logical(x)
}

