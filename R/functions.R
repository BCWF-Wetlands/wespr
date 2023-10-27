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
  lapply(questions, function(x) {
    values <- data$site_1[data$q_no == x$name]
    x$value <- values
    x
  })
}
