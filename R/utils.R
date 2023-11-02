check_numeric <- function(x) {
  valid <- !is.na(x) && x >= 0 && x <= 1
  if (!valid) warning("Help!")
}

names_from_value <- function(x, value) {
  if (!is.list(x)) stop("x must be a list", call. = FALSE)
  names(x) <- vapply(x, `[[`, value, FUN.VALUE = "")
  x
}
