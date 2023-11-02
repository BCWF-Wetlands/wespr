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
    if (!valid) {
      stop("Value must be length ", n, " and be all 0s and one 1", call. = FALSE)
    }
    as.logical(x)
  }
}

multiresponse_binary <- function(n) {
  function(x) {
    x <- as.numeric(x)
    valid <- all(!is.na(x)) &&
      all(x %in% 0:1) &&
      length(x) == n
    if (!valid) {
      stop("Value must be length ", n, " and be all 0s and 1s", call. = FALSE)
    }
    as.logical(x)
  }
}

multiresponse_category <- function(n, min = -Inf, max = Inf) {
  function(x) {
    x <- as.numeric(x)
    valid <- all(!is.na(x)) &&
      all(x %in% min:max) &&
      length(x) == n
    if (!valid) {
      stop("Value must be length ", n,
           " and be an integer from ", min, " to ", max,
           call. = FALSE)
    }
    x
  }
}

numeric_value <- function(min, max) {
  function(x) {
    x <- as.numeric(x)
    valid <- length(x) == 1 &&
      !is.na(x) &&
      x >= min && x <= max
    if (!valid) {
      stop("Value must be a single number between ", min, " and ", max,
           call. = FALSE)
    }
    x
  }
}

logical_value <- function(x) {
  x <- as.numeric(x)
  valid <- length(x) == 1 &&
    !is.na(x) &&
    x %in% 0:1
  if (!valid) stop("Value must be a single logical", call. = FALSE)
  as.logical(x)
}
