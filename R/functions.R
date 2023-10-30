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
    if (exists("value_names", question)) {
      # This only applies to Q F2
      names(question$value) <- question$value_names
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

empty_derived_values <- function() {
  derived_values = list(
    list(
      name = "never_water",
      calculated_from = "F19",
      # the [6] is the index of the sixth choice in q F19, indicating never
      # water. I don't love the use of this "magic number" here
      generator = \(x) x$F19$value[6],
      used_by = c("services that use it"), 
      value = NA
    ),
    list(
      name = "all_water",
      calculated_from = "F19",
      # the [1] is the index of the fist choice in q F19, indicating always
      # water. I don't love the use of this "magic number" here
      generator = \(x) x$F19$value[1],
      used_by = c("services that use it"), 
      value = NA
    )
  )
  
  names_from_value(derived_values, "name")
}
