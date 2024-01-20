#' @export
print.wesp_site <- function(x, ...) {
  cat("A wesp_site object\n\n")

  cat("Site: ", x$site_name, "\n\n")

  print_incomplete_questions_helper(x$questions)

  cat("Derived values:\n")
  print_derived_values_helper(x$derived_values)

  cat("\n")
  cat("Indicators:\n")
  print_indicators_helper(x$indicators)

  invisible(x)
}

print_incomplete_questions_helper <- function(x) {
  incomplete_q <- vapply(x, function(y) {
    isTRUE(y$incomplete)
  }, FUN.VALUE = logical(1))

  if (any(incomplete_q)) {
    incomplete <- names(x)[incomplete_q]
    cat("Incomplete Questions: ", paste(incomplete, collapse = ", "), "\n\n")
  }
}

print_derived_values_helper <- function(x) {
  labels <- paste(names(x), format_value(x), sep = " = ")
  cat(paste("  * ", labels), sep = "\n")
}

print_indicators_helper <- function(x) {
  if (is.null(unlist(x, recursive = TRUE))) {
    cat("All indicators are NULL. Run `calc_indicators()` to calculate them.\n")
  } else {
    lapply(names(x), function(n) {
      cat(paste0("  * ", toupper(n), ": "), "\n") # Indicator name
      lapply(names(x[[n]]), function(t) {
        cat(paste0("    - ", t, ": ", format_value(x[[n]][[t]])), "\n") # Indicator type and value
    })
    })
    cat("\n* Retrieve indicator scores with `get_indicator_scores()`")
  }
}

format_value <- function(x) {
  if (!is.null(x)) {
    x <- round(x, 2)
  }
  x
}

#' Retrieve indicator scores from a `wesp_site` object as a data.frame.
#'
#' @param site A `wesp_site` object, having been updated with [calc_indicators()]
#' @param ... ignored for now
#'
#' @return A data.frame of indicator scores
#' @export
get_indicator_scores <- function(site, ...) {
  UseMethod("get_indicator_scores")
}

#' @export
get_indicator_scores.default <- function(site, ...) {
  stop("No method defined for object of class '", class(site), call. = FALSE)
}

#' @export
get_indicator_scores.wesp_site <- function(site, ...) {
  # TODO: in each indicator there is a list of two - need to get both fun and ben
  # score_values, trying to reconstruct a list of two-element vectors like the
  # old one was
  indicator_scores <- vapply(site$indicators, get_score_value, FUN.VALUE = numeric(1))

  dplyr::bind_cols(
    site = site$site_name,
    dplyr::bind_rows(indicator_scores, .id = "indicator")
  ) %>%
    dplyr::mutate(indicator = toupper(.data$indicator))
}


#' Retrieve all input responses from a `wesp_site` object as a data.frame.
#'
#' @param site A `wesp_site` object
#' @param ... ignored for now
#'
#' @return A data.frame of responses
#' @export
get_responses <- function(site, ...) {
  UseMethod("get_responses")
}

#' @export
get_responses.default <- function(site, ...) {
  stop("No method defined for object of class '", class(site), call. = FALSE)
}

#' @export
get_responses.wesp_site <- function(site, ...) {
  lapply(site$questions, function(q) {
    dplyr::as_tibble(q[c("no", "question", "response_no", "value")])
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(value = unlist(.data$value))
}

#' Retrieve all derived values from a `wesp_site` object as a data.frame.
#'
#' @param site A `wesp_site` object
#' @param ... ignored for now
#'
#' @return A data.frame of responses
#' @export
get_derived_values <- function(site, ...) {
  UseMethod("get_derived_values")
}

#' @export
get_derived_values.default <- function(site, ...) {
  stop("No method defined for object of class '", class(site), call. = FALSE)
}

#' @export
get_derived_values.wesp_site <- function(site, ...) {
dplyr::tibble(
  name = names(site$derived_values),
  value = unname(site$derived_values)
  )
}

