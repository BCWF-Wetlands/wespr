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
  labels <- paste(names(x), x, sep = " = ")
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
  }
}

format_value <- function(x) {
  if (!is.null(x)) {
    x <- round(x, 2)
  }
  x
}


