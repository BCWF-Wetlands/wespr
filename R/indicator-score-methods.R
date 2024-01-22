as.indicator_score <- function(score = NULL, subscores = NULL) {

  stopifnot(is.numeric(score))
  class <- character()

  if (!is.null(subscores)) {
    stopifnot(is.numeric(subscores), !is.null(names(subscores)))
    class <- "with_subscores"
    score <- list(score = score, subscores = subscores)
  }
  structure(
    score,
    class = c(class, "indicator_score")
  )
}

#' @export
round.with_subscores <- function(x, ...) {
  round(x$score, ...)
}

#' @export
round.indicator_score <- function(x, ...) {
  unclass(NextMethod())
}

get_score_value <- function(x, ...) {
  if (is.null(x)) return(x)
  UseMethod("get_score_value")
}

#' @export
get_score_value.with_subscores <- function(x, ...) {
  x$score
}

#' @export
get_score_value.indicator_score <- function(x, ...) {
  unclass(x)
}

#' @export
print.indicator_score <- function(x, ...) {
  cat("Score:", round(unclass(x), 2))
}

#' @export
print.with_subscores <- function(x, ...) {
  cat("Score:", x$score, "\n")
  cat("  Subscores: \n")
  sscore_names <- names(x$subscores)
  cat(paste0("    - ", sscore_names, ": ", round(x$subscores, 2)), sep = "\n")
}
