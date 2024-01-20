as.indicator_score <- function(x) {
  class <- "indicator_score"
  if (is.list(x)) {
    stopifnot(names(x) == c("score", "subscores"))
    class <- c("with_subscores", class)
  }
  class(x) <- class
  x
}

# TODO: Shouldn't need to export these to work internally, but for interactive
# testing they're helpful

#' @export
round.with_subscores <- function(x, ...) {
  round(x$score, ...)
}

#' @export
get_score_value <- function(x, ...) {
  if (is.null(x)) return(x)
  UseMethod("get_score_value")
}

#' @export
get_score_value.with_subscores <- function(x, ...) {
  NextMethod("get_score_value")
  x$score
}

#' @export
get_score_value.indicator_score <- function(x, ...) {
  unclass(x)
}
