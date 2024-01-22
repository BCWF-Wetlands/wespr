as.indicator_score <- function(score = NULL, subscores = NULL) {

  stopifnot(is.numeric(score))
  class <- ""

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
