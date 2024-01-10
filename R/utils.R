check_numeric <- function(x) {
  valid <- !is.na(x) && x >= 0 && x <= 1
  if (!valid) warning("Help!")
}

`%||%` <- function(x, y) if (is.null(x)) y else x

check_wesp_site <- function(x) {
  if (!inherits(x, "wesp_site")) {
    stop("site must be an object of class 'wesp_site', created with `as.wesp_site`",
         call. = FALSE)
  }
}

max_na <- function(...) agg_na(..., fun = max)
min_na <- function(...) agg_na(..., fun = min)
sum_na <- function(...) agg_na(..., fun = sum)
mean_na <- function(...)  agg_na(..., fun = mean)

agg_na <- function(..., fun) {
  x <- c(...)
  if (all(is.na(x))) return(NA_real_)
  fun(x, na.rm = TRUE)
}
