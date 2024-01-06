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

max_na <- function(...)  ifelse(!all(is.na(...)), max(x, na.rm=T), NA)
#max(..., na.rm = TRUE)
min_na <- function(...) min(..., na.rm = TRUE)
mean_na <- function(x, trim = 0, ...) mean(x, trim = trim, na.rm = TRUE, ...)
sum_na <- function(...) sum(..., na.rm = TRUE)
