get_indicator_data <- function(site, ind, type = c("fun", "ben")) {
  check_wesp_site(site)

  type <- match.arg(type)

  type_pre <- substr(type, 1, 1)

  qs <- Filter(
    \(x) grepl(type_pre, x$used_by[ind]),
    site$questions
  )

  qs <- lapply(qs, \(x) x[c("no", "question", "response_no", "value")])

  derived_values <- list(
    no = "derived",
    response_no = names(site$derived_values),
    value = as.list(site$derived_values)
  )

  qs_df <- dplyr::bind_rows(qs)

  all_resps <- dplyr::bind_rows(qs_df, derived_values)

  weights <- dplyr::filter(
    indicator_weightings,
    .data$type_f_b == type,
    tolower(.data$indicator) == tolower({{ind}})
  )

  dplyr::left_join(all_resps, weights, by = "response_no")
}

get_vals <- function(indicator_data) {
  vals <- as.list(indicator_data$value)
  names(vals) <- indicator_data$response_no
  vals
}

get_weights <- function(indicator_data) {
  weights <- as.list(indicator_data$q_weighting)
  names(weights) <- paste0("W", indicator_data$response_no)
  weights
}

get_q <- function(wesp_site, q_no) {
  check_wesp_site(wesp_site)

  if (q_no %in% names(wesp_site$derived_values)) {
    return(wesp_site$derived_values[q_no])
  }

  q <- strsplit(q_no, split = "_")[[1]][1]
  wesp_site$questions[[q]]$value[[q_no]]
}
