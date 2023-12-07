fn <- function(responses, weights, fn) {
  qs <- Filter(\(x) fn %in% x$used_by, responses)
  qs <- lapply(qs, \(x) x[c("q_no", "label", "response_no", "value")])
  q_df <- dplyr::bind_rows(qs)
  dplyr::filter(weights, .data$indicator == fn, .data$`type (F/B)` == "function") |>
    dplyr::select("q_no", "response_no", "q_responses", "Q_weighting") |>
    dplyr::right_join(q_df, by = c("q_no", "response_no"))
}

indicator <- function(site, weights, ind) {
  check_wesp_site(site)
  qs <- Filter(\(x) ind %in% names(x$used_by), site$questions)
  qs <- lapply(qs, \(x) x[c("no", "question", "response_no", "value")])
  derived_values <- list(
    no = "derived",
    response_no = names(site$derived_values),
    value = unname(site$derived_values)
  )
  qs_df <- dplyr::bind_rows(qs)
  qs_df$value <- vapply(qs_df$value, function(x) {
    unname(as.numeric(x))
  }, FUN.VALUE = numeric(1))

  all_resps <- dplyr::bind_rows(qs_df, derived_values)

  weights <- filter(weights, tolower(indicator) == tolower(ind))

  dplyr::left_join(all_resps, weights, by = "response_no")
}

get_q <- function(wesp_site, q_no) {
  check_wesp_site(wesp_site)

  if (q_no %in% names(wesp_site$derived_values)) {
    return(wesp_site$derived_values[q_no])
  }

  q <- strsplit(q_no, split = "_")[[1]][1]
  wesp_site$questions[[q]]$value[[q_no]]
}

