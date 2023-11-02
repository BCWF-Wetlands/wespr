fn <- function(responses, weights, fn) {
  qs <- Filter(\(x) fn %in% x$used_by, responses)
  qs <- lapply(qs, \(x) x[c("q_no", "label", "response_no", "value")])
  q_df <- dplyr::bind_rows(qs)
  dplyr::filter(weights, .data$indicator == fn, .data$`type (F/B)` == "function") |>
    dplyr::select("q_no", "response_no", "q_responses", "Q_weighting") |>
    dplyr::right_join(q_df, by = c("q_no", "response_no"))
}
