fn <- function(responses, weights, fn) {
  qs <- Filter(\(x) fn %in% x$used_by, responses)
  qs <- lapply(qs, \(x) x[c("q_no", "label", "response_no", "value")])
  q_df <- dplyr::bind_rows(qs)
  dplyr::filter(weights, indicator == fn, `type (F/B)` == "function") |>
    select(q_no, response_no, q_responses, Q_weighting) |>
    right_join(q_df, by = c("q_no", "response_no"))
}
