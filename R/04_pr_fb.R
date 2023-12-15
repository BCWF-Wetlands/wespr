
# function
pr_function <- function(site) {
  indicator_data <- get_indicator_data(site, "pr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  outmap4 <- if ((vals$NoOutlet + vals$NoOutletX) > 0) {
    1
  } else {
    vals$OF6_1
  }

  aspect4 <- wt_max(indicator_data, "OF7", "function")

  wetpctrca4 <- wt_max(indicator_data, "OF11", "function")

  flodist4 <- internal_flow_distance(vals, indicator_data)

  growd4 <- degree_days_index(vals)

  gcover4 <- ground_cover(vals, indicator_data)

}


# benefit
pr_benefit <- function(site) {
  indicator_data <- get_indicator_data(site, "pr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)
}
