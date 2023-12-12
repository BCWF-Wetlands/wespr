
sr_func <- function(site) {

  indicator_data <- get_indicator_data(site, "sr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  salinity2 <- ifelse(
    vals$NoOutlet + vals$NoOutletX == 0,
    1, # TODO: verify 1 vs OutDura3 (calced from F40 - Channel Connection and Ouflow duration)
    vals$OutMap
  )

  wetpctrca3 <- wt_max(indicator_data, "OF11", "func")
}


# benefit

