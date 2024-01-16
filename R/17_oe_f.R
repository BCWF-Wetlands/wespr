
oe_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "oe", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev7 <- 1 - vals$OF5_1

  constric7 <- outflow_confinement(vals, indicator_data)

  # as fixed for FH in
  # https://github.com/BCWF-Wetlands/wespr/commit/16dcc6e4c27342d925c1134c4c34f794e81b2feb

  if (vals$NoOutlet + vals$NoOutletX == 0) {
    outmap7 <- outdura7<- wt_max(indicator_data, "F40")
  } else {
    outdura7 <-  outmap7 <- vals$OF6_1
  }


  flordist7 <- wt_max(indicator_data, "OF10")

  wetdef7 <- local_moisture_deficit(vals)

  gdd7 <- degree_days_index(vals)

  soiltex7 <- wt_max(indicator_data, "F17")

  fringe7a <- if(vals$NeverWater == 1 ||
                 vals$NoPersis ==1 ){
    NA_real_
  } else {
    vals$F22_1
  }

  interspers7 <- interspersion_inundated_veg_1(vals, indicator_data)

  thruflo7 <- throughflow_resistance(vals, indicator_data)

  gradient7 <- internal_gradient(vals, indicator_data)

  groundw7 <-  wt_max(indicator_data, "F47")

  appscore7 <- site$indicators$app$fun /10

  cpscore7 <- site$indicators$cp$fun/10


  # subscores

  histaccum7 <- cpscore7
  productiv7 <- appscore7

  exportpot7 <- ifelse((vals$NoOutlet + vals$NoOutletX) > 0, 0,
                   mean_na(c(outdura7 ,
                          mean_na(c(gdd7, groundw7, elev7, gradient7, wetdef7)),
                          mean_na(c(thruflo7, interspers7, fringe7a, constric7, flordist7)))))

  oe_fun_score <- 10 * ifelse(outmap7 == 0, 0,
                              (3 * exportpot7 * max(productiv7, histaccum7)) / 3)

  oe_fun_score
}
