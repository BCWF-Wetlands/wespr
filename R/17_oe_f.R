
oe_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "oe")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev7 <- 1 - vals$OF5_1

  constric7 <- outflow_confinement(vals, indicator_data)

  # TO DO ; check the refernce for this alternate - lists G32 but this refers to F41 not F40?
  #https://github.com/BCWF-Wetlands/wespr/issues/56
  outmap7 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    vals$OF6_1
  } else {
    constric7
  }

  flordist7 <- wt_max(indicator_data, "OF10", "fun")

  wetdef7 <- local_moisture_deficit(vals)

  gdd7 <- degree_days_index(vals)

  soiltex7 <- wt_max(indicator_data, "F17", "fun")

  fringe7a <- if(vals$NeverWater == 1 ||
                 vals$NoPersis ==1 ){
    NA_real_
  } else {
    vals$F22_1
  }

  interspers7 <- interspersion_inundated_veg_1(vals, indicator_data)

  outdura7 <- if(vals$F40_4 + vals$F40_5 > 0){
    outmap7
  } else {
    wt_max(indicator_data, "F40", "fun")
  }

  thruflo7 <- throughflow_resistance(vals, indicator_data)

  gradient7 <- internal_gradient(vals, indicator_data)

  groundw7 <-  wt_max(indicator_data, "F47", "fun")


  #TODO : add calculated score elequantly
  #appscore7 <-


  #TODO : add calculated score elequantly
  #csscore7 <-

  # subscores

  histaccum7 <- csscore7
  productiv7 <- appscore7

  exportpot7 <- ifelse((vals$NoOutlet + vals$NoOutletX) > 0, 0,
                   mean(c(outdura7 ,
                          mean(c(gdd7, groundw7, elev7, gradient7, wetdef7)),
                          mean(c(thruflo7, interspers7, fringe7a, constric7, flordist7)))))

  oe_fun_score <- 10 * ifelse(Outmap7 == 0, 0,
                              (3 * exportpot7 * max(productiv7, histaccum7)) / 3)

  oe_fun_score
}
