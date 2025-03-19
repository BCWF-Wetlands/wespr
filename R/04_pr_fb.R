
# function
pr_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "pr", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  #keeping this in fro the minute but updated to remove from final model.
  outmap4 <- ifelse(((vals$NoOutlet + vals$NoOutletX >0) & vals$OF6_1 == 0), 1, 0)


  aspect4 <- wt_max(indicator_data, "OF7")

  wetpctrca4 <- wt_max(indicator_data, "OF11")

  flodist4 <- internal_flow_distance(vals, indicator_data)

  growd4 <- degree_days_index(vals)

  gcover4 <- ground_cover(vals, indicator_data)

  soiltex4 <- wt_max(indicator_data, "F17")

  girreg4 <- wt_max(indicator_data, "F18")

  permw4 <- persist_water(vals,indicator_data)

  fluctu4 <- surface_water_fluctuation(vals, indicator_data)

  depthdom4 <- predom_depth_class(vals, indicator_data)

  pondpct4 <- ponded_water(vals, indicator_data)

  widthwet4 <- if(any(unlist(vals[c("NeverWater", "NoPersis", "NoDeepPond", "NoOW")]) == 1)) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }

  interspers4 <- if(any(unlist(vals[c("NeverWater", "NoPersis", "NoDeepPond", "NoOW")]) == 1)) {
    NA_real_
  } else {
    wt_max(indicator_data, "F35")
  }

  eutroph4 <- if(vals$F38_2 + vals$F38_3 > 0){
    NA_real_
  } else {
    0
  }

  outdura4 <- if(vals$F40_4 + vals$F40_5 > 0) {
    outmap4
  } else {
    wt_max(indicator_data, "F40")
  }

  constric4 <- outflow_confinement_1(vals, indicator_data)

  thruflo4 <- throughflow_resistance(vals, indicator_data)

  gradient4 <- internal_gradient(vals, indicator_data)

  acid4 <- if(vals$F45_1 > 8) {
    1
  } else {
    NA_real_
  }

  # TODO - check this one : on questions list

  soildisturb4 <- 1 - vals$S5_subscore

  ## calculate function sub-components
  interceptdry3 <- sum_na(mean_na(c(gradient4,wetpctrca4)),
                          mean_na(c(girreg4 , gcover4, soildisturb4, aspect4, growd4)))/2

  interceptwet3 <- if(any(unlist(vals[c("NeverWater", "NoOW")]) == 1)) {
    NA_real_
  } else {
    sum_na(pondpct4, interspers4, thruflo4, widthwet4, flodist4)/5
  }

  connec4 <- sum_na(outdura4, constric4)/2

  adsorb3 <- sum_na(soiltex4, acid4)/2

  desorb3 <- sum_na(permw4, depthdom4, fluctu4, eutroph4)/4


  pr_fun_score <- 10* (ifelse(vals$NoOutlet || vals$NoOutletX == 1, 1,
                              ifelse(vals$NeverWater == 1, mean_na(c(interceptdry3, adsorb3), na.rm = TRUE),
                                     (3 * adsorb3 + 2 * mean_na(c(connec4, desorb3), na.rm = TRUE) +
                                        mean_na(c(interceptwet3, interceptdry3), na.rm = TRUE)) / 6
                              )))

  as.indicator_score(
    pr_fun_score,
    subscores = c(
      interceptdry = interceptdry3,
      interceptwet = interceptwet3,
      connec = connec4,
      adsorb = adsorb3,
      desorb = desorb3
    )
  )
}


# benefit

pr_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "pr", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev4v <- 1 - vals$OF5_1

  wetpctca4v <- wt_max(indicator_data, "OF11")

  impervsca4v <- unveg_surface(vals, indicator_data)

  dryness4v <- local_moisture_deficit(vals)

  sindex4v <- if(sum_na(vals$OF28_1, vals$OF28_2,vals$OF28_3,vals$OF28_4,vals$OF28_5)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF28")
  }

  topopos4v <- vals$OF29_1 / 5

  rddens4v <-if(sum_na(vals$OF30_1, vals$OF30_2,vals$OF30_3)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF30")
  }

  disturb4v <- if(sum_na(vals$OF41_1, vals$OF41_2,vals$OF41_3,vals$OF41_4,vals$OF41_5) ==0 ||
                  vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF41")
  }

  rddenswau4v <-  road_density_wau(vals, indicator_data)

  inflow4v <- if (vals$NoOutlet + vals$NoOutletX > 0) {
    NA_real_
  } else {
    vals$F42_1
  }

  # check these are NA and not blanks
  conductiv4v <- ifelse(is.na(vals$F46a_1) , NA_real_ ,
                        ifelse(vals$F46a_1 < 150, 0,
                               ifelse(vals$F46a_1 > 500, 1, 0.5)))

  # check these are NA and not blanks
  tds4v <- ifelse(is.na(vals$F46b_1), NA_real_ ,
                  ifelse(vals$F46b_1 < 100, 0,
                         ifelse(vals$F46b_1 > 350, 1, 0.5)))


  perminpectper4v <- vegetation_buffer_along_permin(vals, indicator_data)


  imperv4v <- type_of_cover_buff(vals, indicator_data)

  slopebuff4v <- buffer_slope(vals, indicator_data)

  nutrload4v <- vals$S2_subscore

  pr_ben_score <- 10 * (
    3 * mean_na(c(nutrload4v, conductiv4v, tds4v, sindex4v)) +
      mean_na(c(rddenswau4v, rddens4v, disturb4v, imperv4v, slopebuff4v, perminpectper4v))+
      mean_na(c(inflow4v, dryness4v))+
      mean_na(c(wetpctca4v, elev4v, topopos4v)))/6

  as.indicator_score(pr_ben_score)

}

