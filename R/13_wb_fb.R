
wb_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "wb", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  distpond13 <- dist_to_ponded_water(vals, indicator_data)

  distlake11 <- wt_max(indicator_data, "OF4")

  elev11 <- 1 - vals$OF5_1

  pctlakes11 <- wt_max(indicator_data, "OF18")

  lakewet11 <- wt_max(indicator_data, "OF19")

  fish11 <- ifelse(vals$OF20_5 == 1, 0 , NA_real_)

  wb_rare11 <- ifelse(vals$OF24_1 == 1, 0 , NA_real_)

  degreed11 <- degree_days_index(vals)

  herbscape13 <- if(sum_na (vals$OF40_1, vals$OF40_2, vals$OF40_3, vals$OF40_4, vals$OF40_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF40")
  }

  wetdensWAU13 <-  if(sum_na (vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43")
  }

  snags13 <- wt_max(indicator_data, "F8")

  mudflat13 <- wt_max(indicator_data, "F16")

  permwpct13 <- persist_water(vals, indicator_data)

  fringe13 <- if (vals$NeverWater == 1 ||
                  vals$NoPersis == 1||
                  vals$F22_1 == 0 ){
    NA_real_
  } else {
    1
  }

  lake13 <- if (vals$NeverWater == 1 ||
                vals$NoPersis == 1||
                vals$F23_1 == 0 ){
    NA_real_
  } else {
    1
  }

  pondedpct13 <- if (vals$NeverWater == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F27")
  }

  maxpondarea13 <- if((vals$NeverWater == 1) ||
       vals$NoPond == 1 ||
       vals$NoPersis == 1 ) {
      NA_real_
    } else {
      wt_max(indicator_data, "F29")
    }


  maxpondpct13 <- if((vals$NeverWater == 1) ||
                     vals$NoPond == 1 ||
                     vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F30")
  }


  owarea13 <- open_water_extent(vals, indicator_data)

  fetch13 <- distance_across_longest_openwater(vals, indicator_data)

  widthdry13 <- distance_open_water_upland_veg_3(vals, indicator_data)

  interspers13 <- interspersion_inundated_veg_1(vals, indicator_data)

  steepshore13 <- if(vals$NeverWater == 1 ||
                     vals$NoDeepPonded == 1 ||
                     vals$NoOW == 1 ||
                     vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F36")
  }

  cttail13 <- inundated_erect_veg(vals, indicator_data)

  sav13 <- submerged_floating_aquatics(vals, indicator_data)

  gradient13 <- internal_gradient(vals, indicator_data)

  beaver13 <- wt_max(indicator_data, "F48")

  perimcov13 <- vegetation_buffer_along_permin(vals, indicator_data)

  rarebird11 <- ifelse(vals$F58_9 == 1, 1, NA_real_)

  noise11 <- vals$S6_subscore

  appscore11 <- site$indicators$app$fun/10

  # function subscore

  lscape13 <- mean_na(c(perimcov13,
                      mean_na(c(distpond13, distlake11, lakewet11, pctlakes11, wetdensWAU13)),
                      elev11, noise11, herbscape13))
  hydro13 <- mean_na(c(gradient13, pondedpct13, permwpct13, lake13, degreed11))

  produc13 <- appscore11

  struc13 <- (mean_na(c(mudflat13, cttail13, sav13, interspers13)) +
             mean_na(c(maxpondpct13, fringe13, steepshore13, fetch13, widthdry13, fish11, beaver13, snags13)))/2


  wb_fun_score <- ifelse(vals$NeverWater == 1, 0,
                   10 * (3 * max_na(c(mean_na(c(pondedpct13, owarea13, maxpondarea13)), wb_rare11, rarebird11)) +
                                   2 * mean_na(c(hydro13, struc13, produc13, lscape13))) / 5)

  wb_fun_score


}


# benefits

wb_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "wb", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # TODO: Verify weighting of OF3_6 for WB. Currently just skipped
  # https://github.com/BCWF-Wetlands/wespr/issues/74
  distpond11v <- wt_max(indicator_data, "OF3")

  distlake11v <- wt_max(indicator_data, "OF4")

  lakepct11v <- wt_max(indicator_data, "OF18")

  lakewetpct11v <- wt_max(indicator_data, "OF19")

  rarespp11v <- ifelse(vals$OF24_3 == 1, 1, NA_real_)

  wetdenswau12v <- if(sum_na (vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF43")
    }

  recrea13v <- sum_na(vals$F56_1, vals$F56_2, vals$F56_3 )/3

  duckhunt13 <- ifelse(vals$F57_3 == 1, 1, NA_real_)

  rarebird11v <- ifelse(vals$F58_9 == 1, 1, NA_real_)

  wb_ben_score <- 10 * (max_na(c(mean_na(c(distpond11v, distlake11v, lakepct11v, wetdenswau12v)),
                               max_na(c(rarebird11v, rarespp11v)),
                               mean_na(c(duckhunt13, recrea13v)))))


  wb_ben_score

}

