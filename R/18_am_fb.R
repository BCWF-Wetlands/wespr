
am_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "am")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distrd11 <- wt_max(indicator_data, "OF2", "fun")

  distpond11 <- wt_max(indicator_data, "OF3", "fun")

  aspect11 <- wt_max(indicator_data, "OF7", "fun")

  imperv11 <- wt_max(indicator_data, "OF12", "fun")

  wetpct2k <- wt_max(indicator_data, "OF19", "fun")

  fishacc11 <- ifelse(vals$OF20_5 == 1, 1, NA_real_)

  raream11 <- ifelse(vals$OF24_2 == 1, 1, NA_real_)

  ddays11 <- degree_days_index(vals)

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57
  rddens11 <- if(sum_na(intact_vals) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF30", "fun")
    }

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57
  rddenswau11 <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF31", "fun")
  }

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57
  intact11 <-  if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32", "fun")
  }

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57
  oldgro11 <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF33", "fun")
  }

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57
  typerich11 <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36", "fun")
  }

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57
  wetdenswau11 <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43", "fun")
  }

  # check this is correct ?
 #https://github.com/BCWF-Wetlands/wespr/issues/57
  wooddown11 <- if(sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F9", "fun")
  }

  gcover11 <- if(vals$F15_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F15", "fun")
  }

  girreg11 <- wt_max(indicator_data, "F18", "fun")

  permwpct11 <- if (vals$NeverWater + vals$TempWet > 0) {
      NA_real_
    } else {
      wt_max(indicator_data, "F21", "fun")
    }

  fluctu11 <- surface_water_fluctuation(vals, indicator_data)

  lentic11 <- ponded_water(vals, indicator_data)

  woodover11 <- non_veg_aquatic_cover(vals, indicator_data)


  # TO DO : Check value exists in weights table
  deepspot11 <- largest_deep_pond(vals, indicator_data)

  widthwet11 <- distance_open_water_upland_veg_4(vals, indicator_data)

  interspers11 <- interspersion_inundated_veg(vals, indicator_data)

  empct11 <- inundated_erect_veg(vals, indicator_data)

  sav11 <- submerged_floating_aquatics_2(vals, indicator_data)

  gradient11 <- internal_gradient(vals, indicator_data)



  pH <- ifelse(is.na(vals$F45_1), NA_real_, vals$F45_1)

  acidic11 <- ifelse(pH  == 1, NA_real_,
                   ifelse(vals$F45_3 == 1, NA_real_,
                          ifelse( vals$F45_2 == 1, 0.2,
                                 ifelse(pH  > 7.5, 1,
                                        ifelse(pH  > 5 & pH  < 6.5, 0.5, 0)))))


  beaver11 <- wt_max(indicator_data, "F48", "fun")

  perimpctper11 <- vegetation_buffer_along_permin(vals, indicator_data, "fun")

  amrare11 <- ifelse(vals$F58_8 == 1, 1, NA_real_)

  sedca11 <- vals$S4_subscore

  ## TO DO - check how to add a final score

  appscore10 <-


  # function subscores

  waterscape11 <- mean_na(wetpct2k, distpond11,  wetdenswau11)

  hydro11 <- (mean_na(lentic11, gradient11, appscore10) +
              mean_na(deepspot11, beaver11, permwpct11))/2

  aqstruc11 <- if(vals$NeverWater == 1){
    NA_real_
  } else {
    mean_na(widthwet11, max_na(empct11, sav11, woodover11), interspers11)
  }

  terrstruc11 <- mean_na(aspect11, gcover11, girreg11, wooddown11, perimpctper11, intact11, oldgro11, typerich11)

  biostress11 <- mean_na(fishacc11, sedca11, distrd11, fluctu11, rddens11, rddenswau11, imperv11, ddays11, acidic11)


  am_fun_score <- 10 * mean_na(waterscape11, raream11, amrare11, mean_na(hydro11, biostress11, aqstruc11, terrstruc11))

  am_fun_score

}

# benefits score


am_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "am")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distpond9v <- wt_max(indicator_data, "OF3", "ben")

  water2k11v <- wt_max(indicator_data, "OF19", "ben")

  raream11v <- ifelse(vals$F24_2 == 1, 1, NA_real_)

  # check the range is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/57

  wetdenswau11v <- if(sum_na(intact_vals) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF43", "fun")
    }

  amphrare11 <- ifelse(vals$F58_8 == 1, 1, NA_real_)

  # find way to extract the correct fomat for fun result
 # fscorewbf11v <-

  am_ben_score <- 10 * (max_na(amphrare11, raream11v, fscorewbf11v,
                               mean( distpond9v, water2k11v, wetdenswau11)))

  am_ben_score
}
