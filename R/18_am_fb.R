
am_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "am", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distrd11 <- wt_max(indicator_data, "OF2")

  distpond11 <- wt_max(indicator_data, "OF3")

  aspect11 <- wt_max(indicator_data, "OF7")

  imperv11 <- wt_max(indicator_data, "OF12")

  wetpct2k <- wt_max(indicator_data, "OF19")

  fishacc11 <- ifelse(vals$OF20_5 == 1, 1, NA_real_)

  raream11 <- ifelse(vals$OF24_2 == 1, 1, NA_real_)

  ddays11 <- degree_days_index(vals)

  rddens11 <- if(sum_na(vals$OF30_1, vals$OF30_2, vals$OF30_3) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF30")
    }

  rddenswau11 <- if(sum_na(vals$OF31_1, vals$OF31_2, vals$OF31_3) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF31")
  }

  intact11 <-  if(sum_na(vals$OF32_1, vals$OF32_2, vals$OF32_3, vals$OF32_4, vals$OF32_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32")
  }

  oldgro11 <- if(sum_na(vals$OF33_1, vals$OF33_2, vals$OF33_3, vals$OF33_4, vals$OF33_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF33")
  }

  typerich11 <- if(sum_na(vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36")
  }


  wetdenswau11 <- if(sum_na(vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43")
  }


  wooddown11 <- if(sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F9")
  }

  gcover11 <- if(vals$F15_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F15")
  }

  girreg11 <- wt_max(indicator_data, "F18")

  permwpct11 <- if (vals$NeverWater + vals$TempWet > 0) {
      NA_real_
    } else {
      wt_max(indicator_data, "F21")
    }

  fluctu11 <- surface_water_fluctuation(vals, indicator_data)

  lentic11 <- ponded_water(vals, indicator_data)

  woodover11 <- non_veg_aquatic_cover(vals, indicator_data)

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


  beaver11 <- wt_max(indicator_data, "F48")

  perimpctper11 <- vegetation_buffer_along_permin(vals, indicator_data)

  amrare11 <- ifelse(vals$F58_8 == 1, 1, NA_real_)

  sedca11 <- 1 - vals$S4_subscore

  appscore10 <- get_indicator_score(site, "app", "fun") / 10

  # function subscores

  waterscape11 <- mean_na(c(wetpct2k, distpond11,  wetdenswau11))

  hydro11 <- (mean_na(c(lentic11, gradient11, appscore10)) +
              mean_na(c(deepspot11, beaver11, permwpct11)))/2

  aqstruc11 <- if(vals$NeverWater == 1){
    NA_real_
  } else {
    mean_na(c(widthwet11, max_na(c(empct11, sav11, woodover11)), interspers11))
  }

  terrstruc11 <- mean_na(c(aspect11, gcover11, girreg11, wooddown11, perimpctper11, intact11, oldgro11, typerich11))

  biostress11 <- mean_na(c(fishacc11, sedca11, distrd11, fluctu11, rddens11, rddenswau11, imperv11, ddays11, acidic11))

  am_fun_score <- 10 * mean_na(c(waterscape11, max_na(c(raream11, amrare11)), mean_na(hydro11, biostress11, aqstruc11, terrstruc11)))

  as.indicator_score(
    am_fun_score,
    subscores = c(
      waterscape = waterscape11,
      hydro = hydro11,
      aqstruc = aqstruc11,
      terrstruc = terrstruc11,
      biostress = biostress11
    )
  )

}

# benefits score


am_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "am", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distpond9v <- wt_max(indicator_data, "OF3")

  water2k11v <- wt_max(indicator_data, "OF19")

  raream11v <- ifelse(vals$F24_2 == 1, 1, NA_real_)

  wetdenswau11v <- if(sum_na(vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF43")
    }

  amphrare11 <- ifelse(vals$F58_8 == 1, 1, NA_real_)

  fscorewbf11v <- get_indicator_score(site, "wb", "fun") / 10

  am_ben_score <- 10 * (max_na(c(amphrare11, raream11v, fscorewbf11v,
                               mean_na(c(distpond9v, water2k11v, wetdenswau11v)))))

  as.indicator_score(am_ben_score)
}
