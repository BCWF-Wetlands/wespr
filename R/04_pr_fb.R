
# function
pr_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "pr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  outmap4 <- if ((vals$NoOutlet + vals$NoOutletX) > 0) {
    1
  } else {
    vals$OF6_1
  }

  aspect4 <- wt_max(indicator_data, "OF7", "fun")

  # check this indicator
  wetpctrca4 <- wt_max(indicator_data, "OF11", "fun")


  flodist4 <- internal_flow_distance(vals, indicator_data)

  growd4 <- degree_days_index(vals)

  gcover4 <- ground_cover(vals, indicator_data)

  soiltex4 <- if(vals$F15_4 > 1) {
      NA_real_
    } else {
      wt_max(indicator_data, "F15", "fun")
    }

  girreg4 <- wt_max(indicator_data, "F18", "fun")

  permw4 <- persist_water(vals,indicator_data)

  fluctu4 <- surface_water_fluctuation(vals, indicator_data)

  depthdom4 <- predom_depth_class(vals, indicator_data)

  pondpct4 <- ponded_water(vals, indicator_data)

  widthwet4 <- if(any(unlist(vals[c("NeverWater", "NoPersis", "NoDeepPond", "NoOW")]) == 1)) {
      NA_real_
    } else {
      wt_max(indicator_data, "F33", "fun")
    }

  interspers4 <- if(any(unlist(vals[c("NeverWater", "NoPersis", "NoDeepPond", "NoOW")]) == 1)) {
    NA_real_
  } else {
    wt_max(indicator_data, "F35", "fun")
  }

  eutroph4 <- if(vals$F38_2 + vals$F38_3 > 0){
    NA_real_
  } else {
    0
  }

  outdura4 <- if(vals$F40_4 + vals$F40_5 > 0) {
    outmap4
  } else {
    wt_max(indicator_data, "F40", "fun")
  }

  # see issue https://github.com/BCWF-Wetlands/wespr/issues/17
  constric4 <- outflow_confinement_1(vals, indicator_data)

  thruflo4 <- throughflow_resistance(vals, indicator_data)

  gradient4 <- internal_gradient(vals, indicator_data)



  # TO do ; suspect this function is also not consistent with the ph calculations

   acid4 <- if(vals$F45_1 > 8) {
     1
   } else {
    NA_real_
   }

   # S5 - Soil or Sediment Alteration within the assessment area
   soildisturb4 <- vals$S5_subscore



   ## calculate function sub-components

   # TO DO - check this calculation
   interceptdry3 <- sum_na(mean_na(gradient4,wetpctrca4),
                           mean_na(girreg4 , gcover4, soildisturb4, aspect4, growd4))/2


   # TO DO - check this calculation also as values do not equal all values : might be due to ignoring NAs with average value.
   interceptwet3 <- if(any(unlist(vals[c("NeverWater", "NoOW")]) == 1)) {
     NA_real_
   } else {
     sum_na(pondpct4, interspers4, thruflo4, widthwet4, flodist4)/5
   }

   # TO do - check average calculations are correct when missing NAs https://github.com/BCWF-Wetlands/wespr/issues/38
   connec4 <- sum_na(outdura4, constric4)/2

   # TO do - check average calculations are correct when missing NAs https://github.com/BCWF-Wetlands/wespr/issues/38
   adsorb3 <- sum_na(soiltex4, acid4)/2

   # TO do - check average calculations are correct when missing NAs https://github.com/BCWF-Wetlands/wespr/issues/38
   desorb3 <- sum_na(permw4, depthdom4, fluctu4, eutroph4)/4


   # check this score as does not match
   pr_fun_score <- ifelse((vals$NoOutlet + vals$NoOutletX) > 0, 1,
                   ifelse(vals$NeverWater == 1, mean(c(interceptdry3, adsorb3), na.rm = TRUE),
                    (3 * adsorb3 + 2 * mean(c(connec4, desorb3), na.rm = TRUE) +
                       mean(c(interceptwet3, interceptdry3), na.rm = TRUE)) / 6
                               ))



}


# benefit
pr_ben <- function(site) {
  indicator_data <- get_indicator_data(site, "pr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # ...
}
