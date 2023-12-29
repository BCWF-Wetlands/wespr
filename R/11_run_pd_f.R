
pf_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "pd")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distown15 <- wt_max(indicator_data, "OF1", "fun")

  distrd15 <- wt_max(indicator_data, "OF2", "fun")

  outmap15 <- vals$OF6_1

  imperv15 <- wt_max(indicator_data, "OF12", "fun")

  karst15 <- vals$OF16_1

  wetvegarea15 <- wt_max(indicator_data, "OF19", "fun")

  refuge15 <- ifelse(vals$OF21_1 == 1, 1, NA_real_)

  protect15 <- vals$OF22_1

  rarespp15 <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/49

  rddens15 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF30", "fun")
  }

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/49
  intact15 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32", "fun")
  }


  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/49
  oldgro15 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF33", "fun")
  }

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/49
  disturbwau15 <- if(sum_na (XXXXXXX) == 0 ||
                     vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF41", "fun")
  }


  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/49
  rddenswau15 <- if(sum_na (XXXXXXX) == 0 ||
                    vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF42", "fun")
  }


  woodyformrich15 <- max_na(
    sum(c(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6) > 0) / 6,
    (vals$F1_2 + vals$F1_4) / 8
  )

  # todo : missing calculation entirely
  #https://github.com/BCWF-Wetlands/wespr/issues/49

 # woodyhtmix15 <- XXXX


  shrubrich15 <- if(sum_na(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "F4", "fun")
  }

  forb15 <- wt_max(indicator_data, "F11", "fun")

  tussock15 <- wt_max(indicator_data, "F12", "fun")

  invas15 <- wt_max(indicator_data, "F13", "fun")

  girreg15 <- wt_max(indicator_data, "F18", "fun")

  seaspct15 <- wt_max(indicator_data, "F20", "fun")

  drypct15 <- wt_max(indicator_data, "F19", "fun")

  depthdom15 <- predom_depth_class_2(vals, indicator_data)

  widthwet15 <- distance_open_water_upland_veg_2(vals, indicator_data)

  aqveg15 <-  submerged_floating_aquatics_1(vals)

  inflow15 <- if(vals$NoOutlet + vals$NoOutletX > 0){
        max_na(outmap15, vals$F42_1)
      } else {
        vals$F42_1
     }

  groundw15 <- if(vals$F47_3 == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F47", "fun")

  }

  beaver15 <- wt_max(indicator_data, "F48", "fun")

  perimpctper15 <- if(vals$Disturb == 0 ){
    1
  } else {
    wt_max(indicator_data, "F50", "fun")
  }

  firehist15 <- wt_max(indicator_data, "F55", "fun")

  rareplant15 <- if_else(vals$F58_7 == 1, 1, NA_real_)


  # to do : check this calculation as it reference wrong cell?
  # https://github.com/BCWF-Wetlands/wespr/issues/49

   plantrich15 <- if(vals$F59_1 == 0){
    NA_real_
   } else { if((vals$F59_1/8) > 1){
    1 } else {
      (vals$F59_1/8)
    }
   }

   alttiming15 <- vals$S1_subscore

   sedrca15 <- vals$S4_subscore

   soildisturb15a <- vals$S5_subscore

   #todo: find a nice way to extract the subscores
   #https://github.com/BCWF-Wetlands/wespr/issues/48
  # appscore15 <- ind_scores?????


   # function subscores

   spparea <- mean_na(widthwet15, drypct15, seaspct15)

   vrichness <- max_na(mean_na(woodyformrich15, woodyhtmix15, shrubrich15, forb15),
                       plantrich15,
                       max_na(rareplant15, rarespp15))

   aqfertilpd <- max_na(appscore15,
                        mean_na(inflow15, karst15, groundw15, beaver15, girreg15, aqveg15, firehist15))

   vscape <- mean_na(perimpctper15, intact15, oldgro15, wetvegarea15)

   stresspd <- ((2 * invas15) + mean_na(distrd15, max_na(protect15, refuge15),
                                       alttiming15, sedrca15, soildisturb15a, imperv15, distown15))/3



   # TO do: reference to invashigh? not sure where this is ?
   #https://github.com/BCWF-Wetlands/wespr/issues/48

   pd_fun_score <- 10 * if(invashigh == 1){
            0
          } else { mean_na(spparea, plantrich15, vrichness, vscape,
                    max_na(rareplant15, rarespp15),
                    stresspd, aqfertilpd) }

   pd_fun_score

  }










}
