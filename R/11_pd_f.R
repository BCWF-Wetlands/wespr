
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

  rddens15 <- if(sum_na (vals$OF30_1, vals$OF30_2, vals$OF30_3) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF30", "fun")
  }

  intact15 <- if(sum_na (vals$OF32_1, vals$OF32_2, vals$OF32_3, vals$OF32_4, vals$OF32_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32", "fun")
  }

  oldgro15 <- if(sum_na (vals$OF33_1, vals$OF33_2, vals$OF33_3, vals$OF33_4, vals$OF33_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF33", "fun")
  }

  disturbwau15 <- if(sum_na (vals$OF41_1, vals$OF41_2, vals$OF41_3, vals$OF41_4, vals$OF41_5) == 0 ||
                     vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF41", "fun")
  }

  rddenswau15 <- if(sum_na ( vals$OF42_1, vals$OF42_2, vals$OF42_3) == 0 ||
                    vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF42", "fun")
  }

  woodyformrich15 <- max_na(c(
    sum(c(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6) > 0) / 6,
    (vals$F1_2 + vals$F1_4) / 8
  ))

  woodyhtmix15 <- wt_max(indicator_data, "F2", "fun")

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

  # check this
  aqveg15 <-  submerged_floating_aquatics_1(vals)

  inflow15 <- if(vals$NoOutlet + vals$NoOutletX > 0){
        max_na(c(outmap15, vals$F42_1))
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

   plantrich15 <- if(sum(vals$F59_1, vals$F59_2, vals$F59_3, vals$F59_4, vals$F59_5)  == 0){
    NA_real_
   } else {
     wt_max(indicator_data, "F59", "fun")
   }

   alttiming15 <- vals$S1_subscore

   sedrca15 <- vals$S4_subscore

   soildisturb15a <- vals$S5_subscore

   appscore15 <- site$indicators$app$fun

   # function subscores

   spparea <- mean_na(c(widthwet15, drypct15, seaspct15))

   vrichness <- max_na(c(mean_na(c(woodyformrich15, woodyhtmix15, shrubrich15, forb15)),
                       plantrich15,
                       max_na(c(rareplant15, rarespp15))))

   aqfertilpd <- max_na(c(appscore15,
                        mean_na(c(inflow15, karst15, groundw15, beaver15, girreg15, aqveg15, firehist15))))

   vscape <- mean_na(c(perimpctper15, intact15, oldgro15, wetvegarea15))

   stresspd <- ((2 * invas15) + mean_na(c(distrd15, max_na(c(protect15, refuge15))),
                                       alttiming15, sedrca15, soildisturb15a, imperv15, distown15))/3



   # TO do: reference to invashigh? not sure where this is ?
   ## needs more clarification on this.
   #https://github.com/BCWF-Wetlands/wespr/issues/48
   invashigh <- 0 # Place holder til invase high is ascertained (delete once this is clarified)

   pd_fun_score <- 10 * if(invashigh == 1){
            0
          } else { mean_na(c(spparea, plantrich15, vrichness, vscape,
                    max_na(c(rareplant15, rarespp15)),
                    stresspd, aqfertilpd)) }

   pd_fun_score

  }






