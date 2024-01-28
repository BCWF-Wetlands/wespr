app_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "app", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev8 <- 1 - vals$OF5_1


  if (vals$NoOutlet + vals$NoOutletX == 0) {
    mappedout8  <- outdur8 <- wt_max(indicator_data, "F40")
  } else {
    wt_max(indicator_data, "F40")
    mappedout8  <- outdur8 <- vals$OF6_1
  }

#
#   # TODO: SOmthing not correct here : might need to addd an ifelse as circular with these two formulars
#   mappedout8 <- if(vals$NoOutlet + vals$NoOutletX == 0){
#     outdur8
#   } else {
#     vals$OF6_1
#   }
#
#   # TO DO : check this formlar makes sense in order with other formulars
#   outdur8 <- if(vals$F40_4 + vals$F40_5 > 0) {
#     mappedout8
#   } else {
#     wt_max(indicator_data, "F40")
#   }


  aspect8 <- wt_max(indicator_data, "OF7")

  wetpctca8 <- wt_max(indicator_data, "OF11")

  unvegca8 <- wt_max(indicator_data, "OF12")


  karst8 <- if(vals$OF16_1 == 1){
      1
    } else {
      NA_real_
    }

  anadf7 <- fish_occurance(vals)

  wetdef8 <- local_moisture_deficit(vals)

  degreed8 <- degree_days_index(vals)

  solar8 <- local_solar_input(vals)

  sindex8 <- if(sum_na(vals$OF28_1,vals$OF28_2,vals$OF28_3,vals$OF28_4,vals$OF28_5) == 0){
     NA_real_
    } else {
      wt_max(indicator_data, "OF28")
    }


  decid8 <- if(sum_na(vals$OF38_1,vals$OF38_2,vals$OF38_3,vals$OF38_4,vals$OF38_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF38")
  }


  disturb8 <- if(sum_na(vals$OF41_1,vals$OF41_2,vals$OF41_3,vals$OF41_4,vals$OF41_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF41")
  }


  decidtree8 <-(max_na(c(weights$WF1_2, weights$WF1_4, weights$WF1_6))/30 +
                  (sum_na(vals$F1_2, vals$F1_4, vals$F1_6)/10))/2


 nfix8 <- wt_max(indicator_data, "F14")

 gcover8 <- ground_cover(vals, indicator_data)

 soiltex8 <- wt_max(indicator_data, "F17")

 drypct8 <- wt_max(indicator_data, "F19")

 seaspct8 <- percent_flooded_only_seasonally(vals, indicator_data)

 shade8 <- percent_summerwater_shaded(vals, indicator_data)

 fluctu8 <- surface_water_fluctuation(vals, indicator_data)

 depthdom8 <- predom_depth_class(vals, indicator_data)

 interspers8 <- interspersion_inundated_veg_1(vals, indicator_data)

 emarea8 <- inundated_erect_veg(vals, indicator_data)

 savpct8 <- submerged_floating_aquatics(vals, indicator_data)

 color8 <- water_color(vals, indicator_data)

  inflow8 <- vals$F42_1

  pH <- ifelse(is.na(vals$F45_1), NA_real_, vals$F45_1)
  acidic8 <- dplyr::case_when(
    vals$F45_3 == 1 ~ NA,
    vals$F45_2 == 1 ~ 0.2,
    is.na(pH) ~ NA,
    pH > 5 & pH < 7.5 ~ 0.5,
    pH >= 7.5 ~ 1,
    .default = 0
  )


  # check these are NA and not blanks
  conductiv8 <- ifelse(is.na(vals$F46a_1), NA_real_ ,
                        ifelse(vals$F46a_1 < 150, 0,
                               ifelse(vals$F46a_1 > 500, 1, 0.5)))

  # check these are NA and not blanks
  tdsapp8 <- ifelse(is.na(vals$F46b_1), NA_real_ ,
                  ifelse(vals$F46b_1 < 100, 0,
                         ifelse(vals$F46b_1 > 350, 1, 0.5)))


  groundw8 <- wt_max(indicator_data, "F47")

  beaver8 <- wt_max(indicator_data, "F48")

  fire8 <- if(vals$F55_7 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F55")
  }

  nutrin8 <- vals$S3_subscore

  sedrca8 <- 1 - vals$S4_subscore

  soildisturb8 <- 1 - vals$S5_subscore



  # calculate the subscores

  npinput8 <- mean_na(c(conductiv8, tdsapp8,
                      mean_na(karst8, nutrin8, anadf7, fire8, sindex8, nfix8),
                      mean_na(inflow8, wetpctca8, groundw8)))

  npcycling8 <- mean_na(c(fluctu8, seaspct8, soiltex8, color8,
                        max_na(c(mappedout8, outdur8)),
                        interspers8, sindex8))

  templight8 <- mean_na(c(aspect8, solar8, wetdef8, degreed8, shade8, decidtree8, gcover8, depthdom8
                        ))

  stressors8<- mean_na(c(soildisturb8, unvegca8, disturb8, sedrca8, acidic8))

  ap_fun_score <- 10 * (drypct8 * mean_na(c(npinput8, npcycling8, templight8, stressors8)))

  ap_fun_score

}
