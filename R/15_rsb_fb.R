
rsb_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "rsb")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  disrd14 <- wt_max(indicator_data, "OF2", "fun")

  lakewetpct14 <- wt_max(indicator_data, "OF19", "fun")

  anadfish14 <- fish_occurance(vals)

  preserve14 <- vals$OF21_1

  protect14 <- vals$OF22_1

  rarebird14 <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  intact14 <- wt_max(indicator_data, "OF32", "fun")

  maxdomlc14 <- 1 - vals$OF35_1

  lcrich14 <- wt_max(indicator_data, "OF36", "fun")

  lcrich2k14 <- wt_max(indicator_data, "OF37", "fun")

  wetdenswau14 <- if(sum_na (vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43", "fun")
  }

  woodyrich14 <- max_na(c(
    sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6 > 0) / 6,
    (vals$F1_2 + vals$F1_4) / 8)
  )

  # TO DO check this one - still not working
  woodhtmix14 <- wt_max(indicator_data, "F2", "fun")

  ndiams14 <- sum(vals$F3_1, vals$F3_2,vals$F3_3, vals$F3_4, vals$F3_5, vals$F3_6,vals$F3_7, vals$F3_8)/8

  shrubrich14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F4", "fun")
  }

  willow14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F5", "fun")
  }

  shrubflower14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F6", "fun")
  }

  berries14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F7", "fun")
  }

  snags14 <-  wt_max(indicator_data, "F8", "fun")

  forbcov14 <- wt_max(indicator_data, "F11", "fun")

  permw14 <- wt_max(indicator_data, "F21", "fun")

  drypct14 <- wt_max(indicator_data, "F19", "fun")

  fetch14 <- distance_across_longest_openwater_1(vals, indicator_data)

  widthwet14 <- distance_open_water_upland_veg_4(vals, indicator_data)

  interspers14 <- interspersion_inundated_veg(vals, indicator_data)

  beaver14 <- wt_max(indicator_data, "F48", "fun")

  perimpctper14 <- vegetation_buffer_along_permin(vals, indicator_data, "fun")

  nestdist14 <- wt_max(indicator_data, "F53", "fun")

  rarespp14 <- ifelse(vals$F58_9 == 1, 1, NA_real_)

  noise14 <- ifelse(is.na(vals$S6_subscore), NA_real_, 1 - vals$S6_subscore)

  appscore14 <- site$indicators$app$fun

  pdscore14 <-site$indicators$pd$fun


  # RSB subscores :
  hydrosize <- mean_na(c(permw14, drypct14, widthwet14, fetch14))
  stru14 <- mean_na(c(woodhtmix14, woodyrich14, shrubrich14, ndiams14))
  foods14 <- mean_na(max_na(c(pdscore14, appscore14)),
                     max_na(c(berries14, willow14, forbcov14, anadfish14, shrubflower14)),
                     mean_na(c(berries14, willow14, forbcov14, anadfish14, shrubflower14)))

  habs <- mean_na(c(snags14, beaver14, nestdist14, noise14))
  lscape14 <- mean_na(c(wetdenswau14, lakewetpct14, lcrich14, lcrich2k14, maxdomlc14, intact14, preserve14))
  nopred <- mean_na(c(disrd14, perimpctper14, protect14))

  rsb_fun_score <- 10 * ifelse(vals$AllWater == 1, 0,
                        ifelse(max(rarebird14, rarespp14) == 1, 1,
                               (2 * hydrosize  + mean(c(struc14, foods14, habs, lscape14, nopred))) / 3))

  rsb_fun_score
}



# benfit score


rsb_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "rsb")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  lakewetpct14v <- wt_max(indicator_data, "OF19", "ben")

  rarespp14v <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  lcovuniq14v <- if(sum_na (vals$OF34_1, vals$OF34_2, vals$OF34_3) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF34", "ben")
    }

  lcrich14v <- if(sum_na (vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF36", "ben")
    }

  lcrich2k14v <- if(sum_na (vals$OF37_1, vals$OF37_2, vals$OF37_3, vals$OF37_4, vals$OF37_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37", "ben")
  }

  wetdenswau14v <- if(sum_na (vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43", "ben")
  }

  recrea14v <- sum_na(vals$F56_1, vals$F56_2, vals$F56_3 )/3

  rarebird14v <- ifelse(vals$F58_10 == 1, 1, NA_real_)

  rsb_ben_score <- 10 * dplyr::case_when(
    rarebird14v == 1 ~ 1,
    rarespp14v == 1 ~ 1,
    TRUE ~  mean_na(c(lakewetpct14v, lcovuniq14v, lcrich14v, lcrich2k14v, recrea14v))
  )

  rsb_ben_score

}
