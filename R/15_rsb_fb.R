
rsb_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "rsb", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  disrd14 <- wt_max(indicator_data, "OF2")

  lakewetpct14 <- wt_max(indicator_data, "OF19")

  anadfish14 <- fish_occurance(vals)

  preserve14 <- vals$OF21_1

  protect14 <- vals$OF22_1

  rarebird14 <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  intact14 <- wt_max(indicator_data, "OF32")

  maxdomlc14 <- 1 - vals$OF35_1

  lcrich14 <- wt_max(indicator_data, "OF36")

  lcrich2k14 <- wt_max(indicator_data, "OF37")

  wetdenswau14 <- if(sum_na (vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43")
  }

  woodyrich14 <- max_na(c(
    sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6 > 0) / 6,
    (vals$F1_2 + vals$F1_4) / 8)
  )

  woodhtmix14 <- wt_max(indicator_data, "F2")

  ndiams14 <- sum(vals$F3_1, vals$F3_2,vals$F3_3, vals$F3_4, vals$F3_5, vals$F3_6,vals$F3_7, vals$F3_8)/8

  shrubrich14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F4")
  }

  willow14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F5")
  }

  shrubflower14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F6")
  }

  berries14 <- if(sum(vals$F1_1, vals$F1_2,vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6,vals$F1_7, vals$F1_8)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F7")
  }

  snags14 <-  wt_max(indicator_data, "F8")

  forbcov14 <- wt_max(indicator_data, "F11")

  permw14 <- wt_max(indicator_data, "F21")

  drypct14 <- wt_max(indicator_data, "F19")

  fetch14 <- distance_across_longest_openwater_1(vals, indicator_data)

  widthwet14 <- distance_open_water_upland_veg_4(vals, indicator_data)

  interspers14 <- interspersion_inundated_veg(vals, indicator_data)

  beaver14 <- wt_max(indicator_data, "F48")

  perimpctper14 <- vegetation_buffer_along_permin(vals, indicator_data)

  nestdist14 <- wt_max(indicator_data, "F53")

  rarespp14 <- ifelse(vals$F58_9 == 1, 1, NA_real_)

  noise14 <- ifelse(is.na(vals$S6_subscore), NA_real_, 1 - vals$S6_subscore)

  appscore14 <- get_indicator_score(site, "app", "fun")

  pdscore14 <-get_indicator_score(site, "pd", "fun")


  # RSB subscores :
  hydrosize <- mean_na(c(permw14, drypct14, widthwet14, fetch14))
  struc14 <- mean_na(c(woodhtmix14, woodyrich14, shrubrich14, ndiams14))
  foods14 <- mean_na(max_na(c(pdscore14, appscore14)),
                     max_na(c(berries14, willow14, forbcov14, anadfish14, shrubflower14)),
                     mean_na(c(berries14, willow14, forbcov14, anadfish14, shrubflower14)))

  habs <- mean_na(c(snags14, beaver14, nestdist14, noise14))
  lscape14 <- mean_na(c(wetdenswau14, lakewetpct14, lcrich14, lcrich2k14, maxdomlc14, intact14, preserve14))
  nopred <- mean_na(c(disrd14, perimpctper14, protect14))

  rsb_fun_score <- 10 * ifelse(vals$AllWater == 1, 0,
                        ifelse(max(rarebird14, rarespp14) == 1, 1,
                               (2 * hydrosize  + mean(c(struc14, foods14, habs, lscape14, nopred))) / 3))

  as.indicator_score(
    rsb_fun_score,
    subscores = c(
      hydrosize = hydrosize,
      struc = struc14,
      foods = foods14,
      habs = habs,
      lscape = lscape14,
      nopred = nopred
    )
  )
}



# benfit score


rsb_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "rsb", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  lakewetpct14v <- wt_max(indicator_data, "OF19")

  rarespp14v <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  lcovuniq14v <- if(sum_na (vals$OF34_1, vals$OF34_2, vals$OF34_3) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF34")
    }

  lcrich14v <- if(sum_na (vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF36")
    }

  lcrich2k14v <- if(sum_na (vals$OF37_1, vals$OF37_2, vals$OF37_3, vals$OF37_4, vals$OF37_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37")
  }

  wetdenswau14v <- if(sum_na (vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF43")
  }

  recrea14v <- sum_na(vals$F56_1, vals$F56_2, vals$F56_3 )/3

  rarebird14v <- ifelse(vals$F58_10 == 1, 1, NA_real_)

  rsb_ben_score <- 10 * dplyr::case_when(
    rarebird14v == 1 ~ 1,
    rarespp14v == 1 ~ 1,
    TRUE ~  mean_na(c(lakewetpct14v, lcovuniq14v, lcrich14v, lcrich2k14v, recrea14v))
  )

  as.indicator_score(rsb_ben_score)
}
