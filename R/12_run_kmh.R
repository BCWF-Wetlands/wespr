kmh_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "kmh")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  distrd19 <- wt_max(indicator_data, "OF2", "fun")

  distpond19 <- wt_max(indicator_data, "OF3", "fun")

  water2k19 <- wt_max(indicator_data, "OF19", "fun")

  fish19 <- fish_occurance(vals)

  growdays19 <- degree_days_index(vals)

  # check the range for these questions
 # https://github.com/BCWF-Wetlands/wespr/issues/50
  rddens19 <-  if(sum_na (XXXXXXX) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF31", "fun")
    }

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/50
  intact19 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32", "fun")
  }


  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/50
  oldgro19 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF33", "fun")
  }

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/50
  covdiv19 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36", "fun")
  }
  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/50
  covdiv2k19 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37", "fun")
  }

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/50

  decid19 <- if(sum_na (XXXXXXX) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF38", "fun")
  }

  # check this calculator with Paul
  # https://github.com/BCWF-Wetlands/wespr/issues/50

    conif19 <- if(sum_na (XXXXXXX) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF39", "fun")
    }

  decidon19 <- if_else((sum_na(vals$F1_2, vals$F1_4, vals$F1_6)/8)>1, 1, (sum_na(vals$F1_2, vals$F1_4, vals$F1_6)/8))

  conifon19 <- if_else((sum(vals$F1_1, vals$F1_3)/8)>1, 1, (sum(vals$F1_1, vals$F1_3)/8))

  herb <- 1 - sum_na(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6)/14
  herbcov19 <- if_else(herb < 0, 0 , herb)


  # to do: this function is flagging an error on data inputs?
  # need to check weights table?
  woodyhtmix19 <- wt_max(indicator_data, "F2", "fun")

   # up to here


}
