
cri_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "cri")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)
  ind_scores <- get_indicator_scores(site)

  disttown17v <- wt_max(indicator_data, "OF1", "ben")

  distrd17v <- wt_max(indicator_data, "OF2", "ben")

  consinvest17 <- vals$OF13_1

  sciuse17 <- vals$OF14_1

  # check this value as reference 0F18_0 and not value (
  #https://github.com/BCWF-Wetlands/wespr/issues/61

  bylakes17 <- 1 - vals$OF18_1 # check val?

  fish10 <- fish_occurance(vals)

  refuges <- vals$OF21_1

  protect17 <- wt_max(indicator_data, "OF23", "ben")

  rddens17 <-  if(sum_na(vals$OF30_1, vals$OF30_2, vals$OF30_3) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF30", "ben")
    }

  rddens2k17 <- if(sum_na(vals$OF31_1, vals$OF31_2, vals$OF31_3) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF31", "ben")
  }

  fringe10 <- if(vals$NeverWater == 1 ||
                 vals$NoPersis == 1 ||
                 vals$F22_1 ==0 ){
    NA_real_
  } else {
    1
  }

  lake17 <- if(vals$NeverWater == 1 ||
               vals$NoPersis == 1){
    NA_real_
  } else {
    vals$F23_1
  }

  openw17 <- if(vals$NeverWater == 1 ||
                vals$NoPersis == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF31", "ben")
  }

  algae17 <- ifelse(vals$F56_1 == 0 , 0, NA_real_)

  wells17v <- wt_max(indicator_data, "F54", "ben")

  recpoten17v <- sum_na(vals$F56_1, vals$F56_2, vals$F56_3 )/3

  consumpu10 <- (max_na(c(vals$F57_1, vals$F57_2,vals$F57_3,vals$F57_4,vals$F57_5,vals$F57_6)) +
    (sum(vals$F57_1, vals$F57_2,vals$F57_3,vals$F57_4,vals$F57_5,vals$F57_6)/6))/2

  fhscore17 <- site$indicators$fh$fun

  wbscore17 <-  site$indicators$wb$fun


  cri_ben_score <- 10 * (2 * max_na(c(fish10, consumpu10, algae17, wells17v)) +
                    mean_na(c(disttown17v, distrd17v, rddens17, rddens2k17)) +
                    mean_na(c(bylakes17, lake17, openw17, fringe10)) +
                    mean_na(c(refuges, protect17, wbscore17, consinvest17, sciuse17)) +
                    mean_na(c(recpoten17v, fhscore17))) / 6


  cri_ben_score

}
