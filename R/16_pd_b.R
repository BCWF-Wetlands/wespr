
pd_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "pd")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  pctprotected15v <- wt_max(indicator_data, "OF23", "ben")

  rarespp15v <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  # to do : check this is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/54

  lcovuniq15v <- if(sum_na(intact_vals) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF34", "ben")
    }

  lcmaxdom15v <- ifelse(sum_na(intact_vals)== 0 , NA_real_, 1 - vals$OF35_1)

  lcovrich15v <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36", "ben")
  }

  lcrich2k15v <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37", "ben")
  }

  berry15v <- wt_max(indicator_data, "F7", "ben")

  plantcollect15 <- 1 - vals$F57_2

  rareplant15v <- ifelse(vals$F58_7 == 1, 1, NA_real_)

  ## TODO find way to extract score elequantly
  pollscore15 <- XXX / 10

  ## TODO find way to extract score elequantly
  sbmscore15v <-  XXX / 10


  pd_ben_score <-  10 * ifelse(max( rareplant15v, rarespp15v) == 1, 1,
                        (mean(c(berry15v, plantcollect15)) +
                           mean(c(sbmscore15vm, pollscore15)) +
                           mean(c(lcovuniq15v, lcovrich15v, lcrich2k15v, pctprotected15v)))/3)


 pd_ben_score


}
