
pd_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "pd")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  pctprotected15v <- wt_max(indicator_data, "OF23", "ben")

  rarespp15v <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  lcovuniq15v <- if(sum_na( vals$OF34_1, vals$OF34_2, vals$OF34_3) == 0){
      NA_real_
    } else {
      wt_max(indicator_data, "OF34", "ben")
    }

  lcmaxdom15v <- ifelse(vals$OF35_1 ==0 , NA_real_, 1 - vals$OF35_1)

  lcovrich15v <- if(sum_na(vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36", "ben")
  }

  lcrich2k15v <- if(sum_na( vals$OF37_1, vals$OF37_2, vals$OF37_3, vals$OF37_4, vals$OF37_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37", "ben")
  }

  berry15v <- wt_max(indicator_data, "F7", "ben")

  plantcollect15 <- 1 - vals$F57_2

  rareplant15v <- ifelse(vals$F58_7 == 1, 1, NA_real_)

  ## TODO find way to extract score elequantly
  pollscore15 <- 1 #XXX / 10

  ## TODO find way to extract score elequantly
  sbmscore15v <-1 #  XXX / 10


  pd_ben_score <-  10 * ifelse(max( rareplant15v, rarespp15v) == 1, 1,
                        (mean(c(berry15v, plantcollect15)) +
                           mean(c(sbmscore15vm, pollscore15)) +
                           mean(c(lcovuniq15v, lcovrich15v, lcrich2k15v, pctprotected15v)))/3)


 pd_ben_score


}
