cp_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "cp", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  burn6 = 1 - vals$OF15_1

  wetdef6 <- 1 - local_moisture_deficit(vals)

  woodypct6 <- (
    (max_na(c(
      vals$F1_1 * weights$WF1_1,
      vals$F1_3 * weights$WF1_3,
      vals$F1_5 * weights$WF1_5
    )) / 18) +
      sum_na(vals$F1_1, vals$F1_3, vals$F1_5) / 10
  ) / 2

  treetyp6 <- wt_max(indicator_data, "F3")

  moss6 <- wt_max(indicator_data, "F10")

  gcover6 <- wt_max(indicator_data, "F15")

  soiltex6 <- wt_max(indicator_data, "F17")

  outdura6 <- if ((vals$F40_4 + vals$F40_5) > 0) {
    1
  } else {
    wt_max(indicator_data, "F40")
  }

  constric6 <- outflow_confinement(vals, indicator_data)

  # This should be NA or numeric
  pH <- vals$F45_1

  acidic6  <- dplyr::case_when(
    vals$F45_3 == 1 ~ NA,
    vals$F45_2 == 1 ~ 0.6,
    is.na(pH) || pH  ==0 ~ NA,
    pH > 0 & pH < 5 ~ 1,
    pH > 7.5 ~ 0,
    .default = 0.2
  )

  fire6 <- wt_max(indicator_data, "F55")

  soildisturb6 <- 1-vals$S5_subscore

  ## Overall CP score

  cp_fun_score <- 10 * (5 * max_na(c(soiltex6, moss6, acidic6)) +
          2 * outdura6 + woodypct6 +
          mean_na(c(treetyp6, fire6, burn6, gcover6, constric6, wetdef6, soildisturb6))) / 9

  as.indicator_score(cp_fun_score)
}
