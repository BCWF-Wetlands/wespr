nr_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "nr", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev5 <- 1 - vals$OF5_1

  outmap5 <- stream_intersect(vals)

  aspect5 <- wt_max(indicator_data, "OF7")

  wetpctca5 <- wt_max(indicator_data, "OF11")

  gdd5 <- degree_days_index(vals)

  woodypct5 <- max(vals$F1_2, vals$F41_4)/6

  gcover5 <- ground_cover(vals, indicator_data)

  soiltex5 <- wt_max(indicator_data, "F17")

  girreg5 <- wt_max(indicator_data, "F18")

  drypct5 <- wt_max(indicator_data, "F19")

  seaspct5 <- percent_flooded_only_seasonally(vals, indicator_data)

  permw5 <- if(vals$NeverWater + vals$TempWet > 0 ) {
      NA_real_
    } else {
      wt_max(indicator_data, "F21")
    }

  fluctu5 <- surface_water_fluctuation(vals, indicator_data)

  ponded5 <- if(vals$NeverWater + vals$TempWet > 0){
    NA_real_
    } else {
    wt_max(indicator_data, "F27")
    }

  widthwet5 <- distance_open_water_upland_veg_1(vals, indicator_data)

  watermixwet5 <-interspersion_inundated_veg(vals, indicator_data)

  outdura5 <- if ((vals$F40_4 + vals$F40_5) > 0) {
      outmap5
    } else {
      wt_max(indicator_data, "F40")
    }

  constric5 <- outflow_confinement(vals, indicator_data)

  thruflo5 <- throughflow_resistance(vals, indicator_data)

  gradient5 <- internal_gradient(vals, indicator_data)

  pH <- ifelse(is.na(vals$F45_1), NA_real_, vals$F45_1)

  acid5 <- dplyr::case_when(
    vals$F45_3 == 1 ~ NA,
    is.na(pH)|| pH ==0~ NA,
    pH >= 6 & pH <= 8.5 ~ 1,
    TRUE ~ ifelse(pH < 6 | pH > 8.5,  0, 0.5)
  )

  groundw5 <- wt_max(indicator_data, "F47")

  soildisturb5 <- 1-vals$S5_subscore

  # function subscores

  warmth4 <- mean_na(c(woodypct5, gdd5, elev5, aspect5, groundw5))

  intercept4 <- (2 * mean_na(c(ponded5, gradient5, wetpctca5))+
                   mean_na(c(widthwet5, gcover5, watermixwet5, thruflo5, girreg5)))/3

  connec5 <- mean_na(c(outdura5, constric5))

  organic4 <- mean_na(soiltex5, soildisturb5)

  redox <- mean_na(seaspct5, acid5, mean_na(fluctu5, permw5, drypct5))

  nr_subscore_fun <- 10 * (ifelse(vals$NoOutlet || vals$NoOutletX == 1, 1,
                                  ifelse(vals$NeverWater == 1, mean_na(warmth4, organic4),
                 (3 * redox + 2 * connec5 + warmth4 + organic4 + intercept4) / 8)
                                         ))


as.indicator_score(
  nr_subscore_fun,
  subscores = c(
    warmth = warmth4,
    intercept = intercept4,
    connecc = connec5,
    organic = organic4,
    redox = redox
  )

)


}


# benefit score

nr_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "nr", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  rddist5 <- wt_max(indicator_data, "OF2")

  nfix5v <- wt_max(indicator_data, "F14")

  wells5v <- wt_max(indicator_data, "F54")

  pval5 <- get_indicator_score(site, "pr", "ben") / 10

  nr_ben_score <- 10 * max_na(c(wells5v, pval5, mean_na(c(nfix5v, rddist5))))

  as.indicator_score(nr_ben_score)

}
