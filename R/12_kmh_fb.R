#kestone mammal habitat.

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
  coverdiv19 <- if(sum_na (XXXXXXX) == 0){
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


  willow19 <- if(sum_na(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6) == 0){
    0
  } else {
    wt_max(indicator_data, "F5", "fun")
  }

  berries19 <- wt_max(indicator_data, "F7", "fun")

  moss19 <-  wt_max(indicator_data, "F10", "fun")

  permwat12 <- wt_max(indicator_data, "F21", "fun")


  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  lake19 <- ifelse(vals$F20_2 == 1, NA_real_, ifelse(vals$F23_1 == 1, 1, NA_real_ ))

  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  fluc19 <- if(vals$F20_2 == 1){
      NA_real_
  } else {
      wt_max(indicator_data, "F25", "fun") }

  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  depthdom19 <- if(vals$F20_2 == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F26", "fun")
    }

  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  deeppond19a <- if(vals$F20_2 == 1 ||
                    vals$NoPond == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F29", "fun")
  }

  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  vwidth19 <- if(vals$F20_2 == 1 ||
                vals$NoOW == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F33", "fun")
  }

  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50

  denscov19 <- if(vals$F20_2 == 1 ||
                  vals$NoOW == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F33", "fun")
  }

  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  emerg19 <- if(vals$F20_2 == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F37", "fun")
  }


  # check that the F20 is on the used by list for kmh
  #https://github.com/BCWF-Wetlands/wespr/issues/50

  aqplants19 <- if(vals$F20_2 == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F38", "fun")
  }

  beavers19 <- wt_max(indicator_data, "F48", "fun")

  buffalt19 <- ifelse(vals$F49_1 == 0, 1, NA_real_ )

  bufferpct19 <- if(vals$Disturb == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "F50", "fun")
  }

  fire19 <- wt_max(indicator_data, "F55", "fun")

  # to do : check if this is being used ?
  #wildlife19 <- No calculation

  disturb19 <- vals$S6_subscore


  #todo: find better way to select calculations of app.
  #appscore19 <-


  # function subscores
  # Needs attention
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  # beaver? permwat12?
  beaverhab <- ifelse(vals$F20_2 == 1, 0,
                   max(vals$Beaver , (2 * mean(c(beavers19, decid19, decidon19, permwat12 )) +
                                  mean(c( water2k19, willow19,  aqplants19, denscov19, bufferpct19))) / 3))

  # Needs attention
  #https://github.com/BCWF-Wetlands/wespr/issues/50
  # HiFlucW, permwat12? Muskrat, allsat1

  muskrathab <- ifelse(vals$F20_2 == 1, 0,
                   ifelse(vals$HiFlucW == 1, 0,
                          max(vals$Muskrat, (mean(c(permwat12, lake19, depthdom19, deeppond19a, fluc19)) +
                                          herbcov19 + mean(c(aqplants19, emerg19, growdays19, fire19, appscore19))) / 3)))




  moosehab <- ifelse(vals$AllPermW == 1, 0,
                   max(vals$Moose, (2 * mean(c(decid19, decidon19, woodyhtmix19)) +
                                 mean(c(willow19, aqplants19, water2k19, denscov19,
                                        distpond19, appscore19, buffalt19, bufferpct19))) / 3))


  # check the calculation with array works correctly
  caribouhab <- array(ifelse(vals$AllPermW == 1, 0,
                         ifelse(vals$GDeco == 1, NA_real_,
                                max(vals$Caribou, (mean(c(willow19, moss19, conifon19, conifon19, oldgro19, woodyhtmix19)) +
                                                mean(c( distrd19, rddens19, intact19, buffalt19, bufferpct19, disturb19, growdays19))) / 2))
                  ), dim = c(1, 1))


  bearhab <- ifelse(vals$AllPermW == 1, 0,
                    max(vals$Bear, (mean(c(berries19, fish19, woodyhtmix19, coverdiv19, covdiv2k19)) +
                                       mean(c(distrd19, rddens19, disturb19, intact19)) / 3)))



  kmh_fun_score <- mean_na(beaverhab, muskrathab, moosehab, caribouhab, bearhab)

  kmh_fun_score

}

# benefit score

kmh_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "kmh")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  disttown19v <- wt_max(indicator_data, "OF1", "ben")

  distpond19v <- wt_max(indicator_data, "OF3", "ben")

  water2k19v <- wt_max(indicator_data, "OF19", "ben")

  hunt19v <- max_na(vals$F57_4, vals$F57_6)


  kmh_ben_score <- 10 * mean_na(hunt19v, water2k19v, distpond19v, disttown19v)

  kmh_ben_score


}
