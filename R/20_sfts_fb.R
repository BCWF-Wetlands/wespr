sfts_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "sfts")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  # TO DO : these two functions rely on each other, needs a if else statement to check one before the othe is run
  outmap2 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
      vals$OF6_1
    } else {
      outdura2
    }

  # TO DO : these two functions rely on each other, needs a if else statement to check one before the othe is run
  outdura2 <- if ((vals$F40_4 + vals$F40_5) > 0) {
    outmap2
  } else {
    wt_max(indicator_data, "F40", "fun")
  }


  faults2 <- ifelse(vals$OF17_1 == 0 , NA_real_, 1)

  # to do : check this is correct
  topopos2 <- topo_position()

  # check the range is correct 1) and secondly this references the raw file in
  # OF153 - not the range classed, chcek this is correct
  # check OF39 is included in this indicator

  conif2 <- if((sum_na(intact_vals) == 0) ||
               is.na(vals$OF39_0)){
      NA_real_
    } else {
      vals$OF39_1
    }

  woodypct2 <- wt_max(indicator_data, "F1", "fun")

  moss2 <- wt_max(indicator_data, "F10", "fun")

  gcover2 <- if(vals$F15_4 == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F15", "fun")
  }

  soiltex2 <- wt_max(indicator_data, "F17", "fun")

  alldry2 <- wt_max(indicator_data, "F19", "fun")


  # missing F23 values in STFR
  woodydryshade2 <- if(vals$NeverWater == 1 || vals$NoPersis == 1) {
      NA_real_
    } else {
      wt_max(indicator_data, "F23", "fun")
    }

  depthdom2 <- if(vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26", "fun")
  }

  ponded2 <- if(vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F27", "fun")
  }

  openw2 <- open_water_extent(vals, indicator_data)

  vwidth2 <- if(vals$NeverWater ==1 ||
                vals$NoDeepPonded == 1 ||
                vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33", "fun")
  }

  # check these are NA and not blanks
  conduc2 <- ifelse(is.na(vals$F46a_1) , NA_real_ ,
                          ifelse(vals$F46a_1 < 150, 0,
                                 ifelse(vals$F46a_1 > 500, 1, 0.5)))

  # check these are NA and not blanks
  tds2 <- ifelse(is.na(vals$F46b_1), NA_real_ ,
                  ifelse(vals$F46b_1 < 100, 0,
                         ifelse(vals$F46b_1 > 350, 1, 0.5)))



  groundw2 <- wt_max(indicator_data, "F47", "fun")


  # subscore calcs for fun

  shadedsurf <- mean_na(woodydryshade2, woodypct2, conif2, gcover2, alldry2, soiltex2, vwidth2)
  surfacestorage <- if(vals$NeverWater == 1 ||
                         vals$NoPersis ==1 ){
      NA_real_
    } else {
      mean_na(ponded2, openw2, depthdom2, moss2)
    }

  groundwater <- mean_na(groundw2, faults2, topopos2, max_na(conduc2, tds2))



  sfts_fun_score <- 10 * (outmap2 * mean_na(shadedsurf, groundwater, surfacestorage))


  sfts_fun_score

}

# benefits score

sfts_ben<- function(site) {

  indicator_data <- get_indicator_data(site, "sfts")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev2v <- vals$OF5_1

  aspect2v <- wt_max(indicator_data, "OF7", "ben")

  glacier2v <- wt_max(indicator_data, "OF8", "ben")

  wetpctrca2v <- wt_max(indicator_data, "OF11", "ben")

  impervrca2v <- if(vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF11", "ben")
    }

  wetdef2 <- local_moisture_deficit(vals)

  gdd2v <- degree_days_index(vals)

  solar2v <- local_solar_input(vals)


  # check range of values is correct
  rddens2v <- if((sum_na(intact_vals) == 0) ||
                  vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF30", "ben")
  }

  # check range of values is correct
  disturb2v <- if((sum_na(intact_vals) == 0) ||
                  vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF41", "ben")
  }

  # check range of values is correct
  rddenswau2v <- if((sum_na(intact_vals) == 0) ||
                    vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF42", "ben")
  }

  perminpctper2v <- vegetation_buffer_along_permin(vals, indicator_data, "ben")

  flowalt2 <- vals$S1_subscore

  # add fish score value
  fishscore2v <- 1 # Needs to be updated.


  sfts_ben_score <- 10 * (max_na(outmap2, outdura) *
                            3 * fishscore2v +
                            mean_na(elev2v, wetpctrca2v) +
                            mean_na(wetdef2, gdd2v, solar2v, glacier2v, aspect2v) +
                            mean_na(perminpctper2v, impervrca2v, rddens2v, rddenswau2v, flowalt2, disturb2v))/6

  sfts_ben_score

}




