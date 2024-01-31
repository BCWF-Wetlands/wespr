sfts_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "sfts", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  if (vals$NoOutlet + vals$NoOutletX == 0) {
    outmap2 <- outdura2 <- ifelse(vals$F40_4 + vals$F40_5 > 0, outmap2 , wt_max(indicator_data, "F40"))
  } else {
    outdura2 <- outmap2 <- vals$OF6_1
  }

  faults2 <- ifelse(vals$OF17_1 == 0 , NA_real_, 1)

  topopos2 <- vals$OF29_1 / 5

  # OF153 - not the range classed, chcek this is correct
  # check OF39 is included in this indicator
  conif2 <- if(sum_na(vals$OF39_1, vals$OF39_2, vals$OF39_3, vals$OF39_4, vals$OF39_5) == 0) {
    # is.na(vals$OF39_0))
    NA_real_
  } else {
    vals$OF39_1
  }

  woodypct2 <- veg_height_weight(indicator_data, "F1")

  moss2 <- wt_max(indicator_data, "F10")

  gcover2 <- if(vals$F15_4 == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "F15")
  }

  soiltex2 <- wt_max(indicator_data, "F17")

  # calculator is incorrect here
  alldry2 <- wt_max(indicator_data, "F19")


  woodydryshade2 <- if(vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F24")
  }

  depthdom2 <- if(vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26")
  }

  ponded2 <- if(vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F27")
  }

  openw2 <- open_water_extent(vals, indicator_data)

  vwidth2 <- if(vals$NeverWater ==1 ||
                vals$NoDeepPonded == 1 ||
                vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }

  # check these are NA and not blanks
  conduc2 <- ifelse(is.na(vals$F46a_1) , NA_real_ ,
                    ifelse(vals$F46a_1 < 150, 0,
                           ifelse(vals$F46a_1 > 500, 1, 0.5)))

  # check these are NA and not blanks
  tds2 <- ifelse(is.na(vals$F46b_1), NA_real_ ,
                 ifelse(vals$F46b_1 < 100, 0,
                        ifelse(vals$F46b_1 > 350, 1, 0.5)))


  # updated weights table.
  groundw2 <- wt_max(indicator_data, "F47")


  # subscore calcs for fun

  shadedsurf <- mean_na(woodydryshade2, woodypct2, conif2, gcover2, alldry2, soiltex2, vwidth2)
  surfacestorage <- if(vals$NeverWater == 1 ||
                       vals$NoPersis ==1 ){
    NA_real_
  } else {
    mean_na(ponded2, openw2, depthdom2, moss2)
  }

  groundwater <- mean_na(groundw2, faults2, topopos2, max_na(conduc2, tds2))



  sfts_fun_score <- 10 * (outmap2 * mean_na(c(shadedsurf, groundwater, surfacestorage)))

  as.indicator_score(
    score = sfts_fun_score,
    subscores = c(
      shadedsurf = shadedsurf,
      surfacestorage = surfacestorage,
      groundwater = groundwater
    )
  )
}

# benefits score

sfts_ben<- function(site) {

  indicator_data <- get_indicator_data(site, "sfts", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev2v <- vals$OF5_1

  # add outmpa and outdura
  if (vals$NoOutlet + vals$NoOutletX == 0) {
    outmap2 <- outdura2 <- ifelse(vals$F40_4 + vals$F40_5 > 0, outmap2 , wt_max(indicator_data, "F40"))
  } else {
    outdura2 <- outmap2 <- vals$OF6_1
  }

  aspect2v <- wt_max(indicator_data, "OF7")

  glacier2v <- wt_max(indicator_data, "OF8")

  wetpctrca2v <- wt_max(indicator_data, "OF11")

  impervrca2v <- if(vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF12")
  }

  wetdef2 <- local_moisture_deficit(vals)

  gdd2v <- degree_days_index(vals)

  solar2v <- local_solar_input(vals)

  rddens2v <- if((sum_na(vals$OF30_1, vals$OF30_2, vals$OF30_3) == 0) ||
                 vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF30")
  }


  disturb2v <- if((sum_na(vals$OF41_1, vals$OF41_2, vals$OF41_3, vals$OF41_4, vals$OF41_5) == 0) ||
                  vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF41")
  }


  rddenswau2v <- if((sum_na(vals$OF42_1, vals$OF42_2, vals$OF42_3) == 0) ||
                    vals$NoCA == 1){
    NA_real_
  } else {
    wt_max(indicator_data, "OF42")
  }

  perminpctper2v <- vegetation_buffer_along_permin(vals, indicator_data)

  flowalt2 <- vals$S1_subscore

  fishscore2v <- get_indicator_score(site, "fh", "fun") / 10


  sfts_ben_score <- 10 * (max_na(outmap2, outdura2) *
                            ((3 * fishscore2v) + mean_na(elev2v, wetpctrca2v) +
                            mean_na(c(wetdef2, gdd2v, solar2v, glacier2v, aspect2v)) +
                            mean_na(c(perminpctper2v, impervrca2v, rddens2v, rddenswau2v, flowalt2, disturb2v)))/6)

  as.indicator_score(sfts_ben_score)
}




