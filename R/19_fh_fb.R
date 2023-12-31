fh_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "fh")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev10 <- 1 - vals$OF5_1

  unvegca10 <- wt_max(indicator_data, "OF12", "fun")

  fishpres10 <- ifelse(vals$OF20_5 == 1, 0, (3 * (max(vals$OF20_1, vals$OF20_2, vals$OF20_3 ) / 3) + vals$OF20_4) / 4)


  # check this range and that the questions are needed for the indicator
  rddens10 <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF30", "fun")
  }

  # check this range and that the questions are needed for the indicator
  disturbca9 <- if(sum_na(intact_vals) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF41", "fun")
  }

  # check this range is correct
  rddenswau10 <- if(sum_na(intact_vals) == 0 ||
    vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF42", "fun")
  }

  drypct9 <- wt_max(indicator_data, "F19", "fun")

  permwpct10 <- persist_water(vals, indicator_data)

  lake9 <- if(vals$NeverWater == 1 ||
              vals$NoPersis == 1){
    NA_real_
  } else {
    vals$F23_1
  }

  shade9 <- percent_summerwater_shaded(vals, indicator_data)

  depthdom10 <- if (vals$NeverWater == 0 ||
                    vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26", "fun")
  }

  woodover10 <- non_veg_aquatic_cover_1(vals, indicator_data)

  pondsize9 <- largest_deep_pond_acre(vals, indicator_data)

  openw9 <- open_water_extent_1(vals, indicator_data)

  interspers9 <- interspersion_inundated_veg_2(vals)

  sav10 <- submerged_floating_aquatics(vals, indicator_data)

  thurflo10 <- throughflow_resistance(vals, indicator_data)

  pH <- ifelse(is.na(vals$F45_1), NA_real_, vals$F45_1)

  acid10 <- ifelse(is.na(pH)  & vals$F45_2 == 1, 0.3,
                   ifelse(is.na(pH)  & vals$F45_3 == 1, 0.7,
                          ifelse(is.na(pH) , NA_real,
                                 ifelse(pH >= 7.5 & pH <= 9, 1,
                                        ifelse(pH < 5 | pH > 9, 0, 0.5)))))



  groundw10 <- wt_max(indicator_data, "F47", "fun")

  bufferpct10 <- vegetation_buffer_along_permin(vals, indicator_data, "fun")


  # TO DO ; check the refernce for this alternate - lists G32 but this refers to F41 not F40?
  # TO DO : these two functions rely on each other, needs a if else statement to check one before the othe is run
  outmap9 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    vals$OF6_1
  } else {
    outdura10
  }

  # TO DO : these two functions rely on each other, needs a if else statement to check one before the othe is run
  outdura10 <- if ((vals$F40_4 + vals$F40_5) > 0) {
    outmap9
  } else {
    wt_max(indicator_data, "F40", "fun")
  }



  alttiming10 <- vals$S1_subscore
  contam10 <- vals$S3_subscore
  sedrca10 <- vals$S4_subscore

  # TO DO find elegant way to extract the scores.
  appscore9 <- 1 # NOTE THIS NEEDS TO BE RELPLACED

    # function subscores
  hydro10 <- mean_na(drypct9, permwpct10, depthdom10, lake9, pondsize9, openw9)

  struc10 <- if(vals$NeverWater + vals$TempWet > 0){
    NA_real_
  } else {
    mean_na(groundw10, thurflo10 , sav10, shade9, woodover10
            )
  }

  nooxyrisk <- if(vals$NeverWater == 1 ||
                  vals$NoPersis == 1){
    NA_real_
  } else {
    mean_na(outmap9, pondsize9)
  }

  nostress10 <- mean_na(alttiming10, contam10, sedrca10, acid10, rdens10, rdenswau11, bufferpct10)


  # function score

  fh_fun_score <-  10 * ifelse(vals$Fishless == 1, 0,
                        ifelse(vals$NeverWater == 1, 0,
                               ifelse((vals$TooShallow + vals$NoSeasonal) == 0,
                                      (3 * fishpres10 +
                                       2 * mean(c(appscore9, hydro10, struc10 )) + nostress10) / 6, 0)))


  fh_fun_score


}


# Benefits

fh_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "fh")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  popdist10 <- if(vals$Inflow == 0 ||
                 (vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF1", "ben")
  }

  rddist10v <- if(vals$Inflow == 0 ||
                  (vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF2", "ben")
  }

  boats10v <- vals$F56_2

  fishing10v <- vals$F56_5


  # to do : update this with correct extraction scors
  fscorewbf10v <-


  # benefit subscore:

  fh_ben_score <- 10 * (fscorewbf10v + fishing10v + mean_na(popdist10, rddist10v, boats10v))/3

  fh_ben_score

}


