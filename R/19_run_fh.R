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


  ## up to here


  sav10







  }









  # TO DO ; check the refernce for this alternate - lists G32 but this refers to F41 not F40?
  outmap9 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
      vals$OF6_1
    } else {
      OutDura10
    }




}
