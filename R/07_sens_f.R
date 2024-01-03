#
sens_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "sens")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  # TO do update to sensitivity
  # fix check where limited to fun and ben
  distpond18 <- wt_max(indicator_data, "OF3", "ben")

  elev18 <- vals$OF5_1

  # TODO - CHeck the calculation is correct (https://github.com/BCWF-Wetlands/wespr/issues/42)
  outmap18 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    1 # TODO: verify this valiue? vs OutDura3 (calc'ed from F40 - Channel Connection and Ouflow duration)
  } else {
    vals$OF6_1
  }

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  water2k18 <- wt_max(indicator_data, "OF19", "fun")

  # check if amphibian response is to be included in this calculation or not
  # https://github.com/BCWF-Wetlands/wespr/issues/42
  rarespp18 <-((max_na(c(vals$OF24_1, vals$OF24_2, vals$OF24_3, vals$OF24_4)) +
    mean_na(c(vals$OF24_1, vals$OF24_2, vals$OF24_3, vals$OF24_4)))/2)

  gdd18 <- degree_days_index(vals)

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  intact18 <- wt_max(indicator_data, "OF32", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  disturb18 <- wt_max(indicator_data, "OF41", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  wetdenswau18 <- wt_max(indicator_data, "OF43", "ben")

  # todo check the calculation is correct
  #https://github.com/BCWF-Wetlands/wespr/issues/42
  woodypct18a <- max_na(vals$F1_1, vals$F1_2)/6

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  moss18a <- wt_max(indicator_data, "F10", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  invas18a <-  wt_max(indicator_data, "F13", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  nfix18a<-  wt_max(indicator_data, "F14", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  gcover18a <- ground_cover(vals, indicator_data)

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  soiltex18a <-   wt_max(indicator_data, "F17", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  seaspct18a <- percent_flooded_only_seasonally(vals, indicator_data)

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  depthdom18a <- predom_depth_class_1(vals, indicator_data)

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  widthwet18a <- distance_open_water_upland_veg_1(vals, indicator_data)

  #wt_max(indicator_data, "F17", "ben")\
  outdura18a <- if (vals$F40_4 + vals$F40_5 > 0) {
    outmap18
  } else {
    wt_max(indicator_data, "F40", "fun")
  }

  #wt_max(indicator_data, "F17", "ben")
  constric18a <- outflow_confinement_2(vals, indicator_data)

  # check this calculation - specifically refernce to D95? this is another question entirely
  acid18 <- ifelse(vals$F45_3 == 1,  NA_real_ ,
               ifelse(is.na(vals$F45_1) & vals$F45_2 == 1, 0.7,
                    ifelse(D95 < 5 | D95 > 9, 1, 0.5)
                             )
  )



  conductiv18 <- ifelse(vals$F46a_1 == NA , NA_real_ ,
                        ifelse(vals$F46a_1 < 150, 1,
                               ifelse(vals$F46a_1 > 500, 0, 0.5)))

  # check these are NA and not blanks
  tds18a <- ifelse(vals$F46b_1 == NA, NA_real_ ,
                  ifelse(vals$F46b_1 < 100, 0,
                         ifelse(vals$F46b_1 > 350, 1, 0.5)))


  # TO do update to sensitivity
  # fix check where limited to fun and ben
  beaver18a <- wt_max(indicator_data, "F48", "ben")

  # TO do update to sensitivity
  # fix check where limited to fun and ben
  perimpectper18a <- vegetation_buffer_along_permin(vals, indicator_data, "ben")


  rareonsite18a <- sum(c(vals$F58_6, vals$F58_7, vals$F58_8,  vals$F58_9))/4 +
                      max_na(c(vals$F58_6, vals$F58_7, vals$F58_8,  vals$F58_9))/2


  # subscores
  # to do - check this as the text contains an dXXXX not described in calculation
   abiosens <- max_na(outdura18a,
                      mean_na(seaspct18a, elev18, depthdom18a, constric18a),
                      soiltex18a, acid18, conductiv18, tds18a)


   biosens <- max_na(rareonsite18a, rarespp18,
                     mean_na(invas18a, gcover18a))


   colonizer <- mean_na(widthwet18a, perimpectper18a, distpond18, water2k18, wetdenswau18, intact18)

   growrate <- mean_na(moss18a, woodypct18a, beaver18a, gdd18, nfix18a)


   # calculate the final score.
   sens_score_fun <- 10 * mean_na(abiosens, biosens, colonizer, growrate)

   sens_score_fun

}

