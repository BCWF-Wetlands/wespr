#
sens_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "sens", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distpond18 <- wt_max(indicator_data, "OF3")

  elev18 <- vals$OF5_1

  outmap18 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    1
  } else {
    vals$OF6_1
  }

  water2k18 <- wt_max(indicator_data, "OF19")

  rarespp18 <-((max_na(c(vals$OF24_1, vals$OF24_2, vals$OF24_3, vals$OF24_4)) +
    mean_na(c(vals$OF24_1, vals$OF24_2, vals$OF24_3, vals$OF24_4)))/2)

  gdd18 <- degree_days_index(vals)

  intact18 <- wt_max(indicator_data, "OF32")

  disturb18 <- wt_max(indicator_data, "OF41")

  wetdenswau18 <- wt_max(indicator_data, "OF43")

  woodypct18a <- max_na(c(vals$F1_1, vals$F1_2))/6

  moss18a <- wt_max(indicator_data, "F10")

  invas18a <-  wt_max(indicator_data, "F13")

  nfix18a <-  wt_max(indicator_data, "F14")

  gcover18a <- ground_cover(vals, indicator_data)

  soiltex18a <-   wt_max(indicator_data, "F17")

  seaspct18a <- percent_flooded_only_seasonally(vals, indicator_data)

  depthdom18a <- predom_depth_class_1(vals, indicator_data)

  widthwet18a <- distance_open_water_upland_veg_1(vals, indicator_data)

  outdura18a <- if (vals$F40_4 + vals$F40_5 > 0) {
    outmap18
  } else {
    wt_max(indicator_data, "F40")
  }

  constric18a <- outflow_confinement_2(vals, indicator_data)

  acid18 <- ifelse(vals$F45_3 == 1,  NA_real_ ,
               ifelse(is.na(vals$F45_1)|| vals$F45_1 == 0 & vals$F45_2 == 1, 0.7,
                    ifelse(vals$F45_1 < 5 | vals$F45_1 > 9, 1, 0.5)
                             )
  )


  conductiv18 <- ifelse(is.na(vals$F46a_1) , NA_real_ ,
                        ifelse(vals$F46a_1 < 150, 1,
                               ifelse(vals$F46a_1 > 500, 0, 0.5)))

  # check these are NA and not blanks
  tds18a <- ifelse(is.na(vals$F46b_1), NA_real_ ,
                  ifelse(vals$F46b_1 < 100, 0,
                         ifelse(vals$F46b_1 > 350, 1, 0.5)))


  beaver18a <- wt_max(indicator_data, "F48")

  perimpectper18a <- vegetation_buffer_along_permin(vals, indicator_data)


  rareonsite18a <- sum(c(vals$F58_6, vals$F58_7, vals$F58_8,  vals$F58_9))/4 +
                      max_na(c(vals$F58_6, vals$F58_7, vals$F58_8,  vals$F58_9))/2


  # subscores
   abiosens <- max_na(c(outdura18a,
                      mean_na(seaspct18a, elev18, depthdom18a, constric18a),
                      soiltex18a, acid18, conductiv18, tds18a))


   biosens <- max_na(c(rareonsite18a, rarespp18,
                     mean_na(invas18a, gcover18a)))


   colonizer <- mean_na(c(widthwet18a, perimpectper18a, distpond18, water2k18, wetdenswau18, intact18))

   growrate <- mean_na(c(moss18a, woodypct18a, beaver18a, gdd18, nfix18a))


   # calculate the final score.
   sens_score_fun <- 10 * mean_na(c(abiosens, biosens, colonizer, growrate))

   as.indicator_score(
     sens_score_fun,
     subscores = c(
       abiosens = abiosens,
       biosens = biosens,
       colonizer = colonizer,
       growrate = growrate
     )
   )
}
