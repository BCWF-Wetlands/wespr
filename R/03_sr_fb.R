sr_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "sr", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  outmap3 <- if (vals$NoOutlet + vals$NoOutletX > 0) {
    1
  } else {
    vals$OF6_1
  }

  wetpctrca3 <- wt_max(indicator_data, "OF11")

  flodist3 <- internal_flow_distance(vals, indicator_data)

  degreed3 <- degree_days_index(vals)

  sedge3 <- wt_max(indicator_data, "F12")

  gcover3 <- ground_cover(vals, indicator_data)

  girreg3 <- wt_max(indicator_data, "F18")

  seaspct3 <- percent_flooded_only_seasonally(vals, indicator_data)

  fluc2 <- surface_water_fluctuation(vals, indicator_data)

  depthdom3 <- if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26")
  }

  ponded3 <- ponded_water(vals, indicator_data)

  widthwet3 <- if (vals$NeverWater == 1 ||
                   vals$NoPersis == 1 ||
                   vals$NoDeepPonded == 1 ||
                   vals$NoOW == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }

  interspers3 <- if (vals$NeverWater == 1 ||
                     vals$NoPersis == 1 ||
                     vals$NoDeepPonded == 1 ||
                     vals$NoOW == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F35")
  }

  emarea3 <- inundated_erect_veg(vals, indicator_data)

  outdura3 <- if (vals$F40_4 + vals$F40_5 > 0) {
    outmap3
  } else {
    wt_max(indicator_data, "F40")
  }

  constric3 <- outflow_confinement(vals, indicator_data)

  thruflo3 <- throughflow_resistance(vals, indicator_data)

  gradient3 <- internal_gradient(vals, indicator_data)

  soil_disturb3 <- if (vals$S5_subscore == 0) {
    1
  } else {
    vals$S5_subscore
  }

  ## Subscores:

  livestore3 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    mean_na(c(fluc2, seaspct3))
  }

  dryintercept <- mean_na(
    c(gradient3, wetpctrca3,
      mean_na(
        c(girreg3, sedge3, gcover3, soil_disturb3)
      )
    )
  )

  wetintercept <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    mean_na(c(widthwet3, thruflo3, emarea3, interspers3, degreed3, flodist3,
            depthdom3, ponded3, constric3))
  }

  connectiv3 <- outdura3

  ## Final:
  sr_fun_score <- if (vals$NeverWater == 1) {
    dryintercept
  } else if (outmap3 == 0) {
    10
  } else {
    10 * outdura3 * mean_na(c(livestore3, dryintercept, wetintercept))
  }

  sr_fun_score
}


# benefit

sr_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "sr", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev3v <- vals$OF5_1

  glacier3v <- wt_max(indicator_data, "OF8")

  wetpctrca3v <- wt_max(indicator_data, "OF11")

  impervrca3v <- unveg_surface(vals, indicator_data)

  burn3v <- vals$OF15_1

  dryness3v <- local_moisture_deficit(vals)

  topopos3v <- vals$OF29_1 / 5

  rddens3v <- wt_max(indicator_data, "OF30")

  disturb3v <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF41")
  }

  rddenswau3v <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF42")
  }

  alldry3 <- wt_max(indicator_data, "F19")

  colour3 <- if (vals$F39_3 == 1) {
    1
  } else {
    NA_real_
  }

  inflow3v <- if (vals$NoOutlet + vals$NoOutletX > 0) {
    NA_real_
  } else {
    vals$F42_1
  }

  perimpctper3v <- vegetation_buffer_along_permin(vals, indicator_data)

  buffcovtyp3v <- type_of_cover_buff(vals, indicator_data)

  buffslope3v <- buffer_slope(vals, indicator_data)

  fire3 <- wt_max(indicator_data, "F55")

  sedin2 <- vals$S4_subscore


  sr_ben_score <- 10 * (
    2 * mean_na(c(colour3, burn3v, fire3, glacier3v, rddens3v, rddenswau3v, impervrca3v, sedin2)) +
      mean_na(c(buffcovtyp3v, buffslope3v, perimpctper3v, disturb3v)) +
      mean_na(c(elev3v, wetpctrca3v, topopos3v, inflow3v, alldry3, dryness3v))
    ) / 4

  sr_ben_score
}
