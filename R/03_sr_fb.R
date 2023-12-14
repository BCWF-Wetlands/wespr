sr_func <- function(site) {

  indicator_data <- get_indicator_data(site, "sr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  outmap3 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    1 # TODO: verify 1 vs OutDura3 (calced from F40 - Channel Connection and Ouflow duration)
  } else {
    vals$OutMap
  }

  wetpctrca3 <- wt_max(indicator_data, "OF11", "func")

  flodist3 <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF10", "func") # TODO confirm range of OF10 responses to include in weighted max
  }

  degreed3 <- degree_days_index(vals)

  sedge3 <- wt_max(indicator_data, "F12", "func")

  gcover3 <- if (vals$F15_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F15", "func")
  }

  girreg3 <- wt_max(indicator_data, "F18", "func")

  seaspct3 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F20", "func")
  }

  fluc2 <- surface_water_fluctuation(vals, indicator_data)

  depthdom3 <- if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26", "func")
  }

  ponded3 <- ponded_water(vals, indicator_data)

  widthwet3 <- if (vals$NeverWater == 1 ||
                   vals$NoPersis == 1 ||
                   vals$NoDeepPonded == 1 ||
                   vals$NoOW == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33", "func")
  }

  interspers3 <- if (vals$NeverWater == 1 ||
                     vals$NoPersis == 1 ||
                     vals$NoDeepPonded == 1 ||
                     vals$NoOW == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F35", "func")
  }

  emarea3 <- if (vals$NeverWater == 1 ||
                 vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F37", "func")
  }

  outdura3 <- if (vals$F40_4 + vals$F40_5 > 0) {
    outmap3
  } else {
    wt_max(indicator_data, "F40", "func")
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
  #
  # LiveStore3=IF((NeverWater=1),"", AVERAGE(Fluc2,SeasPct2))
  livestore3 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    mean_na(c(fluc2, seaspct3))
  }
  # DryIntercept=AVERAGE(Gradient3, WetPctRCA3, AVERAGE(Girreg3, Sedge3,Gcover3, SoilDisturb3))
  dryintercept <- mean_na(
    c(gradient3, wetpctrca3,
      mean_na(
        c(girreg3, gcover3, soil_disturb3)
      )
    )
  )
  # WetIntercept==IF((NeverWater=1),"",AVERAGE(WidthWet3, ThruFlo3, EmArea3, Interspers3, DegreeD3, FloDist3, DepthDom3, Ponded3, Constric3))
  wetintercept <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    mean_na(c(widthwet3, thruflo3, emarea3, interspers3, degreed3, flodist3,
            depthdom3, ponded3, constric3))
  }
  # Connectiv3==OutDura2
  connectiv3 <- outdura3
  #
  ## Final:
  # =IF((NeverWater=1),DryIntercept,IF((Outmap3=0),10,10*OutDura3*AVERAGE(LiveStore3,DryIntercept,WetIntercept)))
  sr_function <- if (vals$NeverWater == 1) {
    dryintercept
  } else if (outmap3 == 0) {
    10
  } else {
    10 * outdura3 * mean_na(c(livestore3, dryintercept, wetintercept))
  }

  sr_function
}


# benefit

