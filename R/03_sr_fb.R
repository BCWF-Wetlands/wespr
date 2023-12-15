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

sr_benefit <- function(site) {
  indicator_data <- get_indicator_data(site, "sr")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  elev3v <- vals$OF5_1

  glacier3v <- wt_max(indicator_data, "OF8", "benefit")

  wetpctrca3v <- wt_max(indicator_data, "OF11", "benefit")

  impervrca3v <- unveg_surface(vals, indicator_data, "benefit")

  burn3v <- vals$OF15_1

  dryness3v <- local_moisture_deficit(vals)

  topopos3v <- topo_position(vals) # TODO: The formula is wrong in the spreadsheet, need to verify

  #TODO: For the next three calculations, the formula in spreadsheet says:
  # if sum(OF30:OF43) == O, NA, else wt_max.
  # Seems like a huge range https://github.com/BCWF-Wetlands/wespr/issues/18
  rddens3v <- wt_max(indicator_data, "OF30", "benefit")

  disturb3v <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF41", "benefit")
  }

  rddenswau3v <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF42", "benefit")
  }

  alldry3 <- wt_max(indicator_data, "F19", "benefit")

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

  perimpctper3v <- if (vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F50", "benefit")
  }

  buffcovtyp3v <- if (vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F51", "benefit")
  }

  buffslope3v <- if (vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F52", "benefit")
  }

  fire3 <- wt_max(indicator_data, "F55", "benefit")

  sedin2 <- vals$S4_subscore

  # =10*(2*AVERAGE(colour3, Burn3v, Fire3, Glacier3v, RdDens3v, ImpervRCA3v, SedIn2 ) + AVERAGE(BuffCovTyp3v, BuffSlope3v, PerimPctPer3v, Disturb3v) + AVERAGE(Elev3v, WetPctRCA3v, TopoPos3v, Inflow3v, Alldry3, Dryness3v))/4

  10 * (
    2 * mean_na(c(colour3, burn3v, fire3, glacier3v, rddens3v, impervrca3v, sedin2)) +
      mean_na(c(buffcovtyp3v, buffslope3v, perimpctper3v, disturb3v)) +
      mean_na(c(elev3v, wetpctrca3v, topopos3v, inflow3v, alldry3, dryness3v))
    ) / 4
}

