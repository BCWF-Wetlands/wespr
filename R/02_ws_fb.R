ws_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "ws")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # Function

  outmap1 <- if ((vals$NoOutlet + vals$NoOutletX) > 0) {
    1
  } else {
    vals$OF6_1
  }

  # OF10 - internal flow distance

  # TODO: should this condition on noCA as do PR and SR? If so, use
  # internal_flow_distance() function. https://github.com/BCWF-Wetlands/wespr/issues/21
  flodist1 <- wt_max(indicator_data, "OF10", "fun")

  wetpctrca1 <- wt_max(indicator_data, "OF11", "fun")

  #To calculate the indicator score, data were is standardized to range of measured values for wetlands in this region.
  # Assuming D16, GDeco, CMeco, SIMeco, BPeco, and TPeco are vectors or variables in your R environment

  #GDeco = OF44_1 # georgia depression
  #CMeco = OF44_2 # coast and mountain (CM)
  #SIMeco = OF44_3 # southern interior Mts
  #BPeco = OF44_4 # Boreal Plains
  #TPeco = OF44_5 # Taiga Plains

  growdays1 <- degree_days_index(vals)

  # F15 - Percent Bare Ground

  # TODO: See if this should condition on F15_4 like in PR and SR. If so,
  # use ground_cover(). https://github.com/BCWF-Wetlands/wespr/issues/27
  gcover1 <- ground_cover(vals, indicator_data)


  soiltex1 <- wt_max(indicator_data, "F17", "fun")

  girreg1 <- wt_max(indicator_data, "F18", "fun")

  seaspct1 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F20", "fun")
  }

  permwpct1 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F21", "fun")
  }

  fluctu1 <- surface_water_fluctuation(vals, indicator_data)

  pondpct1 <- ponded_water(vals, indicator_data)

  owareawet1 <- if (any(unlist(vals[c("NeverWater", "NoPersis", "NoPond")]) == 1)) {
    NA_real_
  } else {
    wt_max(indicator_data, "F29", "fun")
  }

  outdura1 <- if (vals$F40_4 + vals$F40_5 > 0) {
    outmap1
  } else {
    wt_max(indicator_data, "F40", "fun")
  }

  # F41 Outflow
  # =IF((NoOutlet+NoOutletX>0),"",IF((D75=1),"", MAX(F72:F74)/MAX(E72:E74)))
  # TODO: this is differnt to cs version of this formula. Use outflow_confinement() if
  # resolved to be the same. See https://github.com/BCWF-Wetlands/wespr/issues/17.

  # constric1 <- outflow_confinement(vals, indicator_data)
  constric1 <- dplyr::case_when(
    # (vals$NeverWater + vals$TempWet) > 0 ~ NA,
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA_real_,
    vals$F41_4 == 1 ~ NA_real_,
    .default = wt_max(indicator_data, "F41", "fun")
  )


  thruflo1 <- throughflow_resistance(vals, indicator_data)

  gradient1 <- internal_gradient(vals, indicator_data)

  groundw1 <- wt_max(indicator_data, "F47", "fun")

  #######################################################
  ## Overall WS Function  score

  ## sub - function scores
  subsurf <- mean_na(c(groundw1,soiltex1,wetpctrca1,growdays1))

  livestore <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    mean_na(c(seaspct1, fluctu1))
  }

  friction <- if (vals$NeverWater == 1) {
    mean_na(c(gradient1, girreg1, gcover1))
  } else {
    mean_na(c(gradient1, constric1, thruflo1, flodist1, pondpct1))
  }

  # final function score
  # TODO: Text describing formula includes `NoOutletX`, but is not included in the
  # formula. https://github.com/BCWF-Wetlands/wespr/issues/24

   ws_fun_score <- 10 * dplyr::case_when(
    vals$NoOutlet + vals$NoOutletX > 1 ~ 1,
    vals$NeverWater == 1 ~ mean_na(
      c(max_na(outmap1, outdura1), mean_na(c(friction, subsurf)))
    ),
    .default = mean_na(
      c(outdura1, ((4 * livestore + 2 * friction + subsurf) / 7))
    )
  )

  ws_fun_score
}

#########################################################

## Benefit

ws_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "ws")
  vals <- get_vals(indicator_data)


  elev1v <- vals$OF5_1

  aspect1 <- wt_max(indicator_data, "OF7", "ben")

  floodprop1v <- if ((vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF9", "ben")
  }

  impervca1 <- unveg_surface(vals, indicator_data, "ben")

  dryness1 <- local_moisture_deficit(vals)

  rddens1 <- wt_max(indicator_data, "OF30", "ben")

  disturb1 <- if (vals$NoCA == 1) {
   NA_real_
  } else {
    wt_max(indicator_data, "OF41", "ben")
  }

  rddenswau1 <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF42", "ben")
  }

  #######################################################
  ## Overall WS BENEFIT score

  ws_ben_score  <- 10 *
    if (isTRUE(floodprop1v == 1)) {
      1
    } else {
      mean_na(c(floodprop1v, impervca1, elev1v, aspect1, disturb1, rddenswau1, rddens1, dryness1))
    }

  ws_ben_score
}

