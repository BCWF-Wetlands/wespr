ws_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "ws", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # Function

  outmap1 <- if ((vals$NoOutlet + vals$NoOutletX) > 0) {
    1
  } else {
    vals$OF6_1
  }

  flodist1 <- wt_max(indicator_data, "OF10")

  wetpctrca1 <- wt_max(indicator_data, "OF11")

  growdays1 <- degree_days_index(vals)

  gcover1 <- ground_cover(vals, indicator_data)

  soiltex1 <- wt_max(indicator_data, "F17")

  girreg1 <- wt_max(indicator_data, "F18")

  seaspct1 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F20")
  }

  permwpct1 <- if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F21")
  }

  fluctu1 <- surface_water_fluctuation(vals, indicator_data)

  pondpct1 <- ponded_water(vals, indicator_data)

  owareawet1 <- if (any(unlist(vals[c("NeverWater", "NoPersis", "NoPond")]) == 1)) {
    NA_real_
  } else {
    wt_max(indicator_data, "F29")
  }

  outdura1 <- if (vals$F40_4 + vals$F40_5 > 0) {
    outmap1
  } else {
    wt_max(indicator_data, "F40")
  }

  constric1 <- outflow_confinement_1(vals, indicator_data)

  thruflo1 <- throughflow_resistance(vals, indicator_data)

  gradient1 <- internal_gradient(vals, indicator_data)

  groundw1 <- wt_max(indicator_data, "F47")

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

   ws_fun_score <- 10 * dplyr::case_when(
    vals$NoOutlet + vals$NoOutletX > 0 ~ 1,
    vals$NeverWater == 1 ~ mean_na(
      c(max_na(c(outmap1, outdura1)), mean_na(c(friction, subsurf)))
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

  indicator_data <- get_indicator_data(site, "ws", "ben")
  vals <- get_vals(indicator_data)


  elev1v <- vals$OF5_1

  aspect1 <- wt_max(indicator_data, "OF7")

  floodprop1v <- if ((vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF9")
  }

  impervca1 <- unveg_surface(vals, indicator_data)

  dryness1 <- local_moisture_deficit(vals)

  rddens1 <- wt_max(indicator_data, "OF30")

  disturb1 <- if (vals$NoCA == 1) {
   NA_real_
  } else {
    wt_max(indicator_data, "OF41")
  }

  rddenswau1 <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF42")
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

