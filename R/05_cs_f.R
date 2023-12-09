cs_f <- function(site, weightings) {

  indicator_data <- get_indicator_data(site, weightings, "cs")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # OF15 burned # OF15

  burn6 = 1 - vals$OF15_1

  # OF25 - local moisure deficit - OF25_1

  ##requires standardized range of measures from region
  # 326 in this case
  # need to ask paul how these are calculated?

  wetdef6 <- 1 - local_moisture_deficit(vals)

  # F1 - Vegetation height & form diversity F1 _0 + weighted values
  #In calculations, score is the average of the sum of coniferous cover among the 3 height classes, adjusted to a 0-1 scale, and the maximum of the coniferous height classes, adjusted to a 0-1 scale.

  # not sure where 18 and 10 come from?
  #(MAX(F6,F8,F10)/18 + SUM(D6,D8,D10)/10)/2

  # value * weight

  woodypct6 <- (
    (max_na(
      vals$F1_1 * weights$WF1_1,
      vals$F1_3 * weights$WF1_3,
      vals$F1_5 * weights$WF1_5
    ) / 18) +
      sum_na(vals$F1_1, vals$F1_3, vals$F1_5) / 10
  ) / 2

  # F3 - woody Diameter Classes
  #=MAX(F13:F20)/MAX(E13:E20)
  #<- max(F13:F20) / max(E13:E20)

  treetyp6 <- wt_max(indicator_data, "F3", "function")


  # F10 - dense Moss Extent
  #=MAX(F22:F26)/MAX(E22:E26)

  moss6 <- wt_max(indicator_data, "F10", "function")

  # F15 - Percent Bare Ground
  gcover6 <- wt_max(indicator_data, "F15", "function")

  # F17 - soil surface texture
  #=MAX(F33:F37)/MAX(E33:E37)

  soiltex6 <- wt_max(indicator_data, "F17", "function")

  # F40 - Channel connections and outflows
  #ifelse((D42 + D43) > 0, 1, max(F39:F43) / max(E39:E43))

  outdura6 <- dplyr::case_when(
    (vals$F40_4 + vals$F40_5) > 0 ~ 1,
    .default = wt_max(indicator_data, "F40", "function")
  )


  # F41 - outflow confinement and Artificial drainage
  #=IF((NeverWater+TempWet>0),"",IF((NoOutlet+NoOutletX>0),"",IF((D48=1),"",MAX(F45:F47)/MAX(E45:E47))))
  # requires

  constric6 <- dplyr::case_when(
    vals$NeverWater + vals$TempWet > 0 ~ NA,
    vals$NoOutlet + vals$NoOutletX > 0 ~ NA,
    vals$F41_4 == 1 ~ NA,
    .default = wt_max(indicator_data, "F41", "function")
  )


  # F55 - PH MEASUREMENT

  # This should be NA or numeric
  pH <- vals$F45_1

  acidic6  <- dplyr::case_when(
    vals$F45_3 == 1 ~ NA,
    vals$F45_2 == 1 ~ 0.6,
    is.na(pH) ~ NA,
    pH > 0 & pH < 5 ~ 1,
    pH > 7.5 ~ 0,
    .default = 0.2
  )

  #Fire history # F55

  fire6 <- wt_max(indicator_data, "F55", "function")

  # S5 - Soil or Sediment Alteration within the assessment area

  soildisturb6 <- vals$S5_subscore

  ## Overall CS score
  # Assuming SoilTex6, Moss6, Acidic6, OutDura6, WoodyPct6, TreeTyp6, Fire6, Burn6,
  # Gcover6, Constric6, WetDef6, SoilDisturb6 are variables

  10 * (5 * max_na(soiltex6, moss6, acidic6) +
          2 * outdura6 + woodypct6 +
          mean_na(c(treetyp6, fire6, burn6, gcover6, constric6, wetdef6, soildisturb6))) / 9
}
