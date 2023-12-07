
# CS calculation Prototype

# Gen to populate with calculation in readable format.



# run_cs_calcul

# check for dependencies (other scores being generates )

# check for derived variables




# # extract questionf and weighting for a particular

cs_func <- function(x) {

  vals <- as.list(x$value)
  names(vals) <- x$response_no

  weights <- as.list(x$q_weighting)
  names(weights) <- paste0("W", names(vals))

  # OF15 burned # OF15

  burn6 = 1 - vals$OF15_1

  # OF25 - local moisure deficit - OF25_1

  ##requires standardized range of measures from region
  # 326 in this case
  # need to ask paul how these are calculated?

  wetdef6 <- 1 - dplyr::case_when(
    vals$OF25_1 <= 0 ~ NA,
    vals$GDeco == 1 ~ (vals$OF25_1 - 0) / 329,
    vals$CMeco == 1 ~ (vals$OF25_1 - 0) / 326,
    vals$SIMeco == 1 ~ (vals$OF25_1 - 0) / 825,
    vals$BPeco == 1 ~ (vals$OF25_1 - 24) / 381,
    vals$TPeco == 1 ~ (vals$OF25_1 - 0) / 219,
    .default = NA
  )

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

  treetyp6 <- max_na(
    vals$F3_1 * weights$WF3_1 ,
    vals$F3_2 * weights$WF3_2,
    vals$F3_3 * weights$WF3_3,
    vals$F3_4 * weights$WF3_4,
    vals$F3_5 * weights$WF3_5,
    vals$F3_6 * weights$WF3_6,
    vals$F3_7 * weights$WF3_7,
    vals$F3_8 * weights$WF3_8
  ) /
    max_na(
      weights$WF3_1,
      weights$WF3_2,
      weights$WF3_3,
      weights$WF3_4,
      weights$WF3_5,
      weights$WF3_6,
      weights$WF3_7,
      weights$WF3_8
    )


  # F10 - dense Moss Extent
  #=MAX(F22:F26)/MAX(E22:E26)

  moss6 <- max_na(
    vals$F10_1 * weights$WF10_1,
    vals$F10_2 * weights$WF10_2,
    vals$F10_3 * weights$WF10_3,
    vals$F10_4 * weights$WF10_4,
    vals$F10_5 * weights$WF10_5
  ) /
    max_na(
      weights$WF10_1,
      weights$WF10_2,
      weights$WF10_3,
      weights$WF10_4,
      weights$WF10_5
    )



  # F15 - Percent Bare Ground
  gcover6 <- max_na(
    vals$F15_1 * weights$WF15_1,
    vals$F15_2 * weights$WF15_2,
    vals$F15_3 * weights$WF15_3,
    vals$F15_4 * weights$WF15_4
  ) /
    max_na(
      weights$WF15_1,
      weights$WF15_2,
      weights$WF15_3,
      weights$WF15_4
    )


  # F17 - soil surface texture
  #=MAX(F33:F37)/MAX(E33:E37)

  soiltex6 <- max_na(
    vals$F17_1 * weights$WF17_1,
    vals$F17_2 * weights$WF17_2,
    vals$F17_3 * weights$WF17_3,
    vals$F17_4 * weights$WF17_4,
    vals$F17_5 * weights$WF17_5
  ) /
    max_na(
      weights$WF17_1,
      weights$WF17_2,
      weights$WF17_3,
      weights$WF17_4,
      weights$WF17_5
    )


  # F40 - Channel connections and outflows
  #ifelse((D42 + D43) > 0, 1, max(F39:F43) / max(E39:E43))

  outdura6 <- ifelse((vals$F40_4 + vals$F40_5) > 0, 1,
                     max_na(vals$F40_1 * weights$WF40_1,
                            vals$F40_2 * weights$WF40_2,
                            vals$F40_3 * weights$WF40_3,
                            vals$F40_4 * weights$WF40_4,
                            vals$F40_5 * weights$WF40_5) /
                       max_na(
                         weights$WF40_1,
                         weights$WF40_2,
                         weights$WF40_3,
                         weights$WF40_4,
                         weights$WF40_5
                       ))


  # F41 - outflow confinement and Artificial drainage
  #=IF((NeverWater+TempWet>0),"",IF((NoOutlet+NoOutletX>0),"",IF((D48=1),"",MAX(F45:F47)/MAX(E45:E47))))
  # requires

  constric6 <- dplyr::case_when(
    vals$NeverWater + vals$TempWet > 0 ~ NA,
    vals$NoOutlet + vals$NoOutletX > 0 ~ NA,
    vals$F41_4 == 1 ~ NA,
    .default = max_na(
      vals$F41_1 * weights$WF41_1,
      vals$F41_2 * weights$WF41_2,
      vals$F41_3 * weights$WF41_3
    ) /
      max_na(weights$WF41_1, weights$WF41_2, weights$WF41_3)
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

  fire6 <- max_na(
    vals$F55_1 * weights$WF55_1,
    vals$F55_2 * weights$WF55_2,
    vals$F55_3 * weights$WF55_3,
    vals$F55_4 * weights$WF55_4,
    vals$F55_5 * weights$WF55_5,
    vals$F55_6 * weights$WF55_6,
    vals$F55_7 * weights$WF55_7
  ) /
    max_na(
      weights$WF55_1,
      weights$WF55_2,
      weights$WF55_3,
      weights$WF55_4,
      weights$WF55_5,
      weights$WF55_6,
      weights$WF55_7
    )



  # S5 - Soil or Sediment Alteration within the assessment area

  soildisturb6 <- vals$S5_14


  ## Overall CS score
  # Assuming SoilTex6, Moss6, Acidic6, OutDura6, WoodyPct6, TreeTyp6, Fire6, Burn6,
  # Gcover6, Constric6, WetDef6, SoilDisturb6 are variables

  10 * (5 * max_na(soiltex6, moss6, acidic6) +
          2 * outdura6 + woodypct6 +
          mean_na(c(treetyp6, fire6, burn6, gcover6, constric6, wetdef6, soildisturb6))) / 9
}
