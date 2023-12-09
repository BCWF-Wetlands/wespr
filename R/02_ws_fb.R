#ws_protype_calcs

# NOCA
# NoOutlet
# NoOutletX

#GDeco = OF44_1 # georgia depression
#CMeco = OF44_2 # coast and mountain (CM)
#SIMeco = OF44_3 # southern interior Mts
#BPeco = OF44_4 # Boreal Plains
#TPeco = OF44_5 # Taiga Plains

ws_f <- function(site, weightings) {

  indicator_data <- get_indicator_data(site, weightings, "ws")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # Function

  # OF6 - stream intersect
  #=IF((NoOutlet+NoOutletX>0),1,D3)

  # NoOutlet, NoOutletX

  outmap1 <- dplyr::case_when(
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ 1,
    .default = vals$OF6_1
  )


  # OF10 - internal flow distance

  flodist1 <- wt_max(indicator_data, "OF10", "function")

  # OF11 - wetland as % of its contributing area

  wetpctrca1 <- wt_max(indicator_data, "OF11", "function")

  # OF 26 - Degree Days Index

  #=IF((D16<=0),"",IF((OF!D5=1),(D16-931)/1545, IF((OF!D7=1),(D16-205)/2279, IF((OF!D8=1),(D16-720)/1114, (D16-487)/957))))
  #=IFS(D10 =0,"",GDeco=1,(D16-931)/1545,CMeco=1,(D16-238)/1475,SIMeco=1,(D16-205)/2279,BPeco=1,(D16-720)/1114,TPeco=1,(D16-487)/957)

  #To calculate the indicator score, data were is standardized to range of measured values for wetlands in this region.
  # Assuming D16, GDeco, CMeco, SIMeco, BPeco, and TPeco are vectors or variables in your R environment

  #GDeco = OF44_1 # georgia depression
  #CMeco = OF44_2 # coast and mountain (CM)
  #SIMeco = OF44_3 # southern interior Mts
  #BPeco = OF44_4 # Boreal Plains
  #TPeco = OF44_5 # Taiga Plains

  growdays1 <- dplyr::case_when(
    vals$OF26_1 == 0 ~ NA,
    vals$GDeco == 1 ~ (vals$OF26_1 - 931) / 1545,
    vals$CMeco == 1 ~ (vals$OF26_1 - 238) / 1475,
    vals$SIMeco == 1 ~ (vals$OF26_1 - 205) / 2279,
    vals$BPeco == 1 ~ (vals$OF26_1 - 720) / 1114,
    vals$TPeco == 1 ~ (vals$OF26_1 - 487) / 957,
    .default = NA
  )


  # F15 - Percent Bare Ground

  gcover1 <- wt_max(indicator_data, "F15", "function")

  # F17 - Soil Surface Texture
  soiltex1 <- wt_max(indicator_data, "F17", "function")

  # f18 Microtopography
  girreg1 <- wt_max(indicator_data, "F18", "function")

  # F20 Percent only flooded seasonally
  #=IF((NeverWater=1),"",MAX(F33:F37)/MAX(E33:E37))

  seaspct1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    .default = wt_max(indicator_data, "F20", "function")
  )



  # F21 Percent with Persistent Surface water

  permwpct1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    .default = wt_max(indicator_data, "F21", "function")
  )



  # F25 Surface water fluctuations
  #=IF((NeverWater=1),"",IF((NoPersis=1),"",MAX(F46:F50)/MAX(E46:E50)))

  fluctu1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    vals$NoPersis == 1 ~ NA,
    .default = wt_max(indicator_data, "F25", "function")
  )



  #F27 Ponded water

  pondpct1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    vals$NoPersis == 1 ~ NA,
    .default = wt_max(indicator_data, "F27", "function")
  )


  # F29 - Largest Deep Ponded Water
  # =IF((NeverWater=1),"",IF((NoPersis=1),"",IF((NoPond=1),"",MAX(F59:F64)/MAX(E59:E64))))

  owareawet1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    vals$NoPersis == 1 ~ NA,
    vals$NoPond == 1 ~ NA,
    .default = wt_max(indicator_data, "F29", "function")
  )


  # F40 Channel connection and outflow duration
  #=IF((D69+D70>0), Outmap1,MAX(F66:F70)/MAX(E66:E70))

  outdura1 <- dplyr::case_when(
    (vals$F40_4 + vals$F40_5 > 0) ~ outmap1,
    .default = wt_max(indicator_data, "F40", "function")
  )


  # F41 Outflow
  # =IF((NoOutlet+NoOutletX>0),"",IF((D75=1),"", MAX(F72:F74)/MAX(E72:E74)))
  # this is differnt to cs version of this formular

  constric1 <- dplyr::case_when(
    # (vals$NeverWater + vals$TempWet) > 0 ~ NA,
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA,
    vals$F41_4 == 1 ~ NA,
    .default = wt_max(indicator_data, "F41", "function")
  )

  # F43 - Thoughflow Resistance
  #=IF(OR(Inflow=0,NoOutlet+NoOutletX>0),"",MAX(F77:F81)/MAX(E77:E81))

  thruflo1 <- dplyr::case_when(
    vals$Inflow == 0 | (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA,
    .default = wt_max(indicator_data, "F43", "function")
  )

  # F44 Internal Gradient
  #=IF((NoOutlet+NoOutletX>0),"",IF((Inflow=1),"",MAX(F83:F86)/MAX(E83:E86)))

  gradient1 <- dplyr::case_when(
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA,
    vals$Inflow == 1 ~ NA,
    .default = wt_max(indicator_data, "F44", "function")
  )

  # F47 - gound water input probability

  groundw1 <- wt_max(indicator_data, "F47", "function")

  #######################################################
  ## Overall WS Function  score

  ## sub - function scores

  #=AVERAGE(Groundw1,SoilTex1,WetPctRCA1,GrowDays1)
  #=IF((NeverWater=1), "", AVERAGE(SeasPct1,Fluctu1))
  #=IF((NeverWater=1), AVERAGE(Gradient1,Girreg1,Gcover1), AVERAGE(Gradient1,Constric1,ThruFlo1, FloDist1, PondPct1))

  subsurf <- mean_na(c(groundw1,soiltex1,wetpctrca1,growdays1))

  livestore <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    .default = mean_na(c(seaspct1, fluctu1))
  )

  friction <- dplyr::case_when(
    vals$NeverWater == 1 ~ mean_na(c(gradient1, girreg1, gcover1)),
    .default = mean_na(c(gradient1, constric1, thruflo1, flodist1, pondpct1))
  )

  # final function score

  #=10*(IF((NoOutlet=1),1, IF((NeverWater=1),AVERAGE(MAX(Outmap1,OutDura1), AVERAGE(Friction, Subsurf)),AVERAGE(OutDura1, ((4*LiveStore+2*Friction+Subsurf)/7)))))
  ws_func_score <- 10 * dplyr::case_when(
    vals$NoOutlet + vals$NoOutletX > 1 ~ 1,
    vals$NeverWater == 1 ~ mean(
      c(max_na(outmap1, outdura1), mean_na(c(friction, subsurf)))
    ),
    .default = mean_na(
      c(outdura1, ((4 * livestore + 2 * friction + subsurf) / 7))
    )
  )

  ws_func_score
}

#########################################################

## Benefit

ws_b <- function(site, weightings) {

  indicator_data <- get_indicator_data(site, weightings, "ws")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  # OF5 Relative elevation
  elev1v <- vals$OF5_1

  # OF7 Aspect

  aspect1 <- wt_max(indicator_data, "OF7", "benefit")

  # OF9 - Floodable infrastucture
  #=IF((NoOutlet+ NoOutletX>0),"",MAX(F98:F101)/MAX(E98:E101))

  floodprop1v <- dplyr::case_when(
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA,
    .default = wt_max(indicator_data, "OF9", "benefit")
  )

  # OF12 - unvegetated surface in the wetlands WAU
  #=IF((D15=1),"",MAX(F103:F105)/MAX(E103:E105))

  impervca1 <- dplyr::case_when(
    vals$OF11_4 == 1 ~ NA,
    .default = wt_max(indicator_data, "OF12", "benefit")
  )

  # OF21 Local moisture deficit

  #=1-(IFS(D106<=0,"", GDeco=1,(D106-0)/329,CMeco=1,(D106-0)/326,SIMeco=1,(D106-0)/825, BPeco-1,(D106-24)/381, TPeco=1,(D106-0)/219))

  dryness1 <- local_moisture_defecit(vals)

  # OF 30 Road Density Within AA's buffer
  rddens1 <- wt_max(indicator_data, "OF30", "benefit")

  # OF41 Disturbed Percentage in the WAU

  disturb1 <- dplyr::case_when(
    vals$NoCA == 1 ~ NA,
    .default = wt_max(indicator_data, "OF41", "benefit")
  )



  # OF42 Road Density in the WAU
  rddenswau1 <- dplyr::case_when(
    vals$NoCA == 1 ~ NA,
    .default = wt_max(indicator_data, "OF42", "benefit")
  )

  #######################################################
  ## Overall WS BENEFIT score

  ## sub - function scores
  #=10*IF((FloodProp1v=1),1,AVERAGE(FloodProp1v,ImpervCA1,Elev1v, Aspect1, Disturb1,RdDensWAU1,RdDens1, Dryness1))

  ws_benefit_score  <- 10 * dplyr::case_when(
    floodprop1v == 1 ~ 1,
    .default = mean_na(c(floodprop1v, impervca1, elev1v, aspect1, disturb1, rddenswau1, rddens1, dryness1))
  )

  ws_benefit_score
}

