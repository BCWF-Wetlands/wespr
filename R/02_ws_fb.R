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

  outmap1 <- ifelse(
    (vals$NoOutlet + vals$NoOutletX) > 0,
    1,
    vals$OF6_1
  )


  # OF10 - internal flow distance

  flodist1 <- max_na(
    vals$OF10_1 * weights$WOF10_1,
    vals$OF10_2 * weights$WOF10_2,
    vals$OF10_3 * weights$WOF10_3,
    vals$OF10_4 * weights$WOF10_4,
    vals$OF10_5 * weights$WOF10_5,
    vals$OF10_6 * weights$WOF10_6
  ) / max_na(
    weights$WOF10_1,
    weights$WOF10_2,
    weights$WOF10_3,
    weights$WOF10_4,
    weights$WOF10_5,
    weights$WOF10_6,
    weights$WOF10_7
  )

  # OF11 - wetland as % of its contributing area

  wetpctrca1 <- max_na(
    vals$OF11_1 * weights$WOF11_1,
    vals$OF11_2 * weights$WOF11_2,
    vals$OF11_3 * weights$WOF11_3,
    vals$OF11_4 * weights$WOF11_4
  ) / max_na(
    weights$WOF11_1,
    weights$WOF11_2,
    weights$WOF11_3,
    weights$WOF11_4
  )



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

  gcover1 <- max_na(
    vals$F15_1 * weights$WF15_1,
    vals$F15_2 * weights$WF15_2,
    vals$F15_3 * weights$WF15_3,
    vals$F15_4 * weights$WF15_4
  ) / max_na(
    weights$WF15_1,
    weights$WF15_2,
    weights$WF15_3,
    weights$WF15_4
  )


  # F17 - Soil Surface Texture
  soiltex1 <- max_na(
    vals$F17_1 * weights$WF17_1,
    vals$F17_2 * weights$WF17_2,
    vals$F17_3 * weights$WF17_3,
    vals$F17_4 * weights$WF17_4,
    vals$F17_5 * weights$WF17_5
  ) / max_na(
    weights$WF17_1,
    weights$WF17_2,
    weights$WF17_3,
    weights$WF17_4,
    weights$WF17_5
  )


  # f18 Microtopography
  girreg1 <- max_na(
    vals$F18_1 * weights$WF18_1,
    vals$F18_2 * weights$WF18_2,
    vals$F18_3 * weights$WF18_3
  ) / max_na(
    weights$WF18_1,
    weights$WF18_2,
    weights$WF18_3
  )



  # F20 Percent only flooded seasonally
  #=IF((NeverWater=1),"",MAX(F33:F37)/MAX(E33:E37))

  seaspct1 <- ifelse(
    vals$NeverWater == 1,
    "",
    max_na(
      vals$F20_1 * weights$WF20_1,
      vals$F20_2 * weights$WF20_2,
      vals$F20_3 * weights$WF20_3,
      vals$F20_4 * weights$WF20_4,
      vals$F20_5 * weights$WF20_5
    ) / max_na(
      weights$WF20_1,
      weights$WF20_2,
      weights$WF20_3,
      weights$WF20_4,
      weights$WF20_5
    )
  )



  # F21 Percent with Persistent Surface water

  permwpct1 <- ifelse(
    vals$NeverWater == 1,
    "",
    max_na(
      vals$F21_1 * weights$WF21_1,
      vals$F21_2 * weights$WF21_2,
      vals$F21_3 * weights$WF21_3,
      vals$F21_4 * weights$WF21_4,
      vals$F21_5 * weights$WF21_5,
      vals$F21_6 * weights$WF21_6
    ) / max_na(
      weights$WF21_1,
      weights$WF21_2,
      weights$WF21_3,
      weights$WF21_4,
      weights$WF21_5,
      weights$WF21_6
    )
  )



  # F25 Surface water fluctuations
  #=IF((NeverWater=1),"",IF((NoPersis=1),"",MAX(F46:F50)/MAX(E46:E50)))

  fluctu1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    vals$NoPersis == 1 ~ NA,
    .default = max_na(
      vals$F25_1 * weights$WF25_1,
      vals$F25_2 * weights$WF25_2,
      vals$F25_3 * weights$WF25_3,
      vals$F25_4 * weights$WF25_4,
      vals$F25_5 * weights$WF25_5
    ) / max_na(
      weights$WF25_1,
      weights$WF25_2,
      weights$WF25_3,
      weights$WF25_4,
      weights$WF25_5
    )
  )



  #F27 Ponded water

  pondpct1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    vals$NoPersis == 1 ~ NA,
    .default = max_na(
      vals$F27_1 * weights$WF27_1,
      vals$F27_2 * weights$WF27_2,
      vals$F27_3 * weights$WF27_3,
      vals$F27_4 * weights$WF27_4,
      vals$F27_5 * weights$WF27_5,
      vals$F27_6 * weights$WF27_6
    ) / max_na(
      weights$WF27_1,
      weights$WF27_2,
      weights$WF27_3,
      weights$WF27_4,
      weights$WF27_5,
      weights$WF27_6
    )
  )


  # F29 - Largest Deep Ponded Water
  # =IF((NeverWater=1),"",IF((NoPersis=1),"",IF((NoPond=1),"",MAX(F59:F64)/MAX(E59:E64))))

  owareawet1 <- dplyr::case_when(
    vals$NeverWater == 1 ~ NA,
    vals$NoPersis == 1 ~ NA,
    vals$NoPond == 1 ~ NA,
    .default = max_na(
      vals$F29_1 * weights$WF29_1,
      vals$F29_2 * weights$WF29_2,
      vals$F29_3 * weights$WF29_3,
      vals$F29_4 * weights$WF29_4,
      vals$F29_5 * weights$WF29_5,
      vals$F29_6 * weights$WF29_6
    ) / max_na(
      weights$WF29_1,
      weights$WF29_2,
      weights$WF29_3,
      weights$WF29_4,
      weights$WF29_5,
      weights$WF29_6
    )
  )


  # F40 Channel connection and outflow duration
  #=IF((D69+D70>0), Outmap1,MAX(F66:F70)/MAX(E66:E70))

  outdura1 <- ifelse(
    (vals$F40_4 + vals$F40_5 > 0),
    outmap1,
    max_na(
      vals$F40_1 * weights$WF40_1,
      vals$F40_2 * weights$WF40_2,
      vals$F40_3 * weights$WF40_3,
      vals$F40_4 * weights$WF40_4,
      vals$F40_5 * weights$WF40_5
    ) / max_na(
      weights$WF40_1,
      weights$WF40_2,
      weights$WF40_3,
      weights$WF40_4,
      weights$WF40_5
    )
  )


  # F41 Outflow
  # =IF((NoOutlet+NoOutletX>0),"",IF((D75=1),"", MAX(F72:F74)/MAX(E72:E74)))
  # this is differnt to cs version of this formular

  constric1 <- dplyr::case_when(
    # (vals$NeverWater + vals$TempWet) > 0 ~ NA,
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA,
    vals$F41_4 == 1 ~ NA,
    .default = max_na(
      vals$F41_1 * weights$WF41_1,
      vals$F41_2 * weights$WF41_2,
      vals$F41_3 * weights$WF41_3
    ) /
      max_na(
        weights$WF41_1,
        weights$WF41_2,
        weights$WF41_3
      )
  )

  # F43 - Thoughflow Resistance
  #=IF(OR(Inflow=0,NoOutlet+NoOutletX>0),"",MAX(F77:F81)/MAX(E77:E81))

  thruflo1 <- ifelse(
    vals$Inflow == 0 | (vals$NoOutlet + vals$NoOutletX) > 0, NA,
    max_na(
      vals$F43_1 * weights$WF43_1,
      vals$F43_2 * weights$WF43_2,
      vals$F43_3 * weights$WF43_3,
      vals$F43_4 * weights$WF43_4,
      vals$F43_5 * weights$WF43_5
    ) / max_na(
      weights$WF43_1,
      weights$WF43_2,
      weights$WF43_3,
      weights$WF43_4,
      weights$WF43_5
    )
  )


  # F44 Internal Gradient
  #=IF((NoOutlet+NoOutletX>0),"",IF((Inflow=1),"",MAX(F83:F86)/MAX(E83:E86)))

  gradient1 <- dplyr::case_when(
    (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA,
    vals$Inflow == 1 ~ NA,
    .default = max_na(
      vals$F44_1 * weights$WF44_1,
      vals$F44_2 * weights$WF44_2,
      vals$F44_3 * weights$WF44_3,
      vals$F44_4 * weights$WF44_4
    ) / max_na(
      weights$WF44_1,
      weights$WF44_2,
      weights$WF44_3,
      weights$WF44_4
    )
  )

  # F47 - gound water input probability

  groundw1 <- max_na(
    vals$F47_1 * weights$WF47_1,
    vals$F47_2 * weights$WF47_2,
    vals$F47_3 * weights$WF47_3
  ) / max_na(
    weights$WF47_1,
    weights$WF47_2,
    weights$WF47_3
  )



  #######################################################
  ## Overall WS Function  score

  ## sub - function scores

  #=AVERAGE(Groundw1,SoilTex1,WetPctRCA1,GrowDays1)
  #=IF((NeverWater=1), "", AVERAGE(SeasPct1,Fluctu1))
  #=IF((NeverWater=1), AVERAGE(Gradient1,Girreg1,Gcover1), AVERAGE(Gradient1,Constric1,ThruFlo1, FloDist1, PondPct1))

  subsurf <- mean_na(c(groundw1,soiltex1,wetpctrca1,growdays1))
  livestore <- ifelse(
    vals$NeverWater == 1, NA,
    mean_na(c(seaspct1, fluctu1))
  )

  friction <- ifelse(
    vals$NeverWater == 1,
    mean_na(c(gradient1, girreg1, gcover1)),
    mean_na(c(gradient1, constric1, thruflo1, flodist1, pondpct1))
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

ws_b <- function(x) {

  # OF5 Relative elevation
  elev1v <- OF5_1

  # OF7 Aspect

  aspect1 <- max(
    OF7_1 * WOF7_1,
    OF7_2 * WOF7_2,
    OF7_3 * WOF7_3
  ) / max(WOF7_1,
          WOF7_2,
          WOF7_3)


  # OF9 - Floodable infrastucture
  #=IF((NoOutlet+ NoOutletX>0),"",MAX(F98:F101)/MAX(E98:E101))

  floodprop1v <- ifelse((NoOutlet + NoOutletX) > 0, "",
                        max(OF9_1 * WOF9_1,
                            OF9_2 * WOF9_2,
                            OF9_3 * WOF9_3,
                            OF9_4 * WOF9_4,) / max(WOF9_1,
                                                   WOF9_2,
                                                   WOF9_3,
                                                   WOF9_4))


  # OF12 - unvegetated surface in the wetlands WAU
  #=IF((D15=1),"",MAX(F103:F105)/MAX(E103:E105))

  impervca1 <- ifelse(D15 == 1, "", max(OF12_1 * WOF12_1,
                                        OF12_2 * WOF12_2,
                                        OF12_3 * WOF12_3) / max(WOF12_1,
                                                                WOF12_2,
                                                                WOF12_3))


  # OF21 Local moisture deficit

  #=1-(IFS(D106<=0,"", GDeco=1,(D106-0)/329,CMeco=1,(D106-0)/326,SIMeco=1,(D106-0)/825, BPeco-1,(D106-24)/381, TPeco=1,(D106-0)/219))

  dryness1 <- ifelse(OF25_1 <= 0, "",
                     ifelse(OF44_1== 1, (OF25_1 - 0) / 329,
                            ifelse(OF44_2 == 1, (OF25_1 - 0) / 326,
                                   ifelse(OF44_3 == 1, (OF25_1 - 0) / 825,
                                          ifelse(OF44_4 == 1, (OF25_1 - 24) / 381,
                                                 ifelse(OF44_5 == 1, (OF25_1 - 0) / 219, "")
                                          )
                                   )
                            )
                     )
  )



  # OF 30 Road Density Within AA's buffer
  rddens <- max(OF30_1 * WOF30_1,
                OF30_2 * WOF30_2,
                OF30_3 * WOF30_3) / max(WOF30_1,
                                        WOF30_2,
                                        WOF30_3)

  # OF41 Disturbed Percentage in the WAU

  disturb1 <- ifelse(NoCA == 1, "", max(
    OF41_1 * WOF41_1,
    OF41_2 * WOF41_2,
    OF41_3 * WOF41_3,
    OF41_4 * WOF41_4,
    OF41_5 * WOF41_5
  ) / max(WOF41_1,
          WOF41_2,
          WOF41_3,
          WOF41_4,
          WOF41_5))



  # OF42 Road Density in the WAU
  rddenswau1 <- ifelse(NoCA == 1, "", max(
    OF42_1 * WOF42_1,
    OF42_2 * WOF42_2,
    OF42_3 * WOF42_3
  ) / max(WOF42_1,
          WOF42_2,
          WOF42_3))





  #######################################################
  ## Overall WS BENEFIT score

  ## sub - function scores
  #=10*IF((FloodProp1v=1),1,AVERAGE(FloodProp1v,ImpervCA1,Elev1v, Aspect1, Disturb1,RdDensWAU1,RdDens1, Dryness1))

  ws_benefit_score  <- 10 * ifelse(FloodProp1v == 1, 1,
                                   mean(c(floodprop1v, impervca1, elev1v, aspect1, disturb1, rddenswau1, rddens1, dryness1)))

}



