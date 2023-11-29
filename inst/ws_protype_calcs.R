#ws_protype_calcs

# NOCA
# NoOutlet
# NoOutletX

#GDeco = OF44_1 # georgia depression
#CMeco = OF44_2 # coast and mountain (CM)
#SIMeco = OF44_3 # southern interior Mts
#BPeco = OF44_4 # Boreal Plains
#TPeco = OF44_5 # Taiga Plains




# Function

# OF6 - stream intersect
#=IF((NoOutlet+NoOutletX>0),1,D3)

# NoOutlet, NoOutletX

outmap1 <- ifelse((NoOutlet + NoOutletX) > 0, 1, OF6_1)


# OF10 - internal flow distance

flodist1 <- max(
  OF10_1 * WOF10_1,
  OF10_2 * WOF10_2,
  OF10_3 * WOF10_3,
  OF10_4 * WOF10_4,
  OF10_5 * WOF10_5,
  OF10_6 * WOF10_6) /
  max(WOF10_1, WOF10_2, WOF10_3, WOF10_4, WOF10_5,WOF10_6, WOF10_7)


# OF11 - wetland as % of its contributing area

wetpctrca1 <- max(
  OF11_1 * WOF11_1,
  OF11_2 * WOF11_2,
  OF11_3 * WOF11_3,
  OF11_4 * WOF11_4) /
  max(WOF11_1, WOF11_2, WOF11_3, WOF11_4)



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

growdays =  ifelse(OF26_1 == 0, "",
                   ifelse(OF44_1 == 1, (OF26_1 - 931) / 1545,
                          ifelse(OF44_2  == 1, (OF26_1 - 238) / 1475,
                                 ifelse(OF44_3 == 1, (OF26_1 - 205) / 2279,
                                        ifelse(OF44_4 == 1, (OF26_1 - 720) / 1114,
                                               ifelse(OF44_5 == 1, (OF26_1 - 487) / 957, NA)
                                        )
                                 )
                          )
                   )
  )



# F15 - Percent Bare Ground

gcover1 <- max(
  F15_1 * WF15_1,
  F15_2 * WF15_2,
  F15_3 * WF15_3,
  F15_4 * WF15_4
) /
  max(WF15_1, WF15_2, WF15_3, WF15_4)


# F17 - Soil Surface Texture
soiltex1 <- max(
  F17_1 * WF17_1,
  F17_2 * WF17_2,
  F17_3 * WF17_3,
  F17_4 * WF17_4,
  F17_5 * WF17_5
) /
  max(WF17_1, WF17_2, WF17_3, WF17_4, WF17_5)


# f18 Microtopography
girreg1 <- max(
  F18_1 * WF18_1,
  F18_2 * WF18_2,
  F18_3 * WF18_3
) /
  max(WF18_1, WF18_2, WF18_3)



# F20 Percent only flooded seasonally
#=IF((NeverWater=1),"",MAX(F33:F37)/MAX(E33:E37))

seaspct1 <- ifelse(NeverWater == 1, "", max(
  F20_1 * WF20_1,
  F20_2 * WF20_2,
  F20_3 * WF20_3,
  F20_4 * WF20_4,
  F20_5 * WF20_5
) /
  max(WF20_1, WF20_2, WF20_3, WF20_4, WF20_5))



# F21 Percent with Persistent Surface water

permwpct1 <- ifelse(NeverWater == 1, "", max(
  F21_1 * WF21_1,
  F21_2 * WF21_2,
  F21_3 * WF21_3,
  F21_4 * WF21_4,
  F21_5 * WF21_5,
  F21_6 * WF21_6
) /
  max(WF21_1, WF21_2, WF21_3, WF21_4, WF21_5, WF21_6))



# F25 Surface water fluctuations
#=IF((NeverWater=1),"",IF((NoPersis=1),"",MAX(F46:F50)/MAX(E46:E50)))

fluctu1 <- ifelse(NeverWater == 1, "",
                  ifelse(NoPersis == 1, "",
                         max(F25_1 * WF25_1,
                           F25_2 * WF25_2,
                           F25_3 * WF25_3,
                           F25_4 * WF25_4,
                           F25_5 * WF25_5) /
                           max(WF25_1,
                               WF25_2,
                               WF25_3,
                               WF25_4,
                               WF25_5)))


#F27 Ponded water

pondpct1 <- ifelse(NeverWater == 1, "",
       ifelse(NoPersis == 1, "",
              max(F27_1 * WF27_1,
                  F27_2 * WF27_2,
                  F27_3 * WF27_3,
                  F27_4 * WF27_4,
                  F27_5 * WF27_5,
                  F27_6 * WF27_6) /
                max(WF27_1,
                    WF27_2,
                    WF27_3,
                    WF27_4,
                    WF27_5,
                    WF27_6)))


# F29 - Largest Deep Ponded Water
# =IF((NeverWater=1),"",IF((NoPersis=1),"",IF((NoPond=1),"",MAX(F59:F64)/MAX(E59:E64))))

owareawet1 <- ifelse(NeverWater == 1, "",
                 ifelse(NoPersis == 1, "",
                        ifelse(NoPond == 1, "",
                               max(F29_1 * WF29_1,
                                   F29_2 * WF29_2,
                                   F29_3 * WF29_3,
                                   F29_4 * WF29_4,
                                   F29_5 * WF29_5,
                                   F29_6 * WF29_6) /
                                 max(WF29_1,
                                     WF29_2,
                                     WF29_3,
                                     WF29_4,
                                     WF29_5,
                                     WF29_6))))


# F40 Channel connection and outflow duration
#=IF((D69+D70>0), Outmap1,MAX(F66:F70)/MAX(E66:E70))

outdura1 <- ifelse((F40_4 + F40_5 > 0),
                   outmap1,
                   max(
                     F40_1 * WF40_1,
                     F40_2 * WF40_2,
                     F40_3 * WF40_3,
                     F40_4 * WF40_4,
                     F40_5 * WF40_5
                   ) / max(WF40_1,
                           WF40_2,
                           WF40_3,
                           WF40_4,
                           WF40_5)
)


# F41 Outflow
# =IF((NoOutlet+NoOutletX>0),"",IF((D75=1),"", MAX(F72:F74)/MAX(E72:E74)))
# this is differnt to cs version of this formular

constric1 <- #ifelse((NeverWater + TempWet) > 0, "",
                    ifelse((NoOutlet + NoOutletX) > 0, "",
                           ifelse(F41_4 == 1, "",
                                  max(F41_1 * WF41_1,
                                      F41_2 * WF41_2,
                                      F41_3 * WF41_3) /
                                    max(WF41_1, WF41_2, WF41_3)
                           )
                    )
#)

# F43 - Thoughflow Resistance
#=IF(OR(Inflow=0,NoOutlet+NoOutletX>0),"",MAX(F77:F81)/MAX(E77:E81))

throflo1 <- ifelse(Inflow == 0 | (NoOutlet + NoOutletX) > 0, "", max(
    F43_1 * WF43_1,
    F43_2 * WF43_2,
    F43_3 * WF43_3,
    F43_4 * WF43_4,
    F43_5 * WF43_5
  ) / max(WF43_1,
          WF43_2,
          WF43_3,
          WF43_4,
          WF43_5))


# F44 Internal Gradient
#=IF((NoOutlet+NoOutletX>0),"",IF((Inflow=1),"",MAX(F83:F86)/MAX(E83:E86)))

gradient1 <- ifelse((NoOutlet + NoOutletX) > 0, "",
                    ifelse(Inflow == 1, "",
                           max(
                             F44_1 * WF44_1,
                             F44_2 * WF44_2,
                             F44_3 * WF44_3,
                             F44_4 * WF44_4
                           ) / max(WF44_1,
                                   WF44_2,
                                   WF44_3,
                                   WF44_4)))

# F47 - gound water input probability

groundw1 <- max(
  F47_1 * WF47_1,
  F47_2 * WF47_2,
  F47_3 * WF47_3
) / max(WF47_1,
        WF47_2,
        WF47_3)



#######################################################
## Overall WS Function  score

## sub - function scores

#=AVERAGE(Groundw1,SoilTex1,WetPctRCA1,GrowDays1)
#=IF((NeverWater=1), "", AVERAGE(SeasPct1,Fluctu1))
#=IF((NeverWater=1), AVERAGE(Gradient1,Girreg1,Gcover1), AVERAGE(Gradient1,Constric1,ThruFlo1, FloDist1, PondPct1))

subsurf <- mean(c(groundw1,soiltex1,wetpctrca1,growdays1))
livestore <- ifelse(NeverWater == 1, "", mean(c(seaspct1, fluctu1), na.rm = TRUE))
friction <- ifelse(NeverWater == 1,
                   mean(c(gradient1, girreg1, gcover1), na.rm = TRUE),
                             mean(c(gradient1, constric1, thruflo1, flodist1, pondpct1), na.rm = TRUE))

# final function score

#=10*(IF((NoOutlet=1),1, IF((NeverWater=1),AVERAGE(MAX(Outmap1,OutDura1), AVERAGE(Friction, Subsurf)),AVERAGE(OutDura1, ((4*LiveStore+2*Friction+Subsurf)/7)))))
ws_func_score <- 10 * ifelse(NoOutlet + NoOutletX >1 , 1,
                        ifelse(NeverWater == 1,
                             mean(c(max(outmap1, outdura1), mean(c(friction, subsurf), na.rm = TRUE)), na.rm = TRUE),
                             mean(c(outdura1, ((4 * livestore + 2 * friction + subsurf) / 7)), na.rm = TRUE)
                                )
                              )



#########################################################

## Benefit

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
                      mean(c(floodprop1v, impervca1, elev1v, aspect1, disturb1, rddenswau1, rddens1, dryness1), na.rm = TRUE))




