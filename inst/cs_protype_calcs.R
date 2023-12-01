
# CS calculation Prototype

# Gen to populate with calculation in readable format.



# run_cs_calcul

# check for dependencies (other scores being generates )

# check for derived variables




# # extract questionf and weighting for a particular








# OF15 burned # OF15

burn6 = 1 - OF15_1


# OF25 - local moisure deficit - OF25_1

##requires standardized range of measures from region
# 326 in this case
# need to ask paul how these are calculated?

wetdef6 <- ifelse(OF25_1 < 1, 1, ifelse(OF25_1 > 326, 0, 1 - (OF25_1 / 326)))


# F1 - Vegetation height & form diversity F1 _0 + weighted values
#In calculations, score is the average of the sum of coniferous cover among the 3 height classes, adjusted to a 0-1 scale, and the maximum of the coniferous height classes, adjusted to a 0-1 scale.

# not sure where 18 and 10 come from?
#(MAX(F6,F8,F10)/18 + SUM(D6,D8,D10)/10)/2

# value * weight

woodypct6 <- ((max((F1_1 * WF1_1), (F1_3* WF1_3), F1_5*WF1_5) / 18) + sum(F1_1, F1_3, F1_5) / 10) / 2


# F3 - woody Diameter Classes
#=MAX(F13:F20)/MAX(E13:E20)
#<- max(F13:F20) / max(E13:E20)

treetyp6 <- max(
  F3_1 * WF3_1 ,
  F3_2 * WF3_2,
  F3_3 * WF3_3,
  F3_4 * WF3_4,
  F3_5 * WF3_5,
  F3_6 * WF3_6,
  F3_7 * WF3_7,
  F3_8 * WF3_8
) /
  max(WF3_1, WF3_2, WF3_3, WF3_4, WF3_5, WF3_6, WF3_7, WF3_8)


# F10 - dense Moss Extent
#=MAX(F22:F26)/MAX(E22:E26)

moss6 <- max(
  F10_1 * WF10_1,
  F10_2 * WF10_2,
  F10_3 * WF10_3,
  F10_4 * WF10_4,
  F10_5 * WF10_5
) /
  max(WF10_1, WF10_2, WF10_3, WF10_4, WF10_5)



# F15 - Percent Bare Ground
gcover6 <- max(
  F15_1 * WF15_1,
  F15_2 * WF15_2,
  F15_3 * WF15_3,
  F15_4 * WF15_4
) /
  max(WF15_1, WF15_2, WF15_3, WF15_4)


# F17 - soil surface texture
#=MAX(F33:F37)/MAX(E33:E37)

soiltex6 <- max(
  F17_1 * WF17_1,
  F17_2 * WF17_2,
  F17_3 * WF17_3,
  F17_4 * WF17_4,
  F17_5 * WF17_5
) /
  max(WF17_1, WF17_2, WF17_3, WF17_4, WF17_5)


# F40 - Channel connections and outflows
#ifelse((D42 + D43) > 0, 1, max(F39:F43) / max(E39:E43))

outdura6 <- ifelse((F40_4 + F40_5) > 0, 1,
                   max(F40_1 * WF40_1,
                       F40_2 * WF40_2,
                       F40_3 * WF40_3,
                       F40_4 * WF40_4,
                       F40_5 * WF40_5) /
                     max(WF40_1, WF40_2, WF40_3, WF40_4, WF40_5))


# F41 - outflow confinement and Artificial drainage
#=IF((NeverWater+TempWet>0),"",IF((NoOutlet+NoOutletX>0),"",IF((D48=1),"",MAX(F45:F47)/MAX(E45:E47))))
# requires

constric6 <- ifelse((NeverWater + TempWet) > 0, "",
                   ifelse((NoOutlet + NoOutletX) > 0, "",
                          ifelse(F41_4 == 1, "",
                                 max(F41_1 * WF41_1,
                                     F41_2 * WF41_2,
                                     F41_3 * WF41_3) /
                                max(WF41_1, WF41_2, WF41_3)
                          )
                        )
                      )


# F55 - PH MEASUREMENT


D52 = ifelse(F45_1 == "", "", F45_1)

acidic6  <- ifelse(D52 == 1, "",
              ifelse(D51 == 1, 0.6,
                ifelse(D50 == "", "",
                     ifelse(D50 > 0 & D50 < 5, 1,
                            ifelse(D50 > 7.5, 0, 0.2)
                     )
                    )
                   )
                  )



#Fire history # F55

fire6 <- max(
  F55_1 * WF55_1,
  F55_2 * WF55_2,
  F55_3 * WF55_3,
  F55_4 * WF55_4,
  F55_5 * WF55_5,
  F55_6 * WF55_6,
  F55_7 * WF55_7,

) /
  max(WF55_1, WF55_2, WF55_3, WF55_4, WF55_5,WF55_6, WF55_7)



# S5 - Soil or Sediment Alteration within the assessment area

soildisturb6 <- S5_14


## Overall CS score
# Assuming SoilTex6, Moss6, Acidic6, OutDura6, WoodyPct6, TreeTyp6, Fire6, Burn6,
# Gcover6, Constric6, WetDef6, SoilDisturb6 are variables

cs_score <- 10 * (5 * max(soiltex6, moss6, acidic6) +
                    2 * outdura6 + woodypct6 +
                  mean(c(treetyp6, fire6, burn6, gcover6, constric6, wetdef6, soildisturb6))) / 9




