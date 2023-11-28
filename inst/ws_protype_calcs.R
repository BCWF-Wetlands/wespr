#ws_protype_calcs


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



# F21 Pectent with Persistent Surface water

permwpct1 <- ifelse(NeverWater == 1, "", max(
  F21_1 * WF21_1,
  F21_2 * WF21_2,
  F21_3 * WF21_3,
  F21_4 * WF21_4,
  F21_5 * WF21_5,
  F21_6 * WF21_6
) /
  max(WF21_1, WF21_2, WF21_3, WF21_4, WF21_5, WF21_6))


