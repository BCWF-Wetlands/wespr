
# CS calculation Prototype

# Gen to populate with calculation in readable format.
#
# burned # OF15

Burn6 = 1 - OF15_1


# local moisure deficit - OF25_1

##requires standardized range of measures from region
# 326 in this case
# need to ask paul how these are calculated?

WetDef6 <- ifelse(OF25_1 < 1, 1, ifelse(OF25_1 > 326, 0, 1 - (OF25_1 / 326)))




# vegetation height & form diversity F1 _0 + weighted values
#In calculations, score is the average of the sum of coniferous cover among the 3 height classes, adjusted to a 0-1 scale, and the maximum of the coniferous height classes, adjusted to a 0-1 scale.

# not sure where 18 and 10 come from?
#(MAX(F6,F8,F10)/18 + SUM(D6,D8,D10)/10)/2

# value * weight

WoodyPct6 <- ((max((F1_1 * WF1_1), (F1_3* WF1_3), F1_5*WF1_5) / 18) + sum(F1_1, F1_3, F1_5) / 10) / 2


# woody Diameter Classes
#=MAX(F13:F20)/MAX(E13:E20)
#<- max(F13:F20) / max(E13:E20)

TreeTyp6 <- max(
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



# dense Moss Extent





# Percent Bare Ground


# soil surface texture



