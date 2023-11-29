#sfts_prototype


# stream flow and temperature support



# 1. OF6 - stream intersect
#=IF((NoOutlet+NoOutletX=0),G71,D3)

# NoOutlet, NoOutletX, relies on output of F40

outmap1 <- ifelse((NoOutlet + NoOutletX) > 0, outdura2, OF6_1)


# 2. OF17 Geological faults
#=IF((D4=0),"",1)
faults2 <- ifelse(OF17_1 ==0, "", 1)



# 3. OF29 Topographic Position
#Wetlands located at  - toe slope (T) poosition are scored 5,
#                      - lower slope (L) are scored 4,
#                      - depressions (D) are scored 3,
#                      - mid-slope (M) positions are scored 2,
#                      - and upper slope (U), crest (C) and flats (F) are scored 0.


# UP TO HERE
