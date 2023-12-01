
# generic functions

library(tidyverse)

# read in data and filter to questions we have implemented, and just one site:
data <- load_wesp_data("inst/input_data/wetflat.csv") |>
  select(q_no, response_no, site_1) |>
  filter(
    q_no %in% names(core_questions())
  )



# Order

#Stressors
S1
S2
S3
S4
S5
S6


# order of functions

WS_f ,
WS_b
SR_f,
SR_b,
PR_f,
PR_b,
CS_f,
FR_f,
FR_b.
Sens_f,
STR_f
NR_f,
NR_b,
AP_f
PD_f -  AP_f
KMH_f - AP_f * check ref with Paul as unclear
KMH_b
WB_f  - AP_f
POL_f - PD_f
RSB_f - AP_f, PD_f
PD_b - POL_f, RSB_f
RSB_f - AP_f, PD_f
RSB_b
PD_b - POL_f, RSB_f
OE_f - AP_f , CS_f
AM_f - AP_f ,
AM_b - WB_f
FH_f - AP_f,
FH_b - WB_f
SFTS_f
SFTS_b - FH_f
AP_b - FH_f , AH_f, WB_f, RSB_f
CRI_b - WB_f, FH_f





# standard weight function (build flexible options )

st_weight <- function(question) {

  st <- max(
    OF41_1 * WOF41_1,
    OF41_2 * WOF41_2,
    OF41_3 * WOF41_3,
    OF41_4 * WOF41_4,
    OF41_5 * WOF41_5
  ) / max(WOF41_1,
          WOF41_2,
          WOF41_3,
          WOF41_4,
          WOF41_5)

 return(st)

}

