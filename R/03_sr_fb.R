
sr_func <- function(site) {

  question <- "sr"

  indicator_data <- get_indicator_data(site, question)
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  salinity2 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    1 # TODO: verify 1 vs OutDura3 (calced from F40 - Channel Connection and Ouflow duration)
  } else {
    vals$OutMap
  }

  wetpctrca3 <- wt_max(indicator_data, "OF11", "func")

  flodist3 <- if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF10", "func")
  }

  ## Subscores:
  #
  # LiveStore3=IF((NeverWater=1),"", AVERAGE(Fluc2,SeasPct2))
  # DryIntercept=AVERAGE(Gradient3, WetPctRCA3, AVERAGE(Girreg3, Sedge3,Gcover3, SoilDisturb3))
  # WetIntercept==IF((NeverWater=1),"",AVERAGE(WidthWet3, ThruFlo3, EmArea3, Interspers3, DegreeD3, FloDist3, DepthDom3, Ponded3, Constric3))
  # Connectiv3==OutDura2
  #
  ## Final:
  # =IF((NeverWater=1),DryIntercept,IF((Outmap3=0),10,10*OutDura3*AVERAGE(LiveStore3,DryIntercept,WetIntercept)))
}


# benefit

