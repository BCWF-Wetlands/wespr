str_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "str", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  distRd20 <- wt_max(indicator_data, "OF2")

  impervRCA20 <- unveg_surface_1(vals, indicator_data)

  burned20 <- vals$OF15_1

  protected20 <- 1 - vals$OF22_1

  rddens20 <- wt_max(indicator_data, "OF30")

  rddens2k20 <-  wt_max(indicator_data, "OF31")

  intact20 <- 1 - (wt_max(indicator_data, "OF32"))

  disturb20 <- if(vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF41")
  }


  rddenswau20 <- if(vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF42")
  }

  constric20 <- outflow_confinement(vals, indicator_data)

  perimpctper20 <- vegetation_buffer_along_perminNA(vals, indicator_data)

  imperv20 <- type_of_cover_buff(vals, indicator_data)

  fire20 <- wt_max(indicator_data, "F55")

  #Stressor Subscore
  #S1
  alttiming <- vals$S1_subscore

  #S2
  nutrload <- vals$S2_subscore

  #S3
  contam20 <- vals$S3_subscore

  #S4
  sedload <- vals$S4_subscore

  #S5
  soildisturb <- vals$S5_subscore

  #S6
  wldisturb <- vals$S6_subscore


  # subscores

  hydrostress <- mean_na(c(alttiming, constric20, disturb20))

  wqstress <- mean_na(c(nutrload, contam20, sedload, soildisturb, impervRCA20, imperv20, burned20, fire20))

  connecstress <- mean_na(c(perimpctper20, distRd20, rddens20, rddens2k20, rddenswau20, protected20, intact20, wldisturb))

  str_fun_score <- 10 * max_na(c(hydrostress, wqstress, connecstress))

  as.indicator_score(
    str_fun_score,
    subscores = c(
      hydrostress = hydrostress,
      wqstress = wqstress,
      connecstress = connecstress
    )

  )

}
