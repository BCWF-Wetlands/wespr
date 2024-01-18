#
fr_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "fr", "fun")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)


  aspect7 <- wt_max(indicator_data, "OF7")

  burned7 <- vals$OF15_1

  conif7 <- if(sum(vals$OF39_1 , vals$OF39_2, vals$OF39_3 ,vals$OF39_4 , vals$OF39_5)== 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF39")
  }

  woodycov7 <- ifelse(sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5) < 2, 1,
                     ifelse(sum(vals$F1_1, vals$F1_3) > 8, 0.2,
                            ifelse(sum(vals$F1_1,vals$F1_3) > 6, 0.4,
                                   ifelse(sum(vals$F1_1, vals$F1_3) > 4, 0.6,
                                          ifelse(sum(vals$F1_1, vals$F1_3) > 2, 0.8, 0)
                                   )
                            )
                     )
    )


  gcov8 <- if(vals$F15_5 == 1){
      NA_real_
    } else {
      wt_max(indicator_data, "F15")
    }

  satpct7 <- wt_max(indicator_data, "F19")

  persispct7 <- wt_max(indicator_data, "F21")

  fringe7 <- if (vals$NeverWater >0 ||
                 vals$NoPersis == 1||
                 vals$F22_1 == 0 ){
      NA_real_
    } else {
     1
    }

  lake7 <- if (vals$NeverWater >0 ||
               vals$NoPersis == 1||
               vals$F23_1 == 0 ){
       NA_real_
    } else {
      1
    }

  openw7 <- if(vals$NeverWater == 1 ||
               vals$NoPersis == 1||
               vals$NoDeepPonded == 0 ){
    NA_real_
  } else {
    wt_max(indicator_data, "F31")
  }


  fetch7 <- if (vals$NeverWater == 1 ||
                vals$NoPersis == 1 ||
                vals$NoDeepPonded == 1 ||
                vals$NoOW == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F32")
  }

  fire7 <-  wt_max(indicator_data, "F55")


  fr_fun_score <- 10 * ((3 * mean_na(c(fringe7, lake7, persispct7, openw7, fetch7, satpct7)) +
                        2 * woodycov7 +
                       mean_na(c(aspect7, gcov8, fire7))) / 6)

  fr_fun_score
}

# Benefit

fr_ben <- function(site){

  indicator_data <- get_indicator_data(site, "fr", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  disttown7v <-  wt_max(indicator_data, "OF1")

  dryness7v <- local_moisture_deficit(vals)

  conif7v <- ifelse(sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5) < 2, 0,
                      ifelse(sum(vals$F1_1, vals$F1_3) > 8, 0.8,
                             ifelse(sum(vals$F1_1, vals$F1_3) > 6, 0.6,
                                    ifelse(sum(vals$F1_1, vals$F1_3) > 4, 0.4,
                                           ifelse(sum(vals$F1_1, vals$F1_3) > 2, 0.2, 0)
                                    )
                             )
                      )
  )

  fr_ben_score <- 10* ((4*disttown7v + conif7v + dryness7v)/6)

  fr_ben_score

  }
