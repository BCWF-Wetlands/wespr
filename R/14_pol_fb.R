
pol_fun <- function(site) {

  indicator_data <- get_indicator_data(site, "pol")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  imperv16 <- wt_max(indicator_data, "OF12", "fun")

  intact16 <- if(sum_na (vals$OF32_1, vals$OF32_2, vals$OF32_3, vals$OF32_4, vals$OF32_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32", "fun")
  }

  lcrich16 <- if(sum_na (vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36", "fun")
  }

  lcrich2k16 <- if(sum_na (vals$OF37_1, vals$OF37_2, vals$OF37_3, vals$OF37_4, vals$OF37_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37", "fun")
  }

  willow16 <- wt_max(indicator_data, "F5", "fun")

  flower16 <- wt_max(indicator_data, "F6", "fun")

  snags16 <- if(sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6)==0){
      NA_real_
    } else {
      wt_max(indicator_data, "F8", "fun")
    }

  wooddown16 <- if(sum(vals$F1_1, vals$F1_2, vals$F1_3, vals$F1_4, vals$F1_5, vals$F1_6)==0){
    NA_real_
  } else {
    wt_max(indicator_data, "F9", "fun")
  }

  forb16 <- wt_max(indicator_data, "F11", "fun")

  gcover16 <- if(vals$F15_4 == 1){
      NA_real_
    } else {
      wt_max(indicator_data, "F15", "fun")
}

  girreg16 <- wt_max(indicator_data, "F18", "fun")

  permwpct16 <- wt_max(indicator_data, "F21", "fun")

   # TO DO - note these two values outlet16 and outmap16 rely on each other - might need to add another if
  # else statement to try and prevent circular refernce.

  outlet16 <- if(vals$F40_4 + vals$F40_5 > 0) {
    outmap16
      }else { wt_max(indicator_data, "F40", "fun")
    }

  outmap16 <- if (vals$NoOutlet + vals$NoOutletX == 0) {
    vals$OF6_1
  } else {
    outlet16
  }

 perimpctper16 <- if(vals$Disturb == 0 ){
   1
 } else {
   wt_max(indicator_data, "F50", "fun")
 }

 distnest16 <- wt_max(indicator_data, "F53", "fun")

 toxic16 <- vals$S3_subscore

 pdscore16 <- get_indicator_score(site, "pd", "fun")


  # pol subscores
  pollen <- max_na(c(max_na(c(willow16, flower16, forb16)), pdscore16))

  nestsites <- mean_na(c(permwpct16,
                       mean_na(c(snags16, wooddown16, girreg16, distnest16)),
                       outlet16))

  stress16 <- mean_na(c(toxic16, perimpctper16, imperv16, intact16))


  pol_fun_score <- 10 * (ifelse(vals$AllPermW == 1, 0, (3* pollen + 2* nestsites + stress16/6)))


  pol_fun_score

}

# calculate benefits

pol_ben <- function(site){

  indicator_data <- get_indicator_data(site, "pol")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)

  rareplant16v <- ifelse(vals$OF24_1 == 1, 1, NA_real_)

  intact16v <- if(sum_na (vals$OF32_1, vals$OF32_2, vals$OF32_3, vals$OF32_4, vals$OF32_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF32", "ben")
  }

  lcrich16v <- if(sum_na (vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF36", "ben")
  }

  lcrich2k16v <- if(sum_na (vals$OF37_1, vals$OF37_2, vals$OF37_3, vals$OF37_4, vals$OF37_5) == 0){
    NA_real_
  } else {
    wt_max(indicator_data, "OF37", "ben")
  }

  perimpctper16v <- if(vals$Disturb == 0 ){
    1
  } else {
    wt_max(indicator_data, "F50", "ben")
  }

  pol_ben_score <- 10 * mean_na(c(perimpctper16v, intact16v, lcrich16v,lcrich2k16v , rareplant16v))

  pol_ben_score
}
