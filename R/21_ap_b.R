
ap_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "ap", "ben")
  vals <- get_vals(indicator_data)
  weights <- get_weights(indicator_data)
  ind_scores <- get_indicator_scores(site)


  elev8v <- if(vals$NoOutlet == 1) {
      NA_real_
  } else {
    vals$OF5_1
      }


  # UP TO HERE ..............

  # to do : check how to retrieve the final score
  # is there a more elegant way to extract these

  # fscorefh8v <- ind_scores$indicator

  #fscoream8v <-

  #fscorewb8v <-

  #fscoreSBM8v <-


  ap_ben_score <- 10 * (mean_na (elev8v,
                                 mean_na(fscorefh8v, fscoream8v, fscorewb8v, fscoreSBM8v)))

  ap_ben_score

}
