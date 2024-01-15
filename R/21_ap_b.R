
app_ben <- function(site) {

  indicator_data <- get_indicator_data(site, "ap", "ben")
  vals <- get_vals(indicator_data)

  elev8v <- if (vals$NoOutlet == 1) {
    NA_real_
  } else {
    vals$OF5_1
  }

  fscorefh8v <- get_indicator_score(site, "fh", "fun") / 10

  fscoream8v <- get_indicator_score(site, "am", "fun") / 10

  fscorewb8v <- get_indicator_score(site, "wb", "fun") / 10

  fscoreSBM8v <- get_indicator_score(site, "rsb", "fun") / 10


  ap_ben_score <- 10 * (mean_na(c(elev8v,
                                 mean_na(c(fscorefh8v, fscoream8v, fscorewb8v, fscoreSBM8v)))))

  ap_ben_score

}
