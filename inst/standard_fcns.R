
# generic functions

library(tidyverse)

# read in data and filter to questions we have implemented, and just one site:
data <- load_wesp_data("inst/input_data/wetflat.csv") |>
  select(q_no, response_no, site_1) |>
  filter(
    q_no %in% names(core_questions())
  )



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

