wt_max <- function(indicator_data, question, type_f_b = c("function", "benefit")) {

  type_f_b <- match.arg(type_f_b)

  data <- dplyr::filter(
    indicator_data,
    .data$no == {{question}},
    tolower(.data$type_f_b) == tolower(type_f_b)
  )

  max_na(data$value * data$q_weighting) / max_na(data$q_weighting)
}

local_moisture_deficit <- function(vals) {
  #GDeco = OF44_1 # georgia depression
  #CMeco = OF44_2 # coast and mountain (CM)
  #SIMeco = OF44_3 # southern interior Mts
  #BPeco = OF44_4 # Boreal Plains
  #TPeco = OF44_5 # Taiga Plains

  dplyr::case_when(
    vals$OF25_1 <= 0 ~ NA,
    vals$GDeco == 1 ~ (vals$OF25_1 - 0) / 329,
    vals$CMeco == 1 ~ (vals$OF25_1 - 0) / 326,
    vals$SIMeco == 1 ~ (vals$OF25_1 - 0) / 825,
    vals$BPeco == 1 ~ (vals$OF25_1 - 24) / 381,
    vals$TPeco == 1 ~ (vals$OF25_1 - 0) / 219,
    .default = NA
  )
}

update_site_indicator <- function(site, indicator, type = c("func", "benefit"), value) {
  check_wesp_site(site)
  if (!indicator %in% names(indicators())) {
    stop("Invalid site: ", indicator, call. = FALSE)
  }
  type <- match.arg(type)

  site$indicators[[indicator]][[type]] <- value
  site
}
