wt_max <- function(indicator_data, question, type_f_b = c("func", "benefit")) {

  type_f_b <- match.arg(type_f_b)

  if (type_f_b == "func") type_f_b <- "function"

  data <- dplyr::filter(
    indicator_data,
    .data$no == {{question}},
    tolower(.data$type_f_b) == tolower({{type_f_b}})
  )

  # values are stored as a list because they can be different types. For this
  # function they must be numeric.
  data$value <- as.numeric(unlist(data$value))

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

update_site_indicator <- function(site, indicator, type = c("func", "benefit")) {
  check_wesp_site(site)

  if (!indicator %in% names(indicators()) ||
      !exists(indicator, where = site$indicators)) {
    stop("Invalid indicator: ", indicator, call. = FALSE)
  }

  type <- match.arg(type)

  if (!exists(type, where = site$indicators[[indicator]])) {
    stop("'", type, "' is not a valid type for indicator '", indicator, "'",
         call. = FALSE)
  }

  # This is a bit fragile - make the name of the function from indicator and type
  # args, and call it with do.call:
  indicator_fun <- paste(indicator, type, sep = "_")
  value <- do.call(indicator_fun, list(site = site))

  existing_value <- site$indicators[[indicator]][[type]]

  if (!is.null(existing_value)) {
    warning("'", indicator, ":", type, "' has exisiting value: ", existing_value, ". ",
            "Replacing it with ", round(value, 2))
  }

  site$indicators[[indicator]][[type]] <- value

  site
}
