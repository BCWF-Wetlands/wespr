wt_max <- function(indicator_data, question, type_f_b = c("function", "benefit")) {

  type_f_b <- match.arg(type_f_b)

  data <- dplyr::filter(
    indicator_data,
    .data$no == {{question}},
    tolower(.data$type_f_b) == tolower({{type_f_b}}),
    !is.na(.data$q_weighting),
    !is.na(.data$value)
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
    vals$OF25_1 <= 0 ~ NA_real_,
    vals$GDeco == 1 ~ (vals$OF25_1 - 0) / 329,
    vals$CMeco == 1 ~ (vals$OF25_1 - 0) / 326,
    vals$SIMeco == 1 ~ (vals$OF25_1 - 0) / 825,
    vals$BPeco == 1 ~ (vals$OF25_1 - 24) / 381,
    vals$TPeco == 1 ~ (vals$OF25_1 - 0) / 219,
    .default = NA_real_
  )
}

degree_days_index <- function(vals) {
  dplyr::case_when(
    vals$OF26_1 == 0 ~ NA_real_,
    vals$GDeco == 1 ~ (vals$OF26_1 - 931) / 1545,
    vals$CMeco == 1 ~ (vals$OF26_1 - 238) / 1475,
    vals$SIMeco == 1 ~ (vals$OF26_1 - 205) / 2279,
    vals$BPeco == 1 ~ (vals$OF26_1 - 720) / 1114,
    vals$TPeco == 1 ~ (vals$OF26_1 - 487) / 957,
    .default = NA_real_
  )
}

topo_position <- function(vals) {
  switch(
    vals$OF29_1,
    "T" = 5,
    "L" = 4,
    "D" = 3,
    "M" = 2,
    `NA_character_` = NA_character_,
    0
  ) / 5
}

surface_water_fluctuation <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F25", "function")
  }
}

ponded_water <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F27", "function")
  }
}

outflow_confinement <- function(vals, indicator_data) {
  if (vals$NeverWater + vals$TempWet > 0 ||
      vals$NoOutlet + vals$NoOutletX > 0 ||
      vals$F41_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F41", "function")
  }
}

throughflow_resistance <- function(vals, indicator_data) {
  if (vals$Inflow == 0 || (vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F43", "function")
  }
}

internal_gradient <- function(vals, indicator_data) {
  if ((vals$NoOutlet + vals$NoOutletX) > 0 || vals$Inflow == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F44", "function")
  }
}

unveg_surface <- function(vals, indicator_data, type_f_b) {
  if (vals$OF11_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF12", type_f_b)
  }
}
