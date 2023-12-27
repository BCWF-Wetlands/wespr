wt_max <- function(indicator_data, question, type_f_b = c("fun", "ben")) {

  type_f_b <- match.arg(type_f_b)

  data <- dplyr::filter(
    indicator_data,
    .data$no == {{question}},
    .data$type_f_b == {{type_f_b}},
    !is.na(.data$q_weighting),
    !is.na(.data$value)
  )

  # values are stored as a list because they can be different types. For this
  # function they must be numeric.
  data$value <- as.numeric(unlist(data$value))

  max_na(data$value * data$q_weighting) / max_na(data$q_weighting)
}


# Standard functions for OFFICE QUESTIONS



#OF5






#OF10
internal_flow_distance <- function(vals, indicator_data) {
  if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF10", "fun") # TODO confirm range of OF10 responses to include in weighted max. https://github.com/BCWF-Wetlands/wespr/issues/21
  }
}

#OF12
unveg_surface <- function(vals, indicator_data, type_f_b) {
  if (vals$OF11_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF12", type_f_b)
  }
}


#OF25
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



# OF26
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


# OF 42
road_density_wau <- function(vals, indicator_data) {
    if (vals$NoCA == 1) {
      NA_real_
    } else {
      wt_max(indicator_data, "OF12", "ben")
    }
  }








##################################################
# Standard functions for FIELD QUESTIONS
###################################################

#F15
ground_cover <- function(vals, indicator_data) {
  if (vals$F15_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F15", "fun")
  }
}


#F21
persist_water <- function(vals, indicator_data){
  if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F21", "fun")
  }
}


#F25
surface_water_fluctuation <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F25", "fun")
  }
}


#F26
predom_depth_class <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26", "fun")
  }
}

distance_open_water_upland_veg <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26", "fun")
  }
}


#F27
ponded_water <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F27", "fun")
  }
}


#F41

# TO DO - there are multiple calcultions for outflow confinment - listed as issues
# in the mean time I created multiple versions to continue.
#
outflow_confinement <- function(vals, indicator_data) {
  if (vals$NeverWater + vals$TempWet > 0 ||
      vals$NoOutlet + vals$NoOutletX > 0 ||
      vals$F41_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F41", "fun")
  }
}


#https://github.com/BCWF-Wetlands/wespr/issues/17
outflow_confinement_1 <- function(vals, indicator_data) {
  if (vals$NoOutlet + vals$NoOutletX > 0 ||
      vals$F41_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F41", "fun")
  }
}


#tributary_channel_floodplain()






throughflow_resistance <- function(vals, indicator_data) {
  if (vals$Inflow == 0 || (vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F43", "fun")
  }
}

internal_gradient <- function(vals, indicator_data) {
  if ((vals$NoOutlet + vals$NoOutletX) > 0 || vals$Inflow == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F44", "fun")
  }
}


# outflow_confinement()
# constric1 <- dplyr::case_when(
#   # (vals$NeverWater + vals$TempWet) > 0 ~ NA,
#   (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA_real_,
#   vals$F41_4 == 1 ~ NA_real_,
#   .default = wt_max(indicator_data, "F41", "fun")
# )


# F50
vegetation_buffer_along_permin <- function(vals, indicator_data, type_f_b){
  if(vals$Disturb == 0) {
  NA_real_
} else {
  wt_max(indicator_data, "F50", type_f_b)
  }
}

# F51
type_of_cover_buff <- function(vals, indicator_data, type_f_b){
  if(vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F51", type_f_b)
  }
}

# F52
buffer_slope <- function(vals, indicator_data, type_f_b){
  if(vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F52", type_f_b)
  }
}


