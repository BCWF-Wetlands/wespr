wt_max <- function(indicator_data, question) {

  data <- dplyr::filter(
    indicator_data,
    .data$no == {{question}},
    !is.na(.data$q_weighting),
    !is.na(.data$value)
  )

  # values are stored as a list because they can be different types. For this
  # function they must be numeric.
  data$value <- as.numeric(unlist(data$value))

  max_na(data$value * data$q_weighting) / max_na(data$q_weighting)
}

# Placeholder for confirmation
# intact_vals <- function(vals) {
#   intact_vals <- c(vals$OF30_1, vals$OF30_2, vals$OF30_3,
#                  vals$OF31_1, vals$OF31_2, vals$OF31_3,
#                  vals$OF32_1, vals$OF32_2, vals$OF32_3, vals$OF32_4, vals$OF32_5,
#                  vals$OF33_1, vals$OF33_2, vals$OF33_3, vals$OF33_4, vals$OF33_5,
#                  vals$OF34_1, vals$OF34_2, vals$OF34_3,
#                  vals$OF35_1,
#                  vals$OF36_1, vals$OF36_2, vals$OF36_3, vals$OF36_4,
#                  vals$OF37_1, vals$OF37_2, vals$OF37_3, vals$OF37_4, vals$OF37_5,
#                  vals$OF38_1, vals$OF38_2, vals$OF38_3, vals$OF38_4, vals$OF38_5,
#                  vals$OF39_1, vals$OF39_2, vals$OF39_3, vals$OF39_4, vals$OF39_5,
#                  vals$OF40_1, vals$OF40_2, vals$OF40_3, vals$OF40_4, vals$OF40_5,
#                  vals$OF41_1, vals$OF41_2, vals$OF41_3, vals$OF41_4, vals$OF41_5,
#                  vals$OF42_1, vals$OF42_2, vals$OF42_3,
#                  vals$OF43_1, vals$OF43_2, vals$OF43_3, vals$OF43_4, vals$OF43_5)
#
#   intact_vals
#
# }


# Standard functions for OFFICE QUESTIONS

#OF5
stream_intersect <- function(vals){
  if ((vals$NoOutlet + vals$NoOutletX) > 0) {
     1
  } else {
  vals$OF6_1
  }
}


#OF3
dist_to_ponded_water <- function(vals, indicator_data) {
  if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF3") }
}



#OF10
internal_flow_distance <- function(vals, indicator_data) {
  if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF10")
  }
}

#OF12
unveg_surface <- function(vals, indicator_data) {
  if (vals$OF11_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF12")
  }
}


#OF12 # STR
unveg_surface_1 <- function(vals, indicator_data) {
  if (vals$NoCA == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "OF12")
  }
}


# OF20
fish_occurance <- function(vals){
  ifelse(vals$OF20_5 == 1, 0,
       ifelse(vals$OF20_4 == 1, 0.5,
              max(c(vals$OF20_1, vals$OF20_2, vals$OF20_3)) / 3
       )
  )

}



#OF25
# Todo : this will need to be generalised to create for future averages
# https://github.com/BCWF-Wetlands/wespr/issues/70

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

# Todo : this will need to be generalised to create for future averages
# https://github.com/BCWF-Wetlands/wespr/issues/70

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

#OF 27
# Todo : this will need to be generalised to create for future averages
# https://github.com/BCWF-Wetlands/wespr/issues/70

local_solar_input <- function(vals){
  ifelse(vals$OF27_1 == 0, NA_real_,
  dplyr::case_when(
    vals$GDeco == 1 ~ (vals$OF27_1 - 28.7) / 14.4,
    vals$CMeco == 1 ~ (vals$OF27_1 - 26.3) / 11,
    vals$SIMeco == 1 ~ (vals$OF27_1 - 32.3) / 14.2,
    vals$BPeco == 1 ~ (vals$OF27_1 - 32.1) /6.4,
    vals$TPeco == 1 ~ (vals$OF27_1 - 30.1) / 7.1,
    .default = NA_real_
  ))
}








# OF 42
road_density_wau <- function(vals, indicator_data) {
    if (vals$NoCA == 1) {
      NA_real_
    } else {
      wt_max(indicator_data, "OF42")
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
    wt_max(indicator_data, "F15")
  }
}


#F20
percent_flooded_only_seasonally <- function(vals, indicator_data) {
  if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F20")
  }
}




#F21
persist_water <- function(vals, indicator_data){
  if (vals$NeverWater == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F21")
  }
}




# 24
percent_summerwater_shaded <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F24")
  }
}




#F25
surface_water_fluctuation <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F25")
  }
}


#F26
predom_depth_class <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26")
  }
}

#F26 _ version 2 (Sens)
predom_depth_class_1 <- function(vals, indicator_data) {
  if ((vals$NeverWater + vals$TempWet > 0) ||
      vals$TempWet == 1 ||
      vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26")
  }
}



#F26 _ version 3 (PD)
predom_depth_class_2 <- function(vals, indicator_data) {
  if ((vals$NeverWater == 1) ||
      vals$TempWet == 1 ||
      vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F26")
  }
}




#F27
ponded_water <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F27")
  }
}

#F28
non_veg_aquatic_cover <- function(vals, indicator_data) {
  if((vals$NeverWater + vals$TempWet > 0) ||
      vals$NoPersis == 1 ||
      vals$NoPond == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F28")
  }
}


#F28 : version 2
non_veg_aquatic_cover_1 <- function(vals, indicator_data) {
  if((vals$NeverWater == 0) ||
     vals$NoPersis == 1 ||
     vals$NoPond == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F28")
  }
}


#F29
largest_deep_pond_acre <- function(vals, indicator_data) {
  if((vals$NeverWater == 0) ||
     vals$NoPersis == 1 ||
     vals$NoPond == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F29")
  }
}





#F30
largest_deep_pond <- function(vals, indicator_data) {
  if((vals$NeverWater + vals$TempWet > 0) ||
     vals$NoPersis == 1 ||
     vals$NoPond == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F30")
  }
}


# F31
open_water_extent <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 ||
      vals$NoPersis == 1 ||
      vals$NoDeepPonded == 1  ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F31")
  }
}

# F31 - version 2
open_water_extent_1 <- function(vals, indicator_data) {
  if (vals$NeverWater == 0 ||
      vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F31")
  }
}





# F32
distance_across_longest_openwater <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 ||
      vals$NoPersis == 1 ||
      vals$NoDeepPonded == 1  ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F32")
  }
}
# F32 - version 2
distance_across_longest_openwater_1 <- function(vals, indicator_data) {
  if (vals$NeverWater + vals$TempWet >0 ||
      vals$NoPersis == 1 ||
      vals$NoDeepPonded == 1 ||
      vals$NoOW == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F32")
  }
}



# F33
distance_open_water_upland_veg <- function(vals, indicator_data) {
  if (vals$NeverWater == 1 || vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }
}


# F33 : version 2 (sens)/ NR
distance_open_water_upland_veg_1 <- function(vals, indicator_data) {
  if((vals$NeverWater + vals$TempWet > 0) ||
      vals$NoPond == 1 ||
      vals$NoDeepPonded == 1 ||
      vals$NoOW == 1 ||
      vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }
}


# F33 : version 3 / PD
distance_open_water_upland_veg_2 <- function(vals, indicator_data) {
  if(vals$NeverWater == 1 ||
     vals$NoPond == 1 ||
     vals$NoDeepPonded == 1 ||
     vals$NoOW == 1 ||
     vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }
}

# F33 : version 4 / WB
distance_open_water_upland_veg_3 <- function(vals, indicator_data) {
  if(vals$NeverWater ==1 ||
     vals$NoDeepPonded == 1 ||
     vals$NoOW == 1 ||
     vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }
}


# F33 : version 4 / RSB
distance_open_water_upland_veg_4 <- function(vals, indicator_data) {
  if((vals$NeverWater + vals$TempWet > 0) ||
     vals$NoDeepPonded == 1 ||
     vals$NoOW == 1 ||
     vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F33")
  }
}



# F35
interspersion_inundated_veg <- function(vals, indicator_data){
   if((vals$NeverWater + vals$TempWet > 0) ||
       vals$NoDeepPonded == 1 ||
       vals$NoOW == 1 ||
       vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F35")
  }
}

# F35 (AP), wb
interspersion_inundated_veg_1 <- function(vals, indicator_data){
  if(vals$NeverWater == 1 ||
     vals$NoDeepPonded == 1 ||
     vals$NoOW == 1 ||
     vals$NoPersis == 1 ) {
    NA_real_
  } else {
    wt_max(indicator_data, "F35")
  }
}


# F35 : version 3 - FH
interspersion_inundated_veg_2 <- function(vals){
  ifelse(vals$NeverWater == 1, NA_real_,
                 ifelse(vals$NoPersis == 1, NA_real_,
                        ifelse(vals$NoPond == 1, NA_real_,
                               ifelse(vals$F35_1 == 1, 0, NA_real_))))
}




# F37
inundated_erect_veg <- function(vals, indicator_data){
  if (vals$NeverWater == 1 ||
      vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F37")
  }
}



# F38
submerged_floating_aquatics <- function(vals, indicator_data){
  if (vals$NeverWater == 1 ||
      vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F38")
  }
}
# F38 - version2 (PD)
submerged_floating_aquatics_1 <- function(vals){
  if (vals$NeverWater == 1 ||
      vals$NoPersis == 1 ||
      vals$NoDeepPonded == 1 ||
      vals$NoPond == 1||
      vals$F38_2 != 1) {
    NA_real_
  } else {
    1
  }
}


# F38- version3 (AM)
submerged_floating_aquatics_2 <- function(vals, indicator_data){
  if (vals$NeverWater + vals$TempWet == 1 ||
      vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F38")
  }
}


# F39
water_color <- function(vals, indicator_data){
  if (vals$NeverWater == 1 ||
      vals$NoPersis == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F39")
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
    wt_max(indicator_data, "F41")
  }
}


#https://github.com/BCWF-Wetlands/wespr/issues/17
outflow_confinement_1 <- function(vals, indicator_data) {
  if (vals$NoOutlet + vals$NoOutletX > 0 ||
      vals$F41_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F41")
  }
}

#https://github.com/BCWF-Wetlands/wespr/issues/17 # sens
outflow_confinement_2 <- function(vals, indicator_data) {
  if (vals$NeverWater + vals$TempWet > 0 ||
      vals$NoOutlet ==1 ||
      vals$F41_4 == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F41")
  }
}
#tributary_channel_floodplain()





# F43
throughflow_resistance <- function(vals, indicator_data) {
  if (vals$Inflow == 0 || (vals$NoOutlet + vals$NoOutletX) > 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F43")
  }
}

#44
internal_gradient <- function(vals, indicator_data) {
  if ((vals$NoOutlet + vals$NoOutletX) > 0 || vals$Inflow == 1) {
    NA_real_
  } else {
    wt_max(indicator_data, "F44")
  }
}


# outflow_confinement()
# constric1 <- dplyr::case_when(
#   # (vals$NeverWater + vals$TempWet) > 0 ~ NA,
#   (vals$NoOutlet + vals$NoOutletX) > 0 ~ NA_real_,
#   vals$F41_4 == 1 ~ NA_real_,
#   .default = wt_max(indicator_data, "F41")
# )


# F50
vegetation_buffer_along_permin <- function(vals, indicator_data){
  if(vals$Disturb == 0) {
  NA_real_
} else {
  wt_max(indicator_data, "F50")
  }
}

# F51
type_of_cover_buff <- function(vals, indicator_data){
  if(vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F51")
  }
}

# F52
buffer_slope <- function(vals, indicator_data){
  if (vals$Disturb == 0) {
    NA_real_
  } else {
    wt_max(indicator_data, "F52")
  }
}

get_indicator_score <- function(site, indicator, type) {
  check_wesp_site(site)

  match.arg(type, c("fun", "ben"))

  if (!indicator %in% names(site$indicators)) {
    stop(indicator, " is not a valid indicator")
  }
  score <- get_score_value(site$indicators[[indicator]][[type]])

  if (is.null(score)) {
    stop(indicator, " has not been calculated yet")
  }

  score
}
