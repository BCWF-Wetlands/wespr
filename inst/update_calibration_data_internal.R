## Internal function to generate comparison plots for comparison of ecoprovinces
## last updated March 2026

load_all()
library(dplyr)

#
# #SIM data - read in SIM reference data
#
# simref <- load_wesp_data(system.file("input_data/reference_SIM_20250620.csv", package = "wespr"))
# simref1 <- load_wesp_data(system.file("input_data/reference_SIM_20260331.csv", package = "wespr"))
#
# simref <- read.csv(system.file("input_data/reference_SIM_20250620.csv", package = "wespr"))
# simref1 <- read.csv(system.file("input_data/reference_SIM_20260331.csv", package = "wespr"))
#
#
#
# col_order <- c(simref[505,])
#
#



# # run the base scores for comparison
# base_score <- calculate_jenks_score(simref, out_dir = "temp", out_name = "wesp_sim_scores_base.csv")
#
# base_score <-base_score |>
#   dplyr::mutate(ecoprovince = "SIM") |>
#   dplyr::select(-wetland_id)
#
#
# # GD scores - read in GD reference data
#
# gdref <- load_wesp_data(system.file("input_data/reference_GD_20250620.csv", package = "wespr"))
#
# # run the base scores for comparison
# base_score_gd <- calculate_jenks_score(gdref, out_dir = "temp", out_name = "wesp_gd_scores_base.csv")
#
# base_score_gd <- base_score_gd |>
#   mutate(ecoprovince = "GD") |>
#   select(-wetland_id)
#
#
#
# #SBI data - read in SIM reference data
#
# smiref <- load_wesp_data(system.file("input_data/reference_SBI_20260319.csv", package = "wespr"))
# site <- as.wesp_site(smiref, site = 62)
# site <- calc_indicators(site)
#
#
#
#
#
#
# # run the base scores for comparison
# base_score <- calculate_jenks_score(simref, out_dir = "temp", out_name = "wesp_sim_scores_base.csv")
#
# base_score <-base_score |>
#   dplyr::mutate(ecoprovince = "SIM") |>
#   dplyr::select(-wetland_id)
#
#
#
# # merge both together
#
# calibration_scores <- bind_rows(base_score_gd, base_score)
#
#
# # update the dataset back to package
# usethis::use_data(calibration_scores, overwrite = TRUE)
#
#



###############################################

# Compare variation in ecoprovince calibration scores

library(ggplot2)

cals <- calibration_scores

# get number of ecoprovinces
unique(cals$ecoprovince)

wcols <- names(calibration_scores)

## remove the site column
wcols <- wcols[!wcols %in% c("site", "wetland_id","ecoprovince")]
wcols <- unique(sub("^([^_]*_[^_]*).*", "\\1", wcols))



# 1) loop through the data and get the min and max values of the raw scores

outsum <- purrr::map(wcols, function(x) {
  # get the columns for each service
   #x <- wcols[1]

  tw <- calibration_scores |>
    group_by(ecoprovince) |>
    dplyr::select(dplyr::starts_with(x), ecoprovince) |>
    dplyr::select(-dplyr::ends_with("_norm"))
  names(tw) <- c("jenks", "raw", "ecoprovince" )

  tww <- tw |>
    #dplyr::group_by(jenks) |>
    #dplyr::summarise(n = n(),
    #                  min = min(raw),
    #                  max = max(raw)) |>
    dplyr::mutate(service = x,
                  service_name = unique(sub("^([^_]*).*", "\\1", service)),
                  service_type = unique(sub("^[^_]*_", "", service)))
  tww

}) |>  dplyr::bind_rows()


outsum <- outsum|>
  dplyr::mutate(jenks = ordered(jenks, levels = c("H", "M", "L"))) |>
  mutate(service_full_name = case_when(
    service_name == "AM" ~ "Amphibian Habitat (AM)",
    service_name == "APP" ~ "Aquatic Primary Productivity (APP)",
    service_name == "CP" ~ "Carbon Preservation (CP)",
    service_name == "CRI" ~ "Cultural Recreational Importance (CRI)",
    service_name == "FH" ~ "Fish Habitat (FH)",
    service_name == "FR" ~ "Fire Resistance (FR)",
    service_name == "KMH" ~ "Keystone Mammal Habitat (KMH)",
    service_name == "NR" ~ "Nitrate Removal and Retention (NR)",
    service_name == "OE" ~ "Organic Matter Export (OE)",
    service_name == "PD" ~ "Native Plant Diversity (PD)",
    service_name == "POL" ~ "Pollinator Habitat (POL)",
    service_name == "PR" ~ "Phosphorus Retention (PR)",
    service_name == "RSB" ~ "Raptor and Wetland Songbird Habitat (RSB)",
    service_name == "SENS" ~ "Wetland Sensitivity (SENS)",
    service_name == "SFTS" ~ "Stream Flow and Temperature Support (SFTS)",
    service_name == "SR" ~ "Sediment Retention and stabilization (SR)",
    service_name == "STR" ~ "Wetland Stressors (STR)",
    service_name == "WB" ~ "Waterbird Habitat (WB)",
    service_name == "WS" ~ "Water Storage and Delay (WS)"))



# grouped scores
outsum_f <- outsum |>
  dplyr::filter(service_type == "f")

outsum_b <- outsum |>
  dplyr::filter(service_type == "b")


# 1) compare the histograpms - FUNCTIONS

inds <- unique(outsum_f$service_full_name)

# need to reform these plots so they are visible

outplots <- purrr::map(inds, function(x){

  #x<-inds[1]

  outsum_fi <- outsum_f |>
    filter(service_full_name == x)
  outname <- unique(outsum_fi$service)

  pf <- ggplot(outsum_fi, aes(x = raw , fill = jenks)) +
    geom_histogram( alpha=0.6, position = 'identity')+
    scale_fill_viridis_d()+
    geom_density(data=outsum_fi, aes(x=raw, group=jenks, fill=jenks), adjust=1.5, alpha=.4) +
    facet_wrap(~service_full_name + ecoprovince, scales = "free_y") +
    labs(title = paste0("Calibration Sites: ", x),
       x = "Function Score",
       y = "Number of sites",
       fill = "Rating") +
  guides(col= guide_legend(title= "Class"))+
  theme_minimal()

 ggsave(fs::path("temp", paste0("calibration_plot_",outname , ".png")), pf)

})




# 1) compare the histograpms - BENEFITS
#
# ggplot(outsum_b, aes(x = raw , fill = jenks)) +
#   geom_histogram( alpha=0.6, position = 'identity')+
#   scale_fill_viridis_d()+
#   geom_density(data=outsum_b, aes(x=raw, group=jenks, fill=jenks), adjust=1.5, alpha=.4) +
#   facet_wrap(~service_full_name + ecoprovince, scales = "free_y") +
#   labs(title = "Calibration Sites threshold values for Ecosystem Benefits",
#        x = "Benefit Score",
#        y = "Number of sites") +
#   guides(col= guide_legend(title= "Class"))+
#   theme_minimal()


inds <- unique(outsum_b$service_full_name)

# need to reform these plots so they are visible

outplots <- purrr::map(inds, function(x){

  #x<-inds[1]

  outsum_bi <- outsum_b |>
    filter(service_full_name == x)
  outname <- unique(outsum_bi$service)

  pf <- ggplot(outsum_bi, aes(x = raw , fill = jenks)) +
    geom_histogram( alpha=0.6, position = 'identity')+
    scale_fill_viridis_d()+
    geom_density(data=outsum_bi, aes(x=raw, group=jenks, fill=jenks), adjust=1.5, alpha=.4) +
    facet_wrap(~service_full_name + ecoprovince, scales = "free_y") +
    labs(title = paste0("Calibration Sites: ", x),
         x = "Benefit Score",
         y = "Number of sites",
         fill = "Rating") +
    guides(col= guide_legend(title= "Class"))+
    theme_minimal()

  ggsave(fs::path("temp", paste0("calibration_plot_",outname , ".png")), pf)

})




# 2) compare the thresholds


outsum_th <- purrr::map(wcols, function(x) {
  # get the columns for each service
 # x <- wcols[15]

  tw <- calibration_scores |>
    group_by(ecoprovince) |>
    dplyr::select(dplyr::starts_with(x), ecoprovince) |>
    dplyr::select(-dplyr::ends_with("_norm"))
  names(tw) <- c("jenks", "raw", "ecoprovince" )

  tww <- tw |>
    dplyr::group_by(jenks, ecoprovince) |>
    dplyr::summarise(n = n(),
                      min = min(raw),
                      max = max(raw)) |>
    dplyr::mutate(service = x,
                  service_name = unique(sub("^([^_]*).*", "\\1", service)),
                  service_type = unique(sub("^[^_]*_", "", service)))
  tww

}) |>  dplyr::bind_rows()


tt <- outsum_th |>
  group_by(service, ecoprovince) |>
  select(-n) |>
  tidyr::pivot_wider(names_from = jenks, values_from = c(min, max)) |>
  dplyr::mutate(
    H = 10.1 - min_H,
    M = min_H - min_M,
    L = min_M
  )

ttt <- tt |>
  tidyr::pivot_longer(
    cols = c(H, M, L, -min_H, -min_L, -min_M, -max_H, -max_L, -max_M),
    names_to = "catergory",
    values_to = "threshold"
  ) |>
  dplyr::select(-c(min_H, min_L, min_M, max_H, max_L, max_M))

ttt <- ttt |>
  dplyr::mutate(class = ordered(catergory, levels = c("H", "M", "L"))) |>
  mutate(service_full_name = case_when(
    service_name == "AM" ~ "Amphibian Habitat (AM)",
    service_name == "APP" ~ "Aquatic Primary Productivity (APP)",
    service_name == "CP" ~ "Carbon Preservation (CP)",
    service_name == "CRI" ~ "Cultural Recreational Importance (CRI)",
    service_name == "FH" ~ "Fish Habitat (FH)",
    service_name == "FR" ~ "Fire Resistance (FR)",
    service_name == "KMH" ~ "Keystone Mammal Habitat (KMH)",
    service_name == "NR" ~ "Nitrate Removal and Retention (NR)",
    service_name == "OE" ~ "Organic Matter Export (OE)",
    service_name == "PD" ~ "Native Plant Diversity (PD)",
    service_name == "POL" ~ "Pollinator Habitat (POL)",
    service_name == "PR" ~ "Phosphorus Retention (PR)",
    service_name == "RSB" ~ "Raptor and Wetland Songbird Habitat (RSB)",
    service_name == "SENS" ~ "Wetland Sensitivity (SENS)",
    service_name == "SFTS" ~ "Stream Flow and Temperature Support (SFTS)",
    service_name == "SR" ~ "Sediment Retention and stabilization (SR)",
    service_name == "STR" ~ "Wetland Stressors (STR)",
    service_name == "WB" ~ "Waterbird Habitat (WB)",
    service_name == "WS" ~ "Water Storage and Delay (WS)"
  ))

#

# functions
ft <- ttt |>
  dplyr::filter(service_type == "f")


fn_plot <- ggplot(ft, aes(x = threshold, y = service_full_name)) +
  geom_bar(aes(fill = class), stat = "identity", position = "stack") +
  scale_fill_viridis_d() +
  facet_wrap(~ecoprovince) +
  # geom_text(data= fclass,aes(x= max,y=service_full_name,label=n), color = "darkgrey",vjust=0) +
 # geom_point(data = fclass, aes(x = threshold, y = service_full_name, size = 1.25), colour = "darkgrey") +
  labs(
    x = "Ecosystem Score",
    y = "Ecosystem Function",
    fill = "Rating"
  ) +
  scale_size(guide = 'none')+
  theme_minimal()

fn_plot




# Benefits
ftb <- ttt |>
  dplyr::filter(service_type == "b")



bn_plot <- ggplot(ftb, aes(x = threshold, y = service_full_name)) +
  geom_bar(aes(fill = class), stat = "identity", position = "stack") +
  scale_fill_viridis_d() +
  facet_wrap(~ecoprovince) +
  # geom_text(data= fclass,aes(x= max,y=service_full_name,label=n), color = "darkgrey",vjust=0) +
  # geom_point(data = fclass, aes(x = threshold, y = service_full_name, size = 1.25), colour = "darkgrey") +
  labs(
    x = "Ecosystem Score",
    y = "Ecosystem Benefit",
    fill = "Rating"
  ) +
  scale_size(guide = 'none')+
  theme_minimal()

bn_plot



##########################################################################
# Generate a table for summary of values


ical <- calibration_scores |>
  filter(ecoprovince== "GD")


ical


# get list of col types
wcols <- names(calibration_scores)
wcols <- wcols[!wcols %in% c("site", "wetland_id","ecoprovince")]
wcols <- unique(sub("^([^_]*_[^_]*).*", "\\1", wcols))



# 1) loop through the data and get the min and max values of the raw scores

outsum <- purrr::map(wcols, function(x) {
  # get the columns for each service
 # x <- wcols[1]
  tw <- calibration_scores |>
    group_by(ecoprovince) |>
    dplyr::select(dplyr::starts_with(x), ecoprovince) |>
    dplyr::select(-dplyr::ends_with("_norm"))

  names(tw) <- c("jenks", "raw", "ecoprovince" )

  tww <- tw |>
    group_by(ecoprovince) |>
    mutate(lowest_score = min(raw)) |>
    mutate(highest_score = max(raw))

  twww <- tww |>
    dplyr::group_by(ecoprovince,jenks) |>
    dplyr::summarise(n = n(),
                      min = min(raw),
                      max = max(raw)) |>
    filter(jenks =="M") |>
    rename(low_med_break = min,
           med_high_break = max) |>
    select(ecoprovince,low_med_break ,med_high_break)

  tww_sum <- tww |>
    dplyr::select(ecoprovince, lowest_score, highest_score) |>
    dplyr::mutate(service = x,
                  service_name = unique(sub("^([^_]*).*", "\\1", service)),
                  service_type = unique(sub("^[^_]*_", "", service))) |>
    unique()

  twww_sum <- left_join(tww_sum,twww)#, join_by = "ecoprovince")
  twww_sum <- twww_sum |>
    select(ecoprovince , service_name, service_type, lowest_score ,low_med_break,
           med_high_break, highest_score )

}) |>  dplyr::bind_rows()


outsum <- outsum |>
  mutate(across(where(is.numeric), ~round(.x, digits = 1)))


write.csv(outsum, file.path("temp", "summary_cal_thresholds.csv"))



# check sim values

sim <- read.csv(fs::path("inst", "input_data", "archive", "SIM_30_March_2026_11_43_wesp_scores.csv"))


sio <- calibration_scores |>
  filter(ecoprovince =="SIM")

sio <- sio |>
  arrange(wetland_id)
all <- left_join(sio, sim, by = "wetland_id")
df_sorted_base <- all [, order(colnames(all ))]
