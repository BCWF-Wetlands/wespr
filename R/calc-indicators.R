
calc_indicators <- function(site) {
  site <- ws_f(site)
  site <- ws_b(site)
  # site <- sr_f(site)
  # site <- sr_b(site)
  # site <- pr_f(site)
  # site <- pr_b(site)
  site <- cs_f(site)
  # site <- fr_f(site)
  # site <- fr_b(site)
  # site <- sens_f(site)
  # site <- str_f(site)
  # site <- nr_f(site)
  # site <- nr_b(site)
  # site <- ap_f(site)
  # site <- pd_f(site) # requires AP_f
  # site <- kmh_f(site) # requires AP_f * check ref with Paul as unclear
  # site <- kmh_b(site)
  # site <- wb_f(site)  # requires AP_f
  # site <- wb_b(site)
  # site <- pol_f(site) # requires PD_f
  # site <- pol_b(site)
  # site <- rsb_f(site) # requires AP_f, PD_f
  # site <- rsb_b(site)
  # site <- pd_b(site) # requires POL_f, RSB_f
  # site <- oe_f(site) # requires AP_f , CS_f
  # site <- am_f(site) # requires AP_f ,
  # site <- am_b(site) # requires WB_f
  # site <- fh_f(site) # requires AP_f,
  # site <- fh_b(site) # requires WB_f
  # site <- sfts_f(site)
  # site <- sfts_b(site) # requires FH_f
  # site <- ap_b(site) # requires FH_f , AH_f, WB_f, RSB_f
  # site <- cri_b(site) # requires WB_f, FH_f
  site
}
