
calc_indicators <- function(site) {
  site <- update_site_indicator(site, "ws", "function")
  site <- update_site_indicator(site, "ws", "benefit")
  site <- update_site_indicator(site, "sr", "function")
  site <- update_site_indicator(site, "sr", "benefit")
  # site <- update_site_indicator(site, "pr", "function")
  # site <- update_site_indicator(site, "pr", "benefit")
  site <- update_site_indicator(site, "cs", "function")
  # site <- update_site_indicator(site, "fr", "function")
  # site <- update_site_indicator(site, "fr", "benefit")
  # site <- update_site_indicator(site, "sens", "function")
  # site <- update_site_indicator(site, "str", "function")
  # site <- update_site_indicator(site, "nr", "function")
  # site <- update_site_indicator(site, "nr", "benefit")
  # site <- update_site_indicator(site, "ap", "function")
  # site <- update_site_indicator(site, "pd", "function") # requires ap_func
  # site <- update_site_indicator(site, "kmh", "function") # requires ap_func * check ref with Paul as unclear
  # site <- update_site_indicator(site, "kmh", "benefit")
  # site <- update_site_indicator(site, "wb", "function")  # requires ap_func
  # site <- update_site_indicator(site, "wb", "benefit")
  # site <- update_site_indicator(site, "pol", "function") # requires pd_func
  # site <- update_site_indicator(site, "pol", "benefit")
  # site <- update_site_indicator(site, "rsb", "function") # requires ap_func, pd_func
  # site <- update_site_indicator(site, "rsb", "benefit")
  # site <- update_site_indicator(site, "pd", "benefit") # requires pol_func, rsb_func
  # site <- update_site_indicator(site, "oe", "function") # requires ap_func , cs_func
  # site <- update_site_indicator(site, "am", "function") # requires ap_func ,
  # site <- update_site_indicator(site, "am", "benefit") # requires wb_func
  # site <- update_site_indicator(site, "fh", "function") # requires ap_func,
  # site <- update_site_indicator(site, "fh", "benefit") # requires wb_func
  # site <- update_site_indicator(site, "sfts", "function")
  # site <- update_site_indicator(site, "sfts", "benefit") # requires fh_func
  # site <- update_site_indicator(site, "ap", "benefit") # requires fh_func , ah_func, wb_func, rsb_func
  # site <- update_site_indicator(site, "cri", "benefit") # requires wb_func, fh_func
  site
}
