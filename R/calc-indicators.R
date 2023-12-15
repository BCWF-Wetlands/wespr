
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
  # site <- update_site_indicator(site, "pd", "function") # requires ap_function()
  # site <- update_site_indicator(site, "kmh", "function") # requires ap_function() * check ref with Paul as unclear
  # site <- update_site_indicator(site, "kmh", "benefit")
  # site <- update_site_indicator(site, "wb", "function")  # requires ap_function()
  # site <- update_site_indicator(site, "wb", "benefit")
  # site <- update_site_indicator(site, "pol", "function") # requires pd_function()
  # site <- update_site_indicator(site, "pol", "benefit")
  # site <- update_site_indicator(site, "rsb", "function") # requires ap_function(), pd_function()
  # site <- update_site_indicator(site, "rsb", "benefit")
  # site <- update_site_indicator(site, "pd", "benefit") # requires pol_function(), rsb_function()
  # site <- update_site_indicator(site, "oe", "function") # requires ap_function() , cs_function()
  # site <- update_site_indicator(site, "am", "function") # requires ap_function() ,
  # site <- update_site_indicator(site, "am", "benefit") # requires wb_function()
  # site <- update_site_indicator(site, "fh", "function") # requires ap_function(),
  # site <- update_site_indicator(site, "fh", "benefit") # requires wb_function()
  # site <- update_site_indicator(site, "sfts", "function")
  # site <- update_site_indicator(site, "sfts", "benefit") # requires fh_function()
  # site <- update_site_indicator(site, "ap", "benefit") # requires fh_function() , ah_function(), wb_function(), rsb_function()
  # site <- update_site_indicator(site, "cri", "benefit") # requires wb_function(), fh_function()
  site
}
