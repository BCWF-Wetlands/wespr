
#' Calculate Indicators for WESP assessment site
#'
#' Calculate all Function and Benefit indicators
#' for a site.
#'
#' @param site An object of type `wesp_site`, as created
#'   with [as.wesp_site()].
#'
#' @return `wesp_site` object with indicators calculated.
#' @export
calc_indicators <- function(site) {
  site <- update_site_indicator(site, "ws", "fun")
  site <- update_site_indicator(site, "ws", "ben")
  site <- update_site_indicator(site, "sr", "fun")
  site <- update_site_indicator(site, "sr", "ben")
  # site <- update_site_indicator(site, "pr", "fun")
  # site <- update_site_indicator(site, "pr", "ben")
  site <- update_site_indicator(site, "cs", "fun")
  # site <- update_site_indicator(site, "fr", "fun")
  # site <- update_site_indicator(site, "fr", "ben")
  # site <- update_site_indicator(site, "sens", "fun")
  # site <- update_site_indicator(site, "str", "fun")
  # site <- update_site_indicator(site, "nr", "fun")
  # site <- update_site_indicator(site, "nr", "ben")
  # site <- update_site_indicator(site, "ap", "fun")
  # site <- update_site_indicator(site, "pd", "fun") # requires ap_fun()
  # site <- update_site_indicator(site, "kmh", "fun") # requires ap_fun() * check ref with Paul as unclear
  # site <- update_site_indicator(site, "kmh", "ben")
  # site <- update_site_indicator(site, "wb", "fun")  # requires ap_fun()
  # site <- update_site_indicator(site, "wb", "ben")
  # site <- update_site_indicator(site, "pol", "fun") # requires pd_fun()
  # site <- update_site_indicator(site, "pol", "ben")
  # site <- update_site_indicator(site, "rsb", "fun") # requires ap_fun(), pd_fun()
  # site <- update_site_indicator(site, "rsb", "ben")
  # site <- update_site_indicator(site, "pd", "ben") # requires pol_fun(), rsb_fun()
  # site <- update_site_indicator(site, "oe", "fun") # requires ap_fun() , cs_fun()
  # site <- update_site_indicator(site, "am", "fun") # requires ap_fun() ,
  # site <- update_site_indicator(site, "am", "ben") # requires wb_fun()
  # site <- update_site_indicator(site, "fh", "fun") # requires ap_fun(),
  # site <- update_site_indicator(site, "fh", "ben") # requires wb_fun()
  # site <- update_site_indicator(site, "sfts", "fun")
  # site <- update_site_indicator(site, "sfts", "ben") # requires fh_fun()
  # site <- update_site_indicator(site, "ap", "ben") # requires fh_fun() , ah_fun(), wb_fun(), rsb_fun()
  # site <- update_site_indicator(site, "cri", "ben") # requires wb_fun(), fh_fun()
  site
}
