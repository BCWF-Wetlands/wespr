
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
  site <- update_site_indicator(site, "pr", "fun")
  site <- update_site_indicator(site, "pr", "ben")
  site <- update_site_indicator(site, "cp", "fun")
  site <- update_site_indicator(site, "fr", "fun") # add allsat1 val once confirmed with Paul
  site <- update_site_indicator(site, "fr", "ben")
  site <- update_site_indicator(site, "sens", "fun")
  site <- update_site_indicator(site, "str", "fun")
  site <- update_site_indicator(site, "nr", "fun")
  site <- update_site_indicator(site, "nr", "ben")
  site <- update_site_indicator(site, "app", "fun")
  site <- update_site_indicator(site, "pd", "fun")
  site <- update_site_indicator(site, "kmh", "fun") # add allsat1 vals
  site <- update_site_indicator(site, "kmh", "ben")
  site <- update_site_indicator(site, "wb", "fun")
  site <- update_site_indicator(site, "wb", "ben")
  site <- update_site_indicator(site, "pol", "fun")
  site <- update_site_indicator(site, "pol", "ben")
  site <- update_site_indicator(site, "rsb", "fun")
  site <- update_site_indicator(site, "rsb", "ben")
  site <- update_site_indicator(site, "pd", "ben")
  site <- update_site_indicator(site, "oe", "fun")
  site <- update_site_indicator(site, "am", "fun")
  site <- update_site_indicator(site, "am", "ben")
  site <- update_site_indicator(site, "fh", "fun")
  site <- update_site_indicator(site, "fh", "ben")
  site <- update_site_indicator(site, "app", "ben")
  site <- update_site_indicator(site, "sfts", "fun")  # waiting on check conifer values
  site <- update_site_indicator(site, "sfts", "ben")
  site <- update_site_indicator(site, "cri", "ben")
  site
}
