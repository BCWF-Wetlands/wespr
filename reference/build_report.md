# Build Site Report

Build Site Report

## Usage

``` r
build_report(
  ind_scores,
  calibration_scores,
  EcoP,
  EcoP_calibration = NA,
  output_dir = NULL,
  x_loc = NA,
  y_loc = NA
)
```

## Arguments

- ind_scores:

  A data.frame of indicator scores. The output of
  get_indicator_scores().

- calibration_scores:

  an internal dataset containing the calibration data for all sites.
  This can be updated by admin.

- EcoP:

  A character string specifying the region in which the wetland was
  sampled. Default = 'GD'

- EcoP_calibration:

  A character string specifying the region in which will be calibrated
  against. Default = N. This should only be used where no calibration
  data is available for the ecoprovince where the wetland was sampled.

- output_dir:

  A character string specifying the directory to save the report.
  Default = NULL.

- x_loc:

  A latitude location in WGS84 to plot the wetland location on the map
  in the report. Default is NA.

- y_loc:

  A longitude location in WGS84 to plot the wetland location on the map
  in the report. Default is NA.

## Value

A data.frame with indicator scores and jenks classification score (Low,
Medium, High (L, M, H)).

## Examples

``` r
if (FALSE) { # \dontrun{
wesp <- fs::path("inst/input_data/wetFlat_20250427_single.csv")
wesp_data <- load_wesp_data(wesp)
site <- as.wesp_site(wesp_data)
site <- calc_indicators(site)
ind_scores <- get_indicator_scores(site)
out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "GD")
out <- build_report(ind_scores, calibration_scores, EcoP = "GD", output_dir = "temp")
} # }
```
