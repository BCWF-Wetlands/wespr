# Calculate Jenks Breaks

Calculate Jenks Breaks

## Usage

``` r
calculate_jenks_score(
  wespdata,
  sites = NULL,
  out_dir,
  out_name = "wesp_scores.csv"
)
```

## Arguments

- wespdata:

  A data frame containing the formatted wesp data. This is the output of
  load_wesp_data()

- sites:

  A numeric with the number of sites if specific sites are to be
  calculated. The default is NULL, which will include all sites in the
  calculation

- out_dir:

  a character string or path to location where output file is to be
  saved

- out_name:

  a character string with the name of the output file. Default values is
  "wesp_scores.csv"

## Value

a dateframe with compiled raw, normalised and jenks break values for
each site and each ecosystem function or benefit.

## Examples

``` r
if (FALSE) { # \dontrun{
wespdata <- load_wesp_data(system.file("input_data/wetFlat_20240130.csv", package = "wespr"))
calculate_jenks_score(wespdata, out_dir = "temp",  out_name = "wesp_scores.csv")
} # }
```
