# Calculate indicators for multiple sites

Calculate indicators for multiple sites

## Usage

``` r
calculate_multi_site(wespdata, sites = NULL)
```

## Arguments

- wespdata:

  A data frame containing the formatted wesp data. This is the output of
  load_wesp_data()

- sites:

  A numeric with the number of sites if specific sites are to be
  calculated. The default is NULL, which will include all sites in the
  calculation

## Value

a dataframe with raw values for each site and each ecosystem service or
benefit.

## Examples

``` r
if (FALSE) { # \dontrun{
wespdata <- load_wesp_data(system.file("input_data/wetFlat_20240130.csv", package = "wespr"))
calculate_multi_site(wespdata)
} # }
```
