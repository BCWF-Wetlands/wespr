# Title

Title

## Usage

``` r
wespr_workflow(desktop_data, field_data, out_dir, EcoP)
```

## Arguments

- desktop_data:

  A file path to survey123 csv file for office data collected

- field_data:

  A file path to survey123 csv file for field data collected

- out_dir:

  A character string specifying the output directory.

- EcoP:

  A character string specifying the region. Default = 'GD'

## Value

A tibble with the combined data

## Examples

``` r
if (FALSE) { # \dontrun{
wespr_workflow(
  field_data = fs::path("inst/input_data/raw", "field_survey123_edited.xls"),
  office_data = fs::path("inst/input_data/raw", "scripted_office.xlsx"),
  EcoP = "GD",
  write_subfiles = FALSE,
  out_dir = "temp",
  overwrite = TRUE)
} # }
```
