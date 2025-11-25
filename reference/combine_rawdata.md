# Combine all types of raw data

Combine all types of raw data

## Usage

``` r
combine_rawdata(
  field_data,
  office_data,
  EcoP = "GD",
  write_subfiles = TRUE,
  out_dir = "temp",
  overwrite = FALSE
)
```

## Arguments

- field_data:

  A path to the field data

- office_data:

  A path to the office data

- EcoP:

  A character string specifying the region. Default = 'GD'

- write_subfiles:

  A logical value specifying whether to write the subfiles. Default =
  TRUE

- out_dir:

  A character string specifying the output directory.

- overwrite:

  A logical value specifying whether to overwrite the output files.
  Default = FALSE

## Value

A tibble with the combined data

## Examples

``` r
if (FALSE) { # \dontrun{
combine_rawdata(
  field_data = fs::path("inst/input_data/raw", "field_survey123_edited.xls"),
  office_data = fs::path("inst/input_data/raw", "scripted_office.xlsx"),
  EcoP = "GD",
  write_subfiles = FALSE,
  out_dir = "temp",
  overwrite = TRUE
)
} # }
```
