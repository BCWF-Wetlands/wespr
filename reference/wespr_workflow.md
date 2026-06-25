# Run the entire wespr workflow

Run the entire wespr workflow

## Usage

``` r
wespr_workflow(
  desktop_data,
  field_data,
  out_dir,
  EcoP,
  EcoP_calibration,
  report_dir
)
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

- EcoP_calibration:

  A character string specifying the region in which will be calibrated
  against. Default = N. This should only be used where no calibration
  data is available for the ecoprovince where the wetland was sampled.

- report_dir:

  A character string specifying the output directory for the report
  document

## Value

A tibble with the combined data

## Examples
