# Format raw inputs This function cleans, formats and combines raw outputs from field and desktop analysis survey123 forms.

Format raw inputs This function cleans, formats and combines raw outputs
from field and desktop analysis survey123 forms.

## Usage

``` r
format_rawinputs(
  field_data,
  desktop_data,
  write_subfiles = TRUE,
  out_dir = "temp",
  overwrite = FALSE
)
```

## Arguments

- field_data:

  A file path to survey123 csv file for field data collected

- desktop_data:

  A file path to survey123 csv file for Desktop data

- write_subfiles:

  A logical value specifying whether to write the subfiles. Default =
  TRUE

- out_dir:

  A character string specifying the output directory.

- overwrite:

  A logical value specifying whether to overwrite the output files.
  Default = FALSE

## Value

dataframe in format for use in wespr

## Examples

``` r
if (FALSE) { # \dontrun{
ww <- format_rawinputs(
 field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr"),
 desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr"),
 write_subfiles = FALSE,
 out_dir = "temp",
 overwrite = TRUE)
} # }
```
