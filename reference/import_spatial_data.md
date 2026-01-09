# Download base spatial information layers to support desktop analysis for Wespr

Download base spatial information layers to support desktop analysis for
Wespr

## Usage

``` r
import_spatial_data(aoi = NA, out_dir = NA)
```

## Arguments

- aoi:

  An `sf` object (e.g. polygon) of designated wetland

- out_dir:

  A character string of filepath which points to output location.

## Value

path to the output directory where files are written (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
#import_spatial_data(aoi, out_dir = fs::path("./temp/spatial_layers"))
} # }

```
