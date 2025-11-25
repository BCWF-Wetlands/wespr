# Convert a data.frame of WESP question responses to a `wesp_site`

Read a `data.frame` that has been read in by
[`load_wesp_data()`](https://bcwf-wetlands.github.io/wespr/reference/load_wesp_data.md)
and convert it into a `wesp_site` object, in preparation for calculating
the indicators. This function also calculates various "derived" values
that are used in indicator calculations.

## Usage

``` r
as.wesp_site(data, site = NULL)
```

## Arguments

- data:

  `data.frame` of questions and responses, ideally created by reading in
  data with
  [`load_wesp_data()`](https://bcwf-wetlands.github.io/wespr/reference/load_wesp_data.md).
  Contains columns `q_no`, `response_no`, and one or more `site_[x]`
  columns.

- site:

  A number, or the name of a column in `data` indicating which site to
  calculate, if more than one site in `data`. Defaults to the first
  `site_[x]` column.

## Value

An object of type `wesp_site`
