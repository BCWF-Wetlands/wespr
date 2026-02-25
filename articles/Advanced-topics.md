# Advanced-topics

This vignette walks through some helper functions for those interested
in more advanced topics relating to the wespr package. **This is not
required to calculate scores** and is intended for advanced R users.

## 1) Updating internal data

### Weighs and ranking tables

The wespr package contains two internal datasets which facilitate
question validation and indicator calculations: `question_metadata` and
`indicator_weightings`.

These are updated by:

1.  Update the Google Sheet at:
    <https://docs.google.com/spreadsheets/d/1l2h7Z65H5z0cKv_gvorxkT6k9LC7ZKLS/edit#gid=924841838>
    (you need to be authorized to edit this document)

2.  Run the script in `data-raw/internal-data.R`

3.  Reload/reinstall the package.

4.  Run `devtools::check()` to make sure the changes you made to the
    Google Sheet haven’t broken any existing behaviour.

5.  Commit the changes to `R/sysdata.rda` (this file contains both
    datasets)

### Calibration data

The wespr package contains the summarized `calibration_scores`. These
will require updating as more calibration data becomes available.

These are updated by:

1.  Run the script in `data-raw/upload-calibration-data.R`

2.  Reload/reinstall the package.

3.  Run `devtools::check()` to make sure the changes you made to the
    Google Sheet haven’t broken any existing behaviour.

4.  The calibration data is accessible to users via the
    `calibration_scores` function.

## 2) Anatomy of the `wesp_site` object

As mentioned above, this is a large, complex object, and it is almost
always best to interact with the `wesp_site` object via the provided
functions. If there is functionality that is missing please [open an
issue](https://github.com/BCWF-Wetlands/wespr/issues).

It has four elements:

``` r
length(site)
names(site)
```

The first three (`site_name`, `questions`, and `derived_values`) are
populated when the `wesp_site` object is created, but the `indicators`
element is just a placeholder, until
[`calc_indicators()`](https://bcwf-wetlands.github.io/wespr/reference/calc_indicators.md)
has been run:

``` r
site$site_name

# The questions element is large and complex, so we will just show the names:
names(site$questions)

site$derived_values

site$indicators
```
