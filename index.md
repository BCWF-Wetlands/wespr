# wespr

The B.C. Wildlife Federation’s Wetlands Workforce project is a
collaboration with conservation organizations and First Nations working
to maintain and monitor wetlands across British Columbia.
<https://bcwf.bc.ca/initiatives/wetlands-workforce/>.

The [Wetland Ecosystem Services
Protocol](https://bcwfwatershedteam.ca/wetland-ecosystem-services-protocol/)
(WESP-BC) model is a standardized method for assessing the function and
value of the services provided by wetlands within British Columbia.

Wespr is an R package to assist with the preparation and assessment of
wesp-BC scores. The package provides tools to assist with 1) downloading
spatial layers, 2) prepossessing field and desktop questionnaires, 3)
calculating wesp-BC scores for a range of functional and beneficial
ecosystem indicators, 4) advanced functions to upload and verify
calibration data sets.

## Installation

You can install the development version of wespr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BCWF-Wetlands/wespr")
```

## Worked examples

A series of vignettes with step by step instructions is provided to
assist users. It is assumed that users are generally familiar with the
process of wespr-BC and have at least a basic knowledge of R coding.
Further information on the WESP-BC data collection process is available
[here](https://bcwfwatershedteam.ca/wetland-ecosystem-services-protocol/)

#### 1) Pre-processing spatial data

[Prepare spatial layers
vignette](https://bcwf-wetlands.github.io/wespr/articles/prepare_spatial_data.html)
walks through a helper function to downloads a standard set of spatial
layers via the bcdata packge. This data provides a majority of
information required to answer desktop analysis portion of the wesp-BC
protocol

#### 2) Post data survey123 datasets

Once the user has answered the required field and desktop survey123
surveys (and output these as .csv files), we use the
*format_rawinputs()* function to standardize the input data into a
structure that allows us to calculate the wespr-BC values. This [step by
step
guide](https://bcwf-wetlands.github.io/wespr/articles/prepare-rawdata.html)
details the process.

#### 3) Calculate Wespr scores - single site

Once the raw data has been converted to the wespr input format we can
[calculate the wespr
scores](https://bcwf-wetlands.github.io/wespr/articles/calculate_wespr_single_site.html)
for the given site and compare these against a calibration dataset
within the same Eocprovince.

#### 4) Calculate Wespr scores - multiple site

Alternatively, where we have multiple wetland information, we can
generate [scores from multiple
sites](https://bcwf-wetlands.github.io/wespr/articles/calculate_wespr_multi_site.html).

#### 5) Advanced topics

For those interested in understanding how the wespr package operates
under the hood. This technical
[vignette](https://bcwf-wetlands.github.io/wespr/articles/Advanced-topics.html)
provides advanced topics for keeners interested in understanding the
structure of the R package in more detail. This covers two advanced
topics 1) How to update provincial ecoprovince calibration data and 2)
Understanding the anatomy of a wespr object.

These are advanced topics and not required to generate scores.
