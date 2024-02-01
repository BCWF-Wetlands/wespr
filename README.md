
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wespr

<!-- badges: start -->

[![R-CMD-check](https://github.com/BCWF-Wetlands/wespr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BCWF-Wetlands/wespr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The Wetland Ecosystem Services Protocol (WESP) model is a standardized method for rapidly assessing the function and
value of the services provided by wetlands. 

Within British Columbia, each wetland function and benefit can be assessed by answering a series of office and field based questions. Values are then calculated using rule based logic models with final values compared to a ecoprovince calibrated dataset to provide a relative score. 
WESP scores were previously calculated using a macro driven excel workbook, managed by Paul Adamus. 

wespr is an R package to help automate the validation of WESP data and
to calculate WESP indicators in a reproducible manner. 

This version is currently in draft format and future changes are likley. 

This project was funded by the British Columbia Wildlife Federation. 

## Installation

You can install the development version of wespr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BCWF-Wetlands/wespr")
```

## Example usage

``` r
library(wespr)
```

Once the package is loaded, we can read in the WESP data from a csv,
which should be in a standardized format as output from the scripts at
<https://github.com/BCWF-Wetlands/WESP_Calculator>.

An example format of the file is shown, with the first 10 questions
(rows) and 6 sites (columns) shown:

    Question,1,2,3,4,5,6
    F1_1,1,0,1,1,1,2
    F1_2,0,3,3,2,0,2
    F1_3,1,0,3,1,1,1
    F1_4,1,4,1,1,1,3
    F1_5,1,0,1,1,1,0
    F1_6,1,1,1,1,1,4
    F2_A1,0,0,0,0,0,1
    F2_A2,0,0,0,0,0,0
    F2_B1,0,1,0,0,0,0
    F2_B2,1,0,1,1,1,0

We have included an example file in the package, which we can use for
demonstration purposes:

``` r
wesp_file <- system.file("input_data/wetFlat.csv", package = "wespr")
wesp_data <- load_wesp_data(wesp_file)

head(wesp_data)
#> # A tibble: 6 × 102
#>   q_no  response_no site_1 site_2 site_3 site_4 site_5 site_6 site_7 site_8
#>   <chr> <chr>       <chr>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1 F1    1           1           0 1      1      1      2      1      4     
#> 2 F1    2           0           3 3      2      0      2      0      0     
#> 3 F1    3           1           0 3      1      1      1      1      3     
#> 4 F1    4           1           4 1      1      1      3      1      0     
#> 5 F1    5           1           0 1      1      1      0      1      1     
#> 6 F1    6           1           1 1      1      1      4      3      0     
#> # ℹ 92 more variables: site_9 <chr>, site_10 <chr>, site_11 <chr>,
#> #   site_12 <chr>, site_13 <chr>, site_14 <chr>, site_15 <chr>, site_16 <chr>,
#> #   site_17 <chr>, site_18 <chr>, site_19 <chr>, site_20 <chr>, site_21 <chr>,
#> #   site_22 <chr>, site_23 <chr>, site_24 <chr>, site_25 <chr>, site_26 <chr>,
#> #   site_27 <chr>, site_28 <chr>, site_29 <chr>, site_30 <chr>, site_31 <chr>,
#> #   site_32 <chr>, site_33 <chr>, site_34 <chr>, site_35 <chr>, site_36 <chr>,
#> #   site_37 <chr>, site_38 <chr>, site_39 <chr>, site_40 <chr>, …
```

Next we convert the data into a special `wesp_site` object. This process
validates the input data, and will give informative errors if a question
is not answered correctly. It also calculates many “derived values” from
the questions, which are common inputs into different indicator
calculations.

If there is more than one site in the input data, you can select which
site you would like to use — if no site is selected it will default to
choosing the first site in the file.

``` r
site <- as.wesp_site(wesp_data, site = 1)
#> Warning: Question F25 does not appear to have been filled out.
#> Warning: Question F41 does not appear to have been filled out.
#> Warning: Question F43 does not appear to have been filled out.
#> Warning: Question F44 does not appear to have been filled out.
#> Warning: Question F59 does not appear to have been filled out.
#> Warning: Question OF28 does not appear to have been filled out.
```

You should not normally need to work with the internals of the
`wesp_site` object as it is quite complex, however we can get an
overview of what is in it by just typing the name of the object:

``` r
site
#> A wesp_site object
#> 
#> Site:  site_1 
#> 
#> Incomplete Questions:  F25, F41, F43, F44, F59, OF28 
#> 
#> Derived values:
#>   *  AllWater = 0
#>   *  NeverWater = 0
#>   *  NoSeasonal = 0
#>   *  NoPersis = 0
#>   *  TempWet = 0
#>   *  AllPermW = 0
#>   *  HiFlucW = 0
#>   *  TooShallow = 0
#>   *  NoPond = 0
#>   *  NoDeepPonded = 0
#>   *  NoOW = 0
#>   *  NoOutletX = 0
#>   *  NoOutlet = 1
#>   *  Inflow = 0
#>   *  Disturb = 1
#>   *  FishFound = 0
#>   *  Moose = 0
#>   *  Beaver = 1
#>   *  Muskrat = 0
#>   *  Bear = 0
#>   *  Caribou = 0
#>   *  NoCA = 0
#>   *  Fishless = 0
#>   *  GDeco = 0
#>   *  CMeco = 1
#>   *  SIMeco = 0
#>   *  BPeco = 0
#>   *  TPeco = 0
#>   *  OutMap = 0
#>   *  S1_sum = 2
#>   *  S1_subscore = 0.17
#>   *  S2_sum = 0
#>   *  S2_subscore = 0
#>   *  S3_sum = 0
#>   *  S3_subscore = 0
#>   *  S4_sum = 5
#>   *  S4_subscore = 0.42
#>   *  S5_sum = 0
#>   *  S5_subscore = 0
#>   *  S6_sum = 0
#>   *  S6_subscore = 0
#> 
#> Indicators:
#> All indicators are NULL. Run `calc_indicators()` to calculate them.
```

We can see that the responses are loaded, with a few flagged as
incomplete, and the derived values have been calculated. However, the
indicators have not yet been calculated.

Now, we can calculate the indicator scores with the `calc_indicators()`
function.

You must assign the output of this function back to the original
`wesp_site` object, as it updates the `indicators` with the calculated
values:

``` r
site <- calc_indicators(site)
```

If we view the site object again, we can see the calculated indicator
scores.

``` r
site
#> A wesp_site object
#> 
#> Site:  site_1 
#> 
#> Incomplete Questions:  F25, F41, F43, F44, F59, OF28 
#> 
#> Derived values:
#>   *  AllWater = 0
#>   *  NeverWater = 0
#>   *  NoSeasonal = 0
#>   *  NoPersis = 0
#>   *  TempWet = 0
#>   *  AllPermW = 0
#>   *  HiFlucW = 0
#>   *  TooShallow = 0
#>   *  NoPond = 0
#>   *  NoDeepPonded = 0
#>   *  NoOW = 0
#>   *  NoOutletX = 0
#>   *  NoOutlet = 1
#>   *  Inflow = 0
#>   *  Disturb = 1
#>   *  FishFound = 0
#>   *  Moose = 0
#>   *  Beaver = 1
#>   *  Muskrat = 0
#>   *  Bear = 0
#>   *  Caribou = 0
#>   *  NoCA = 0
#>   *  Fishless = 0
#>   *  GDeco = 0
#>   *  CMeco = 1
#>   *  SIMeco = 0
#>   *  BPeco = 0
#>   *  TPeco = 0
#>   *  OutMap = 0
#>   *  S1_sum = 2
#>   *  S1_subscore = 0.17
#>   *  S2_sum = 0
#>   *  S2_subscore = 0
#>   *  S3_sum = 0
#>   *  S3_subscore = 0
#>   *  S4_sum = 5
#>   *  S4_subscore = 0.42
#>   *  S5_sum = 0
#>   *  S5_subscore = 0
#>   *  S6_sum = 0
#>   *  S6_subscore = 0
#> 
#> Indicators:
#>   * WS:  
#>     - fun: 10 
#>     - ben: 5.89 
#>   * SR:  
#>     - fun: 4.12 
#>     - ben: 4.14 
#>   * PR:  
#>     - fun: 1 
#>     - ben: 14.24 
#>   * CP:  
#>     - fun: 8.72 
#>   * FR:  
#>     - fun: 2.23 
#>     - ben: 8.06 
#>   * SENS:  
#>     - fun: 7.3 
#>   * STR:  
#>     - fun: 7.96 
#>   * NR:  
#>     - fun: 5.38 
#>     - ben: 14.24 
#>   * APP:  
#>     - fun: 2.61 
#>     - ben:  
#>   * PD:  
#>     - fun: 10.44 
#>     - ben: NA 
#>   * KMH:  
#>     - fun:  
#>     - ben: 7.8 
#>   * WB:  
#>     - fun: 25.66 
#>     - ben: 5.62 
#>   * POL:  
#>     - fun:  
#>     - ben: 3.5 
#>   * RSB:  
#>     - fun:  
#>     - ben: 4.03 
#>   * OE:  
#>     - fun: 0 
#>   * AM:  
#>     - fun:  
#>     - ben: 256.64 
#>   * FH:  
#>     - fun:  
#>     - ben:  
#>   * SFTS:  
#>     - fun:  
#>     - ben:  
#>   * CRI:  
#>     - ben:  
#> 
#> * Retrieve indicator scores with `get_indicator_scores()`
```

We probably want to get the indicator scores out as a usable object. We
can do that with `get_indicator_scores()`, which gives them to us as a
`data.frame`:

``` r
ind_scores <- get_indicator_scores(site)

ind_scores
#> # A tibble: 16 × 4
#>    site   indicator   fun    ben
#>    <chr>  <chr>     <dbl>  <dbl>
#>  1 site_1 WS        10      5.89
#>  2 site_1 SR         4.12   4.14
#>  3 site_1 PR         1     14.2 
#>  4 site_1 CP         8.72  NA   
#>  5 site_1 FR         2.23   8.06
#>  6 site_1 SENS       7.30  NA   
#>  7 site_1 STR        7.96  NA   
#>  8 site_1 NR         5.38  14.2 
#>  9 site_1 APP        2.61  NA   
#> 10 site_1 PD        10.4   NA   
#> 11 site_1 KMH       NA      7.80
#> 12 site_1 WB        25.7    5.62
#> 13 site_1 POL       NA      3.5 
#> 14 site_1 RSB       NA      4.03
#> 15 site_1 OE         0     NA   
#> 16 site_1 AM        NA    257.
```

Similarly, we can extract the original responses with `get_responses()`:

``` r
get_responses(site)
#> # A tibble: 488 × 4
#>    no    question                           response_no value
#>    <chr> <chr>                              <chr>       <dbl>
#>  1 F1    Vegetation Height & Form Diversity F1_1            1
#>  2 F1    Vegetation Height & Form Diversity F1_2            0
#>  3 F1    Vegetation Height & Form Diversity F1_3            1
#>  4 F1    Vegetation Height & Form Diversity F1_4            1
#>  5 F1    Vegetation Height & Form Diversity F1_5            1
#>  6 F1    Vegetation Height & Form Diversity F1_6            1
#>  7 F10   Dense Moss Extent                  F10_1           0
#>  8 F10   Dense Moss Extent                  F10_2           0
#>  9 F10   Dense Moss Extent                  F10_3           0
#> 10 F10   Dense Moss Extent                  F10_4           0
#> # ℹ 478 more rows
```

We can also get out a data.frame of derived values, those values which
are calculated from the responses, and used as inputs into many
indicators.

``` r
get_derived_values(site)
#> # A tibble: 41 × 2
#>    name         value
#>    <chr>        <dbl>
#>  1 AllWater         0
#>  2 NeverWater       0
#>  3 NoSeasonal       0
#>  4 NoPersis         0
#>  5 TempWet          0
#>  6 AllPermW         0
#>  7 HiFlucW          0
#>  8 TooShallow       0
#>  9 NoPond           0
#> 10 NoDeepPonded     0
#> # ℹ 31 more rows
```

## Anatomy of the `wesp_site` object

As mentioned above, this is a large, complex object, and it is almost
always best to interact with the `wesp_site` object via the provided
functions. If there is functionality that is missing please [open an
issue](https://github.com/BCWF-Wetlands/wespr/issues).

It has four elements:

``` r
length(site)
#> [1] 4
names(site)
#> [1] "site_name"      "questions"      "derived_values" "indicators"
```

The first three (`site_name`, `questions`, and `derived_values`) are
populated when the `wesp_site` object is created, but the `indicators`
element is just a placeholder, until `calc_indicators()` has been run:

``` r
site$site_name
#> [1] "site_1"

# The questions element is large and complex, so we will just show the names:
names(site$questions)
#>   [1] "F1"   "F10"  "F11"  "F12"  "F13"  "F14"  "F15"  "F16"  "F17"  "F18" 
#>  [11] "F19"  "F2"   "F20"  "F21"  "F22"  "F23"  "F24"  "F25"  "F26"  "F27" 
#>  [21] "F28"  "F29"  "F3"   "F30"  "F31"  "F32"  "F33"  "F34"  "F35"  "F36" 
#>  [31] "F37"  "F38"  "F39"  "F4"   "F40"  "F41"  "F42"  "F43"  "F44"  "F45" 
#>  [41] "F46a" "F46b" "F47"  "F48"  "F49"  "F5"   "F50"  "F51"  "F52"  "F53" 
#>  [51] "F54"  "F55"  "F56"  "F57"  "F58"  "F59"  "F6"   "F7"   "F8"   "F9"  
#>  [61] "OF1"  "OF10" "OF11" "OF12" "OF13" "OF14" "OF15" "OF16" "OF17" "OF18"
#>  [71] "OF19" "OF2"  "OF20" "OF21" "OF22" "OF23" "OF24" "OF25" "OF26" "OF27"
#>  [81] "OF28" "OF29" "OF3"  "OF30" "OF31" "OF32" "OF33" "OF34" "OF35" "OF36"
#>  [91] "OF37" "OF38" "OF39" "OF4"  "OF40" "OF41" "OF42" "OF43" "OF44" "OF5" 
#> [101] "OF6"  "OF7"  "OF8"  "OF9"  "S1"   "S2"   "S3"   "S4"   "S5"   "S6"

site$derived_values
#>     AllWater   NeverWater   NoSeasonal     NoPersis      TempWet     AllPermW 
#>    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
#>      HiFlucW   TooShallow       NoPond NoDeepPonded         NoOW    NoOutletX 
#>    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
#>     NoOutlet       Inflow      Disturb    FishFound        Moose       Beaver 
#>    1.0000000    0.0000000    1.0000000    0.0000000    0.0000000    1.0000000 
#>      Muskrat         Bear      Caribou         NoCA     Fishless        GDeco 
#>    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
#>        CMeco       SIMeco        BPeco        TPeco       OutMap       S1_sum 
#>    1.0000000    0.0000000    0.0000000    0.0000000    0.0000000    2.0000000 
#>  S1_subscore       S2_sum  S2_subscore       S3_sum  S3_subscore       S4_sum 
#>    0.1666667    0.0000000    0.0000000    0.0000000    0.0000000    5.0000000 
#>  S4_subscore       S5_sum  S5_subscore       S6_sum  S6_subscore 
#>    0.4166667    0.0000000    0.0000000    0.0000000    0.0000000

site$indicators
#> $ws
#> $ws$fun
#> [1] 10
#> 
#> $ws$ben
#> [1] 5.891013
#> 
#> 
#> $sr
#> $sr$fun
#> [1] 4.11715
#> 
#> $sr$ben
#> [1] 4.139236
#> 
#> 
#> $pr
#> $pr$fun
#> [1] 1
#> 
#> $pr$ben
#> [1] 14.24296
#> 
#> 
#> $cp
#> $cp$fun
#> [1] 8.715292
#> 
#> 
#> $fr
#> $fr$fun
#> [1] 2.226852
#> 
#> $fr$ben
#> [1] 8.062372
#> 
#> 
#> $sens
#> $sens$fun
#> [1] 7.300439
#> 
#> 
#> $str
#> $str$fun
#> [1] 7.959184
#> 
#> 
#> $nr
#> $nr$fun
#> [1] 5.384203
#> 
#> $nr$ben
#> [1] 14.24296
#> 
#> 
#> $app
#> $app$fun
#> [1] 2.606893
#> 
#> $app$ben
#> NULL
#> 
#> 
#> $pd
#> $pd$fun
#> [1] 10.4376
#> 
#> $pd$ben
#> [1] NA
#> 
#> 
#> $kmh
#> $kmh$fun
#> NULL
#> 
#> $kmh$ben
#> [1] 7.797619
#> 
#> 
#> $wb
#> $wb$fun
#> [1] 25.66432
#> 
#> $wb$ben
#> [1] 5.625
#> 
#> 
#> $pol
#> $pol$fun
#> NULL
#> 
#> $pol$ben
#> [1] 3.5
#> 
#> 
#> $rsb
#> $rsb$fun
#> NULL
#> 
#> $rsb$ben
#> [1] 4.033333
#> 
#> 
#> $oe
#> $oe$fun
#> [1] 0
#> 
#> 
#> $am
#> $am$fun
#> NULL
#> 
#> $am$ben
#> [1] 256.6432
#> 
#> 
#> $fh
#> $fh$fun
#> NULL
#> 
#> $fh$ben
#> numeric(0)
#> 
#> 
#> $sfts
#> $sfts$fun
#> NULL
#> 
#> $sfts$ben
#> NULL
#> 
#> 
#> $cri
#> $cri$ben
#> NULL
```

## Development

### Updating internal data

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
