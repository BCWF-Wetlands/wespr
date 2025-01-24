
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wespr

<!-- badges: start -->

[![R-CMD-check](https://github.com/BCWF-Wetlands/wespr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BCWF-Wetlands/wespr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The B.C. Wildlife Federation’s Wetlands Workforce project is a
collaboration with conservation organizations and First Nations working
to maintain and monitor wetlands across British Columbia.
<https://bcwf.bc.ca/initiatives/wetlands-workforce/>.

The [Wetland Ecosystem Services
Protocol](https://wetlandsworkforce.ca/portfolio/wetlands-ecosystem-services-protocol/)
(WESP) model is a standardized method for assessing the function and
value of the services provided by wetlands.

wespr is an R package to help automate the validation of WESP data and
to calculate WESP indicators in a reproducible manner.

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
wesp_file <- system.file("input_data/wetFlat_20240130.csv", package = "wespr")
wesp_data <- load_wesp_data(wesp_file)

head(wesp_data)
#> # A tibble: 6 × 102
#>   q_no  response_no site_1 site_2 site_3 site_4 site_5 site_6 site_7 site_8
#>   <chr> <chr>       <chr>   <dbl> <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1 F1    1           0           1 1      1      2      1      4      1     
#> 2 F1    2           3           3 2      0      2      0      0      0     
#> 3 F1    3           0           3 1      1      1      1      3      1     
#> 4 F1    4           4           1 1      1      3      1      0      0     
#> 5 F1    5           0           1 1      1      0      1      1      0     
#> 6 F1    6           1           1 1      1      4      3      0      0     
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
#> Questions F43, F44, F50 do not appear to have been filled out.
#>  Please ensure this is valid.
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
#> Incomplete Questions:  F43, F44, F50 
#>   * Please ensure that it is valid to leave these questions unanswered.
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
#>   *  NoOutlet = 0
#>   *  Inflow = 0
#>   *  Disturb = 0
#>   *  FishFound = 0
#>   *  Moose = 0
#>   *  Beaver = 0
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
#> Incomplete Questions:  F43, F44, F50 
#>   * Please ensure that it is valid to leave these questions unanswered.
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
#>   *  NoOutlet = 0
#>   *  Inflow = 0
#>   *  Disturb = 0
#>   *  FishFound = 0
#>   *  Moose = 0
#>   *  Beaver = 0
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
#>     - fun: 4.61 
#>     - ben: 10 
#>   * SR:  
#>     - fun: 10 
#>     - ben: 4.06 
#>   * PR:  
#>     - fun: 10 
#>     - ben: 3.18 
#>   * CP:  
#>     - fun: 6.86 
#>   * FR:  
#>     - fun: 4.39 
#>     - ben: 8.06 
#>   * SENS:  
#>     - fun: 6.24 
#>   * STR:  
#>     - fun: 7.96 
#>   * NR:  
#>     - fun: 10 
#>     - ben: 5 
#>   * APP:  
#>     - fun: 4.81 
#>     - ben: 4 
#>   * PD:  
#>     - fun: 5.59 
#>     - ben: 5.45 
#>   * KMH:  
#>     - fun: 5.38 
#>     - ben: 5.3 
#>   * WB:  
#>     - fun: 4.95 
#>     - ben: 7.19 
#>   * POL:  
#>     - fun: 5.81 
#>     - ben: 6 
#>   * RSB:  
#>     - fun: 4.96 
#>     - ben: 5.58 
#>   * OE:  
#>     - fun: 0 
#>   * AM:  
#>     - fun: 3.17 
#>     - ben: 6.03 
#>   * FH:  
#>     - fun: 2.92 
#>     - ben: 4.98 
#>   * SFTS:  
#>     - fun: 1.81 
#>     - ben: 2.07 
#>   * CRI:  
#>     - ben: 5.35 
#> 
#> * Retrieve indicator scores with `get_indicator_scores()`
```

We probably want to get the indicator scores out as a usable object. We
can do that with `get_indicator_scores()`, which gives them to us as a
`data.frame`:

``` r
ind_scores <- get_indicator_scores(site)

ind_scores
#> # A tibble: 19 × 4
#>    site   indicator   fun   ben
#>    <chr>  <chr>     <dbl> <dbl>
#>  1 site_1 WS         4.61 10   
#>  2 site_1 SR        10     4.06
#>  3 site_1 PR        10     3.18
#>  4 site_1 CP         6.86 NA   
#>  5 site_1 FR         4.39  8.06
#>  6 site_1 SENS       6.24 NA   
#>  7 site_1 STR        7.96 NA   
#>  8 site_1 NR        10     5   
#>  9 site_1 APP        4.81  4.00
#> 10 site_1 PD         5.59  5.45
#> 11 site_1 KMH        5.38  5.30
#> 12 site_1 WB         4.95  7.19
#> 13 site_1 POL        5.81  6   
#> 14 site_1 RSB        4.96  5.58
#> 15 site_1 OE         0    NA   
#> 16 site_1 AM         3.17  6.03
#> 17 site_1 FH         2.92  4.98
#> 18 site_1 SFTS       1.81  2.07
#> 19 site_1 CRI       NA     5.35
```

Similarly, we can extract the original responses with `get_responses()`:

``` r
get_responses(site)
#> # A tibble: 498 × 4
#>    no    question                           response_no value
#>    <chr> <chr>                              <chr>       <dbl>
#>  1 F1    Vegetation Height & Form Diversity F1_1            0
#>  2 F1    Vegetation Height & Form Diversity F1_2            3
#>  3 F1    Vegetation Height & Form Diversity F1_3            0
#>  4 F1    Vegetation Height & Form Diversity F1_4            4
#>  5 F1    Vegetation Height & Form Diversity F1_5            0
#>  6 F1    Vegetation Height & Form Diversity F1_6            1
#>  7 F10   Dense Moss Extent                  F10_1           1
#>  8 F10   Dense Moss Extent                  F10_2           0
#>  9 F10   Dense Moss Extent                  F10_3           0
#> 10 F10   Dense Moss Extent                  F10_4           0
#> # ℹ 488 more rows
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

## Calculating multiple site scores

In most cases we wish to calculate indicator scores for multiple site.
This can be done with the `calculate_multi_site()` function. Which
requires the data to firstly be loaded using the `load_wesp_data()`
function. We can then iterate through all sites.

``` r

calculate_multi_site(wesp_data)
```

## Calculating Jenks scores

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
#>    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000    0.0000000 
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
#> Score: 4.605741 
#>   Subscores: 
#>     - subsurf: 0.46
#>     - livestore: 0.6
#>     - friction: 0.62
#> 
#> $ws$ben
#> Score: 10
#> 
#> $sr
#> $sr$fun
#> Score: 10 
#>   Subscores: 
#>     - livestore: 0.62
#>     - dryintercept: 0.17
#>     - wetintercept: 0.65
#>     - connectiv: 0.2
#> 
#> $sr$ben
#> Score: 4.06
#> 
#> $pr
#> $pr$fun
#> Score: 10 
#>   Subscores: 
#>     - interceptdry: 0.31
#>     - interceptwet: 0.46
#>     - connec: 0.67
#>     - adsorb: 0.7
#>     - desorb: 0.49
#> 
#> $pr$ben
#> Score: 3.18
#> 
#> $cp
#> $cp$fun
#> Score: 6.86
#> 
#> $fr
#> $fr$fun
#> Score: 4.39
#> $fr$ben
#> Score: 8.06
#> 
#> $sens
#> $sens$fun
#> Score: 6.237939 
#>   Subscores: 
#>     - abiosens: 1
#>     - biosens: 0.62
#>     - colonizer: 0.45
#>     - growrate: 0.42
#> 
#> 
#> $str
#> $str$fun
#> Score: 7.959184 
#>   Subscores: 
#>     - hydrostress: 0.17
#>     - wqstress: 0.13
#>     - connecstress: 0.8
#> 
#> 
#> $nr
#> $nr$fun
#> Score: 10 
#>   Subscores: 
#>     - warmth: 0.51
#>     - intercept: 0.37
#>     - connecc: 0.67
#>     - organic: 0.7
#>     - redox: 0.9
#> 
#> $nr$ben
#> Score: 5
#> 
#> $app
#> $app$fun
#> Score: 4.810678 
#>   Subscores: 
#>     - npinput: 0.12
#>     - npcycling: 0.74
#>     - templight: 0.68
#>     - stressors: 0.77
#> 
#> $app$ben
#> Score: 4
#> 
#> $pd
#> $pd$fun
#> Score: 5.592124 
#>   Subscores: 
#>     - spparea: 0.56
#>     - vrichness: 0.6
#>     - aqfertilpd: 0.48
#>     - vscape: 0.58
#>     - stresspd: 0.54
#> 
#> $pd$ben
#> Score: 5.45
#> 
#> $kmh
#> $kmh$fun
#> Score: 5.384443 
#>   Subscores: 
#>     - beaverhab: 0.45
#>     - muskrathab: 0.44
#>     - moosehab: 0.75
#>     - caribouhab: 0.35
#>     - bearhab: 0.7
#> 
#> $kmh$ben
#> Score: 5.3
#> 
#> $wb
#> $wb$fun
#> Score: 4.952343 
#>   Subscores: 
#>     - lscape: 0.36
#>     - hydro: 0.55
#>     - produc: 0.48
#>     - struc: 0.45
#> 
#> $wb$ben
#> Score: 7.19
#> 
#> $pol
#> $pol$fun
#> Score: 5.809951 
#>   Subscores: 
#>     - pollen: 0.56
#>     - nestsites: 0.63
#>     - stress: 0.54
#> 
#> $pol$ben
#> Score: 6
#> 
#> $rsb
#> $rsb$fun
#> Score: 4.956015 
#>   Subscores: 
#>     - hydrosize: 0.52
#>     - struc: 0.65
#>     - foods: 0.49
#>     - habs: 0.5
#>     - lscape: 0.28
#>     - nopred: 0.33
#> 
#> $rsb$ben
#> Score: 5.58
#> 
#> $oe
#> $oe$fun
#> Score: 0 
#>   Subscores: 
#>     - histaccum: 0.69
#>     - productiv: 0.48
#>     - exportpot: 0.5
#> 
#> 
#> $am
#> $am$fun
#> Score: 3.165836 
#>   Subscores: 
#>     - waterscape: 0.11
#>     - hydro: 0.54
#>     - aqstruc: 0.71
#>     - terrstruc: 0.43
#>     - biostress: 0.41
#> 
#> $am$ben
#> Score: 6.03
#> 
#> $fh
#> $fh$fun
#> Score: 2.920322 
#>   Subscores: 
#>     - hydro: 0.47
#>     - struc: 0.08
#>     - nooxyrisk: 0.5
#>     - nostress: 0.31
#> 
#> $fh$ben
#> Score: 4.98
#> 
#> $sfts
#> $sfts$fun
#> Score: 1.808248 
#>   Subscores: 
#>     - shadedsurf: 0.44
#>     - surfacestorage: 0.14
#>     - groundwater: 0.33
#> 
#> $sfts$ben
#> Score: 2.07
#> 
#> $cri
#> $cri$ben
#> Score: 5.35
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
