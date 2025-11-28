
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
wesp_file <- system.file("input_data/reference_multisite.csv", package = "wespr")
wesp_data <- load_wesp_data(wesp_file)

head(wesp_data)
#> # A tibble: 6 × 12
#>   q_no  response_no site_1 site_2 site_3 site_4 site_5 site_6 site_7 site_8
#>   <chr> <chr>       <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr>  <chr> 
#> 1 F1    1           1      0      1      2      1      3      1      1     
#> 2 F1    2           1      1      0      1      0      0      2      0     
#> 3 F1    3           1      0      1      1      1      3      1      1     
#> 4 F1    4           2      1      1      2      1      0      3      0     
#> 5 F1    5           1      0      0      1      2      0      0      1     
#> 6 F1    6           1      0      2      1      1      3      2      1     
#> # ℹ 2 more variables: site_9 <chr>, site_10 <chr>
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
#> Questions F31, F32, F33, F34, F35, F36, F37, F50, F52, F55 do not appear to have been filled out.
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
#> Incomplete Questions:  F31, F32, F33, F34, F35, F36, F37, F50, F52, F55 
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
#>   *  NoDeepPonded = 1
#>   *  NoOW = 0
#>   *  NoOutletX = 0
#>   *  NoOutlet = 0
#>   *  Inflow = 1
#>   *  Disturb = 0
#>   *  FishFound = 1
#>   *  Moose = 1
#>   *  Beaver = 1
#>   *  Muskrat = 1
#>   *  Bear = 1
#>   *  Caribou = 1
#>   *  NoCA = 0
#>   *  Fishless = 0
#>   *  GDeco = 0
#>   *  CMeco = 0
#>   *  SIMeco = 1
#>   *  BPeco = 0
#>   *  TPeco = 0
#>   *  OutMap = 1
#>   *  S1_sum = 1
#>   *  S1_subscore = 0.08
#>   *  S2_sum = 0
#>   *  S2_subscore = 0
#>   *  S3_sum = 1
#>   *  S3_subscore = 0.11
#>   *  S4_sum = 0
#>   *  S4_subscore = 0
#>   *  S5_sum = 0
#>   *  S5_subscore = 0
#>   *  S6_sum = 3
#>   *  S6_subscore = 0.5
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
#> Incomplete Questions:  F31, F32, F33, F34, F35, F36, F37, F50, F52, F55 
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
#>   *  NoDeepPonded = 1
#>   *  NoOW = 0
#>   *  NoOutletX = 0
#>   *  NoOutlet = 0
#>   *  Inflow = 1
#>   *  Disturb = 0
#>   *  FishFound = 1
#>   *  Moose = 1
#>   *  Beaver = 1
#>   *  Muskrat = 1
#>   *  Bear = 1
#>   *  Caribou = 1
#>   *  NoCA = 0
#>   *  Fishless = 0
#>   *  GDeco = 0
#>   *  CMeco = 0
#>   *  SIMeco = 1
#>   *  BPeco = 0
#>   *  TPeco = 0
#>   *  OutMap = 1
#>   *  S1_sum = 1
#>   *  S1_subscore = 0.08
#>   *  S2_sum = 0
#>   *  S2_subscore = 0
#>   *  S3_sum = 1
#>   *  S3_subscore = 0.11
#>   *  S4_sum = 0
#>   *  S4_subscore = 0
#>   *  S5_sum = 0
#>   *  S5_subscore = 0
#>   *  S6_sum = 3
#>   *  S6_subscore = 0.5
#> 
#> Indicators:
#>   * WS:  
#>     - fun: 2.37 
#>     - ben: 3.39 
#>   * SR:  
#>     - fun: 0 
#>     - ben: 3.05 
#>   * PR:  
#>     - fun: 4.73 
#>     - ben: 3.84 
#>   * CP:  
#>     - fun: 6.56 
#>   * FR:  
#>     - fun: 2.75 
#>     - ben: 0.35 
#>   * SENS:  
#>     - fun: 6.95 
#>   * STR:  
#>     - fun: 5.54 
#>   * NR:  
#>     - fun: 5.23 
#>     - ben: 3.84 
#>   * APP:  
#>     - fun: 2.77 
#>     - ben: 4.51 
#>   * PD:  
#>     - fun: 6.31 
#>     - ben: 2.63 
#>   * KMH:  
#>     - fun: 8.8 
#>     - ben: 4.58 
#>   * WB:  
#>     - fun: 3.96 
#>     - ben: 6.67 
#>   * POL:  
#>     - fun: 5.78 
#>     - ben: 3.33 
#>   * RSB:  
#>     - fun: 6.7 
#>     - ben: 4.72 
#>   * OE:  
#>     - fun: 4.37 
#>   * AM:  
#>     - fun: 4.44 
#>     - ben: 3.96 
#>   * FH:  
#>     - fun: 2.94 
#>     - ben: 4.65 
#>   * SFTS:  
#>     - fun: 4.5 
#>     - ben: 2.93 
#>   * CRI:  
#>     - ben: 5.54 
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
#>    site   indicator   fun    ben
#>    <chr>  <chr>     <dbl>  <dbl>
#>  1 site_1 WS         2.37  3.39 
#>  2 site_1 SR         0     3.05 
#>  3 site_1 PR         4.73  3.84 
#>  4 site_1 CP         6.56 NA    
#>  5 site_1 FR         2.75  0.352
#>  6 site_1 SENS       6.95 NA    
#>  7 site_1 STR        5.54 NA    
#>  8 site_1 NR         5.23  3.84 
#>  9 site_1 APP        2.77  4.51 
#> 10 site_1 PD         6.31  2.63 
#> 11 site_1 KMH        8.80  4.58 
#> 12 site_1 WB         3.96  6.67 
#> 13 site_1 POL        5.78  3.33 
#> 14 site_1 RSB        6.70  4.72 
#> 15 site_1 OE         4.37 NA    
#> 16 site_1 AM         4.44  3.96 
#> 17 site_1 FH         2.94  4.65 
#> 18 site_1 SFTS       4.5   2.93 
#> 19 site_1 CRI       NA     5.54
```

Similarly, we can extract the original responses with `get_responses()`:

``` r
get_responses(site)
#> # A tibble: 501 × 4
#>    no    question                           response_no value
#>    <chr> <chr>                              <chr>       <dbl>
#>  1 F1    Vegetation Height & Form Diversity F1_1            1
#>  2 F1    Vegetation Height & Form Diversity F1_2            1
#>  3 F1    Vegetation Height & Form Diversity F1_3            1
#>  4 F1    Vegetation Height & Form Diversity F1_4            2
#>  5 F1    Vegetation Height & Form Diversity F1_5            1
#>  6 F1    Vegetation Height & Form Diversity F1_6            1
#>  7 F10   Dense Moss Extent                  F10_1           0
#>  8 F10   Dense Moss Extent                  F10_2           0
#>  9 F10   Dense Moss Extent                  F10_3           0
#> 10 F10   Dense Moss Extent                  F10_4           1
#> # ℹ 491 more rows
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
#> 10 NoDeepPonded     1
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

Once all the indicator scores have been calculated, we can calculate the
Jenks breaks and assign high, medium and low categories. This process
firstly normalizes the data, then calculates the Jenks breaks and
assigns the catergories. The process will also flag unusual results,
such as all values being the same across all sites.

``` r
calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_breaks.csv")
```

    site, AM_b_jenks, AM_b_norm, AM_b_raw, AM_f_jenks, AM_f_norm
    1,  L,  0.00,   3.96,    M, 0.59
    2,  M,  0.52,   7.14,    L, 0.00
    3,  M,  0.68,   8.09,    L, 0.02    
    4,  M,  0.52,   7.14,    M, 0.33
    5,  H,  0.76,   8.57,    L, 0.14
    6,  M,  0.42,   6.50,    M, 0.48
    7,  H,  1.00,   10.00, H,   1.00
    8,  H,  0.76,   8.57,    L, 0.11
    9,  H,  1.00,   10.00, M,   0.57    
    10, M,  0.60,   7.59,    L, 0.25

This is currently in development and requires finer regional assessment
to be incorporated. More detail is provided in this
[vignette](https://bcwf-wetlands.github.io/wespr/articles/calculate_wesp_scores.html)

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
#>   0.00000000   0.00000000   0.00000000   0.00000000   0.00000000   0.00000000 
#>      HiFlucW   TooShallow       NoPond NoDeepPonded         NoOW    NoOutletX 
#>   0.00000000   0.00000000   0.00000000   1.00000000   0.00000000   0.00000000 
#>     NoOutlet       Inflow      Disturb    FishFound        Moose       Beaver 
#>   0.00000000   1.00000000   0.00000000   1.00000000   1.00000000   1.00000000 
#>      Muskrat         Bear      Caribou         NoCA     Fishless        GDeco 
#>   1.00000000   1.00000000   1.00000000   0.00000000   0.00000000   0.00000000 
#>        CMeco       SIMeco        BPeco        TPeco       OutMap       S1_sum 
#>   0.00000000   1.00000000   0.00000000   0.00000000   1.00000000   1.00000000 
#>  S1_subscore       S2_sum  S2_subscore       S3_sum  S3_subscore       S4_sum 
#>   0.08333333   0.00000000   0.00000000   1.00000000   0.11111111   0.00000000 
#>  S4_subscore       S5_sum  S5_subscore       S6_sum  S6_subscore 
#>   0.00000000   0.00000000   0.00000000   3.00000000   0.50000000

site$indicators
#> $ws
#> $ws$fun
#> Score: 2.367995 
#>   Subscores: 
#>     - subsurf: 0.6
#>     - livestore: 0.4
#>     - friction: 0.56
#> 
#> $ws$ben
#> Score: 3.39
#> 
#> $sr
#> $sr$fun
#> Score: 0 
#>   Subscores: 
#>     - livestore: 0.38
#>     - dryintercept: 0.51
#>     - wetintercept: 0.48
#>     - connectiv: 0
#> 
#> $sr$ben
#> Score: 3.05
#> 
#> $pr
#> $pr$fun
#> Score: 4.733932 
#>   Subscores: 
#>     - interceptdry: 0.52
#>     - interceptwet: 0.39
#>     - connec: 0.42
#>     - adsorb: 0.3
#>     - desorb: 0.54
#> 
#> $pr$ben
#> Score: 3.84
#> 
#> $cp
#> $cp$fun
#> Score: 6.56
#> 
#> $fr
#> $fr$fun
#> Score: 2.75
#> $fr$ben
#> Score: 0.35
#> 
#> $sens
#> $sens$fun
#> Score: 6.947053 
#>   Subscores: 
#>     - abiosens: 1
#>     - biosens: 0.75
#>     - colonizer: 0.37
#>     - growrate: 0.66
#> 
#> 
#> $str
#> $str$fun
#> Score: 5.544218 
#>   Subscores: 
#>     - hydrostress: 0.03
#>     - wqstress: 0.02
#>     - connecstress: 0.55
#> 
#> 
#> $nr
#> $nr$fun
#> Score: 5.230225 
#>   Subscores: 
#>     - warmth: 0.55
#>     - intercept: 0.42
#>     - connecc: 0.42
#>     - organic: 1
#>     - redox: 0.46
#> 
#> $nr$ben
#> Score: 3.84
#> 
#> $app
#> $app$fun
#> Score: 2.769416 
#>   Subscores: 
#>     - npinput: 0.33
#>     - npcycling: 0.5
#>     - templight: 0.59
#>     - stressors: 0.8
#> 
#> $app$ben
#> Score: 4.51
#> 
#> $pd
#> $pd$fun
#> Score: 6.309854 
#>   Subscores: 
#>     - spparea: 0.7
#>     - vrichness: 0.65
#>     - aqfertilpd: 0.4
#>     - vscape: 0.69
#>     - stresspd: 0.95
#> 
#> $pd$ben
#> Score: 2.63
#> 
#> $kmh
#> $kmh$fun
#> Score: 8.803362 
#>   Subscores: 
#>     - beaverhab: 1
#>     - muskrathab: 0.4
#>     - moosehab: 1
#>     - caribouhab: 1
#>     - bearhab: 1
#> 
#> $kmh$ben
#> Score: 4.58
#> 
#> $wb
#> $wb$fun
#> Score: 3.961478 
#>   Subscores: 
#>     - lscape: 0.61
#>     - hydro: 0.55
#>     - produc: 0.28
#>     - struc: 0.36
#> 
#> $wb$ben
#> Score: 6.67
#> 
#> $pol
#> $pol$fun
#> Score: 5.775628 
#>   Subscores: 
#>     - pollen: 0.63
#>     - nestsites: 0.43
#>     - stress: 0.72
#> 
#> $pol$ben
#> Score: 3.33
#> 
#> $rsb
#> $rsb$fun
#> Score: 6.6958 
#>   Subscores: 
#>     - hydrosize: 0.68
#>     - struc: 0.6
#>     - foods: 0.46
#>     - habs: 0.52
#>     - lscape: 0.71
#>     - nopred: 1
#> 
#> $rsb$ben
#> Score: 4.72
#> 
#> $oe
#> $oe$fun
#> Score: 4.366295 
#>   Subscores: 
#>     - histaccum: 0.66
#>     - productiv: 0.28
#>     - exportpot: 0.67
#> 
#> 
#> $am
#> $am$fun
#> Score: 4.440898 
#>   Subscores: 
#>     - waterscape: 0.31
#>     - hydro: 0.55
#>     - aqstruc: 0.5
#>     - terrstruc: 0.61
#>     - biostress: 0.65
#> 
#> $am$ben
#> Score: 3.96
#> 
#> $fh
#> $fh$fun
#> Score: 2.936314 
#>   Subscores: 
#>     - hydro: 0.33
#>     - struc: 0.5
#>     - nooxyrisk: 0
#>     - nostress: 0.27
#> 
#> $fh$ben
#> Score: 4.65
#> 
#> $sfts
#> $sfts$fun
#> Score: 4.5 
#>   Subscores: 
#>     - shadedsurf: 0.46
#>     - surfacestorage: 0.52
#>     - groundwater: 0.37
#> 
#> $sfts$ben
#> Score: 2.93
#> 
#> $cri
#> $cri$ben
#> Score: 5.54
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
