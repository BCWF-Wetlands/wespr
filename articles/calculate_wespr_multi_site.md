# calculate_wespr_multi_site

``` r
library(wespr)
```

## Calculating multiple site scores

We may wish to calculate indicator scores for multiple sites. If we only
need the raw values for each site we can use the
[`calculate_multi_site()`](https://bcwf-wetlands.github.io/wespr/reference/calculate_multi_site.md)
function. Which requires the data to firstly be loaded using the
[`load_wesp_data()`](https://bcwf-wetlands.github.io/wespr/reference/load_wesp_data.md)
function. For example the work flow would be:

## Calculating multiple site scores

In some cases we may wish to calculate indicator scores for multiple
sites. This can be done with the
[`calculate_multi_site()`](https://bcwf-wetlands.github.io/wespr/reference/calculate_multi_site.md)
function. Which requires the data to firstly be loaded using the
[`load_wesp_data()`](https://bcwf-wetlands.github.io/wespr/reference/load_wesp_data.md)
function. We can then iterate through all sites.

``` r
wesp_file <- system.file("input_data/reference_multisite.csv", package = "wespr")
wesp_data <- load_wesp_data(wesp_file)

# generate a site key 
# generate a key for site names 
wespkey <- wesp_data |>
    dplyr::filter(.data$q_no == "Wetland" ) |>
    tidyr::pivot_longer(cols = -c(.data$response_no),
                  names_to = "site_no",
                  values_to = "wetland_id"
    ) |>
    dplyr::select(-response_no) |>
    dplyr::filter(site_no != "q_no") |>
    dplyr::mutate(site = as.numeric(gsub("site_", "", site_no))) |>
    dplyr::select(-site_no)

allsites <- calculate_multi_site(wesp_data)

# add site specific names
allsites <- dplyr::left_join(wespkey, allsites, by = "site")
```

This will return a date.frame with all the values combined for each site
within the dataset provided.

\`\`\` q_no, response_no,site_1,site_2,site_3,site_4,site_5,site_6
F1,1,1,0,1,1,1,2 F1,2,0,3,3,2,0,2 F1,3,1,0,3,1,1,1 F1,4,1,4,1,1,1,3
F1,5,1,0,1,1,1,0 F1,6,1,1,1,1,1,4 F2,A1,0,0,0,0,0,1 F2,A2,0,0,0,0,0,0
F2,B1,0,1,0,0,0,0 F2,B2,1,0,1,1,1,0
