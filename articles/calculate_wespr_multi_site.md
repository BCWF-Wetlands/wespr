# calculate_wespr_multi_site

``` r
library(wespr)
```

## Calculating multiple site scores

We can calculate indicator scores for multiple sites at once. Using the
example data within the packge we can firstly load the data, generate
ids and then calculate the raw values.

``` r
# read in example data
wesp_file <- system.file("input_data/reference_multisite.csv", package = "wespr")

# Read in data into wesp format
wesp_data <- load_wesp_data(wesp_file)

# generate a site key 
wespkey <- wespr::generate_ids(wesp_data)

# calculate raw scores for all site
allsites <- calculate_multi_site(wesp_data, format = "wide")


# add site specific names
allsites <- dplyr::left_join(wespkey, allsites, by = "site")

allsites_long <- calculate_multi_site(wesp_data, format = "long")
```

This will return a date.frame with all the values combined for each site
within the dataset provided.

We can also assign jenks scores for each site. It is assumed that all
sites are located within the same ecoprovince.

``` r

# read in example data
wesp_file <- system.file("input_data/reference_multisite.csv", package = "wespr")

# Read in data into wesp format
wesp_data <- load_wesp_data(wesp_file)

# generate a site key 
wespkey <- generate_ids(wesp_data)

# calculate scores and choose long format 
allsites_long <- calculate_multi_site(wesp_data, format = "long")

# make a list of all unique sites 
usites <- unique(allsites_long$site)

# loop through all 
site_overall <- purrr::map(usites, function(x){
  
  soi = x

  ind_scores <- allsites_long |> dplyr::filter(site == soi)

  out <- assign_jenks_score(ind_scores, calibration_scores, EcoP = "GD")

  out

}) |> dplyr::bind_rows()


# add site specific names
all_jenks <- dplyr::left_join(wespkey, site_overall, by = "site")
```
