# beginner_workflow

``` r
library(wespr)
```

To help with the use of WESPR, we have developed a workflow to
streamline the steps to generate wetland ecosystem values.

The follow function steps through from raw files to calculate wespr
scores and assign jenks breaks. Users need to define the following
parameters:

- field_data : file path to location of field survey123 output (.csv)
- desktop_data : file path to location of desktop survey123 output
  (.csv)
- Eco : ecoprovince code (i.e “GD”)
- out_dir : file path to location where output files will be saved

``` r
# Define the location of the survey123 Desktop data 
field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr")
desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr")

out <- wespr_workflow(field_data = field_data,
                      desktop_data = desktop_data, 
                      EcoP = "GD",
                      out_dir = "temp")

write.csv(out, fs::path(out_dir, "wespr_outputs.csv"))
```
