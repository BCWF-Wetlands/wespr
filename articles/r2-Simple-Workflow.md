# r2 Simple Workflow

``` r

library(wespr)
```

To help with the use of WESPR-BC, we have developed a workflow to
streamline the steps to generate wetland ecosystem values.

The follow function steps through from raw files to calculate WESPR-BC
scores, assign jenks breaks and output a report. Users need to define
the following parameters:

- field_data : file path to location of field survey123 output (.csv)
- desktop_data : file path to location of desktop survey123 output
  (.csv)
- Eco : BC Ecoprovince code (i.e “GD”)
- out_dir : file path to location where output files will be saved

``` r

# Define the location of the survey123 Desktop data 
field_data <- system.file(file.path('extdata','WESP_FIELDV1.csv'), package = "wespr")
desktop_data <- system.file(file.path('extdata','WESP_DESKTOPV1.csv'), package = "wespr")

out <- wespr_workflow(field_data = field_data,
                      desktop_data = desktop_data, 
                      EcoP = "GD",
                      out_dir = "temp", 
                      report_dir = "temp",
                      EcoP_calibration = NA)
                      
                
write.csv(out, fs::path("temp", "wespr_outputs.csv"))
```
