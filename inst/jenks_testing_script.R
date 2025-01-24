
#Prepare for and install wespr
#devtools::install_github("BCWF-Wetlands/wespr")
library(wespr)
load_all()

#read wesp data into wespr
data_test <- file.path("C:/Users/genev/OneDrive/Documents/02.Contracts/2023_BCWF_wetlands/04.Data/testing_don/wesp.csv")

wesp_data <- load_wesp_data(data_test)

# 2)  run all sites

calculate_jenks_score(wesp_data, out_dir = "temp",  out_name = "wesp_scores_test.csv")




allsites <- unique(names(wesp_data)[grepl("site", names(wesp_data))])

sitelist <- seq(1,length(allsites), 1)
#sitelist <- seq(1, 4, 1)

cdat <- purrr::map(sitelist, function(i){
  cli::cli_alert_info(" processsing {allsites[i]}")
  #print(allsites[i])
  site <- as.wesp_site(data, i)
  site <- calc_indicators(site)
  #aa = get_derived_values(site)
  aa = get_indicator_scores(site)
  #print(aa)
}) |>  bind_rows()



# reformat into widetable

library(tidyr)

cdatf <- cdat |>
  select(-ben) |>
  pivot_wider(names_from = indicator, values_from = fun,  names_glue = "{indicator}_f_raw",) |>
  select(site, everything())%>%
  mutate(site = as.numeric(sub("site_", "", site)))

cdatb <- cdat |>
  select(-fun) |>
  pivot_wider(names_from = indicator, values_from =ben,  names_glue = "{indicator}_b_raw",) |>
  select(site, everything())%>%
  mutate(site = as.numeric(sub("site_", "", site)))


wespRaw <- left_join(cdatf, cdatb)



# drop all NA cols
wespRaw <- wespRaw[colSums(!is.na(wespRaw)) > 0]


# check each column for NA values and set up a flag

na_count <- sapply(wespRaw, function(y) sum(length(which(is.na(y)))))
na_count <- na_count[na_count > 0]


nas_check <- wespRaw |>
  select(site, all_of(names(na_count)))

nas_check <- nas_check[!complete.cases(nas_check),]

#output a warning and file?
cli::cli_alert_warning ("Check the following sites and functions")

nas_check



# flag values that are all the same?

unique_indicator <- as.data.frame(sapply(wespRaw, function(y) length(unique(y))))
names(unique_indicator) <- "vals"
unique_indicator <- unique_indicator |>
  filter(vals == 1) %>% row.names()
unique_indicator <- sub("_raw", "", unique_indicator)

cli::cli_alert_warning("Check the following sites and functions as all values are equal: {unique_indicator}")


#Calculate Jenks breaks and add to data.frame
#First normalize the service and add to data.frame
#Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE))
}


#apply Min-Max normalization
wespNorm <- as.data.frame(lapply(wespRaw[,-1], min_max_norm))%>%
  mutate(site=as.numeric(rownames(.)), .before=1)
wespNorm <- rename_with(wespNorm, ~ gsub("_raw", "_norm", .x, fixed = TRUE))


library('BAMMtools')

# wesp_breaksL.1 <- lapply(2:(nServices+1), function(x) {
#   jen_breaks<-getJenksBreaks(wespNorm[[x]], 4, subset = NULL)
#   .bincode(wespNorm[[x]], jen_breaks,include.lowest=TRUE)
# })
#
# x <-

wesp_breaks_raw <- purrr::map(names(wespNorm)[-1], function(x) {
  # names(wespNorm)[-1][1]
  #x <- names(wespNorm)[-1][15]
  if(all(is.na(wespNorm[[x]]) == TRUE)){
    cli::cli_alert_warning("skipping {x} as all values are NA")
    return(NA)
  } else {
  jen_breaks <- getJenksBreaks(wespNorm[[x]], 4, subset = NULL)
  .bincode(wespNorm[[x]], sort(jen_breaks),include.lowest=TRUE)
  }

})

names(wesp_breaks_raw)<- names(wespNorm)[-1]
# output jenks breaks ?



#Change numeric to character High, Medium, Low
wesp_breaks_cat <- lapply(wesp_breaks_raw[1:length(wesp_breaks_raw)], function(x) case_when(
  x ==1 ~ 'L',
  x ==2  ~ 'M',
  x ==3  ~ 'H'
))

#Change list to data frame
wespBreaks <-as.data.frame(do.call(cbind, wesp_breaks_cat))
wespBreaks <- wespBreaks |>
  mutate(site=as.numeric(rownames(wespBreaks)), .before=1)
wespBreaks <- rename_with(wespBreaks, ~ gsub("_norm", "_jenks", .x, fixed = TRUE))


#Make a single data frame that includes the raw, normalized and Jenks values
wespEcoS <-list(wespRaw, wespNorm, wespBreaks) %>%
  purrr::reduce(full_join, by='site') %>%
  dplyr::select(site,sort(names(.)))

#wespEcoS <-data.frame(Wetland_Co=wetLUT,wespEcoS.1)

#Write out the data frame
write.csv(wespEcoS, fs::path(out_dir, out_name),row.names=FALSE)


#wespEcoS.csv

