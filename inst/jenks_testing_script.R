
#Prepare for and install wespr
#devtools::install_github("BCWF-Wetlands/wespr")
library(wespr)
load_all()

#read wesp data into wespr
data_test <- file.path("C:/Users/genev/OneDrive/Documents/02.Contracts/2023_BCWF_wetlands/04.Data/testing_don/wesp.csv")

wesp_data <- load_wesp_data(data_test)


# 1)  run a single site
# convert to wesp struture
single_site <- as.wesp_site(data, 1)

# calculate indicators
single_site <- calc_indicators(single_site)

# extract the indicator scores
singlescores <- get_indicator_scores(single_site)

# we can also extract indicvidual information from the site
resp <- get_responses(single_site)
derived <- get_derived_values(single_site)

get_q(single_site, "F1_1")
get_q(single_site, "NeverWater")



# 2)  run all sites

library(purrr)

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
  select(site, everything())

cdatb <- cdat |>
  select(-fun) |>
  pivot_wider(names_from = indicator, values_from =ben,  names_glue = "{indicator}_b_raw",) |>
  select(site, everything())


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



#Calculate Jenks breaks and add to data.frame
#First normalize the service and add to data.frame
#Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization
wespNorm <- as.data.frame(lapply(wespRaw[,-1], min_max_norm))



%>%
  mutate(site=as.numeric(rownames(.)), .before=1)
#colnames(wespNorm)<-c('site',paste0(wespServices,'Norm'))











#nsites<-ncol(wesp_data)-2


#loop over all sites and calculate indicators and store in list
# used below for normalizing and jenks breaks
siteL<-lapply(1:nsites, function(x) {
  calc_indicators(as.wesp_site(wesp_data, site=x))
})

#store names of eco services and get number of services
wespServices<-names(siteL[[1]]$indicators)
nServices<-length(wespServices)

##### Calculate Jenks  Breaks #####

#Pull out the raw wesp scores for each service and site and store it in a data frame
FunctionL<-lapply(1:nServices, function(x) {
  siteFL<-lapply(1:nsites, function(y) {
    #Index into wesp object to get raw scores
    siteL[[y]][[4]][[x]][[1]][[1]]
  })
  siteLL <- as.data.frame(do.call(rbind, siteFL))
})
wespFn.1<-do.call(cbind, FunctionL)
colnames(wespFn.1)<-c(paste0(wespServices,'Raw'))
wespRaw<-wespFn.1 %>%
  mutate(site=as.numeric(rownames(.)), .before=1) %>%
  replace(is.na(.),0)


#Calculate Jenks breaks and add to data.frame
#First normalize the service and add to data.frame
#Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
#apply Min-Max normalization
wespNorm <- as.data.frame(lapply(wespRaw[2:20], min_max_norm)) %>%
  mutate(site=as.numeric(rownames(.)), .before=1)
colnames(wespNorm)<-c('site',paste0(wespServices,'Norm'))

#Do Jenks breaks using normalized scores
# use BAMMtools' getJenksBreaks function
#install.packages('BAMMtools')
library('BAMMtools')

wesp_breaksL.1 <- lapply(2:(nServices+1), function(x) {
  jen_breaks<-getJenksBreaks(wespNorm[[x]], 4, subset = NULL)
  .bincode(wespNorm[[x]], jen_breaks,include.lowest=TRUE)
})

#Change numeric to character High, Medium, Low
wesp_breaksL<- lapply(wesp_breaksL.1[1:(nServices+1)], function(x) case_when(
  x ==1 ~ 'L',
  x ==2  ~ 'M',
  x ==3  ~ 'H'
))
#Change list to data frame
wespBreaks<-as.data.frame(do.call(cbind, wesp_breaksL)) %>%
  mutate(site=as.numeric(rownames(.)), .before=1)
colnames(wespBreaks)<-c('site',paste0(wespServices,'Jenks'))

#Make a single data frame that includes the raw, normalized and Jenks values
wespEcoS.1<-list(wespRaw, wespNorm, wespBreaks) %>%
  purrr::reduce(full_join, by='site') %>%
  dplyr::select(site,sort(names(.)))
wespEcoS<-data.frame(Wetland_Co=wetLUT,wespEcoS.1)

#Write out the data frame
WriteXLS(wespEcoS,file.path(dataOutDir,paste('wespEcoS.xlsx',sep='')),
         row.names=FALSE,col.names=TRUE,AllText=TRUE)

