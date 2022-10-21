## Trials with Daymet 
## building off of https://github.com/bbuzzee/DaymetRShiny/blob/master/DaymetRShiny/app.R
library(devtools)
library(shiny)
library(tidyverse)
library(daymetr) #install_github("khufkens/daymetr")
library(shinythemes)
library(plotly)
library(lubridate)
library(devtools) 
#data(zipcode)

################################################
### ###     PRISM Function           ###########
################################################
#library(devtools) #needed to download prism from github
library(reshape2) ##melting dataframes
library(dplyr) #data wrangling
library(raster) ##working with raster data
library(sp) ##manipulationg spatial data

#install_github(repo = "prism", username = "ropensci")
#install.packages("prism")
# library(prism) ##prism data access
# #Be sure to set the download folder using `prism_set_dl_dir()`.
# 
# ### Retrieve Dailys Air Temp Mean
# prism_set_dl_dir("data/prism")
# get_prism_dailys(
#   type = "tmean",
#   minDate = "2020-12-31",
#   maxDate = "2021-12-31",
#   keepZip = TRUE,
#   check = "httr"
# )
# 
# ls_prism_data(name=TRUE)


############ Daymet Function  ##################
### to read directly from R
### from the raw download_dayment_batch - reworked to save from workspace
batch_daymet <- function(
  locations = x,
  #start = as.numeric(format(Sys.time(), "%Y")) - 3,#default
  #end = as.numeric(format(Sys.time(), "%Y")) - 1, #default
  internal = TRUE,
  force = FALSE,
  silent = FALSE,
  path = tempdir(),
  simplify = TRUE #when is best as list versus df? I would assume df but this can be "simply" changed. TRUE is df FALSE is list
){
  
output <- apply(locations, 1, function(location) {#by row
  site <- location[1]
  lat <- as.numeric(location[2])
  lon <- as.numeric(location[3])
  start <- if_else(as.numeric(year(location[4]))> 1979, as.numeric(year(location[4])), 1980)
  end <- if_else(as.numeric(year(location[5]))< 2022, as.numeric(year(location[5])), 2021)#needs to be updated every year
  #try(start <- as.numeric(year(start))) #as.numeric(format(location[4], "%Y"))) #default otherwise
  #try(end <-  as.numeric(year(end)))
  
  try(download_daymet(
    site = site,
    lat = lat,
    lon = lon,
    start = start,
    end = end,
  internal = internal,
  force = force,
  silent = silent,
  path = path,
  simplify = simplify
  ),
  silent = FALSE)
  

})

# if the output is tidy, row bind to one big
# tibble otherwise return a nested list
if (simplify){
  output <- do.call("rbind", output)
}

if(internal){
  return(output)
}
}


clean_daymet <- function(daymet_output){
  #climate_var <- unique(output$measurement) 
  temp_var <- c("tmax..deg.c.", "tmin..deg.c.") #for extraction of only temperature data
  # 
  ##clean daymet data 
  daymet_clean <- daymet_output %>%
      dplyr::rename("site_id" = "site") %>%
      dplyr::filter(measurement %in% temp_var) %>%
      spread(measurement, value) %>%
      dplyr::mutate(
        tavg_air_C = (tmax..deg.c. + tmin..deg.c.)/2, #name the daily air temperature average tavg_air_C
        date = ymd(paste0(as.character(year),"-01-01")) + days(yday -1)
      )
    
  }
 


#################debugging
# 
# df <- read.csv("../../../200_Data/201_SpatialLoc/example_geospatial.csv") %>%
#   #dplyr::select(1:3)%>%
#   rename(site = 1, lat = 2, long = 3)
# 
# test <- batch_daymet(df_loc)
