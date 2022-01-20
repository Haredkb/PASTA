## Trials with Daymet 
## building off of https://github.com/bbuzzee/DaymetRShiny/blob/master/DaymetRShiny/app.R
library(devtools)
library(shiny)
library(tidyverse)
library(daymetr) #install_github("khufkens/daymetr")
library(shinythemes)
library(plotly)
#data(zipcode)

############ Function  ##################
### to read directly from R
### from the raw download_dayment_batch - reworked to save from workspace
batch_daymet <- function(
  locations = x,
  start = as.numeric(format(Sys.time(), "%Y")) - 3,#default
  end = as.numeric(format(Sys.time(), "%Y")) - 1, #default
  internal = TRUE,
  force = FALSE,
  silent = FALSE,
  path = tempdir(),
  simplify = TRUE #when is best as list versus df? I would assume df but this can be "simply" changed. TRUE is df FALSE is list
){
  
output <- apply(locations, 1, function(location) {
  site <- as.character(location[1])#removed [,1]
  lat <- as.numeric(location[2])
  lon <- as.numeric(location[3])
  try(start <- as.numeric(year(location[5]))) #as.numeric(format(location[4], "%Y"))) #default otherwise
  try(end <-  as.numeric(year(location[6])))
  
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


#################debugging
# 
# df <- read.csv("../../../200_Data/201_SpatialLoc/example_geospatial.csv") %>%
#   #dplyr::select(1:3)%>%
#   rename(site = 1, lat = 2, long = 3)
# 
# test <- batch_daymet(df_loc)
