suppressPackageStartupMessages({
  
  ### RMEOVED 12-5-2023 due to GITLAB REPO not recongized. 
  #install.packages(c('smwrBase', 'smwrGraphs', 'smwrStats', 'smwrQW'), repo='https://owi.usgs.gov/R/', type="source")
  #remotes::install_github("USGS-R/smwrBase@v1.1.0")
  # remotes::install_gitlab("water/analysis-tools/smwrData", 
  #                         host = "code.usgs.gov", force = TRUE)
  #library(smwrBase)
  #remotes::install_github("USGS-R/DVstats")
  # remotes::install_gitlab("water/analysis-tools/DVstats", 
  #                         host = "code.usgs.gov")
  #library(DVstats) #for baseflow
  
  library(shiny)
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(shinythemes)
  library(dataRetrieval)#USGS-R, still on github
  #remotes::install_github("khufkens/daymetr")
  library(daymetr)
  library(DT) #for table selection options 
  library(leaflet)
  library(shinybusy) #for spinning icon
  library(timetk)
  library(shinydashboard)
  library(shinyauthr)#for username and password functionality
  library(shinyjs)#https://github.com/daattali/shinyjs#overview-main
  #for hiding and toggling 
  #library(shinycssloaders)
  library(shinyWidgets)
  library(shinyalert)
  library(plotly)
  library(ggplot2)
  library(viridis)
  library(tidyr)
  #web scraping
  library(xml2)
  library(rvest)
  library(zip)
  library(plyr) #rbind.fill
  library(reshape2) ##melting dataframes
  library(dplyr) #data wrangling
  library(raster) ##working with raster data
  library(sp) ##manipulationg spatial data
  ###Norwest
  library(readxl)

  library(tidyverse)
  library(foreign) #for reading dbf
  
  
  
  #library(dygraphs)
  require("httr")
  require("stringr")
  require("sf")
  envCan_stations <- read.csv("https://data-donnees.ec.gc.ca/data/substances/monitor/automated-fresh-water-quality-monitoring-and-surveillance-data/auto-water-qual-eau-stations.csv")
  source("R/nwis_data_v2.R")
  source("R/daymet_data_v2.R")
  source("R/rundata_v1.R")
  source("R/norWeST_data.R")
  #source("R/env_Canada_data.R")
  source("R/plotFunctions.R")
  #source("R/baseflow_regression.R")
  #for app functions
  source("serverFunctions.R")
  source("uiFunctions.R")

})


