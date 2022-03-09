suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(dplyr)
  library(lubridate)
  library(plotly)
  library(shinythemes)
  library(dataRetrieval)#USGS-R
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
  library(plotly)
  library(ggplot2)
  library(viridis)
  #web scraping
  library(xml2)
  library(rvest)
  #library(dygraphs)
  require("httr")
  require("stringr")
  
  envCan_stations <- read.csv("https://data-donnees.ec.gc.ca/data/substances/monitor/automated-fresh-water-quality-monitoring-and-surveillance-data/auto-water-qual-eau-stations.csv")
  #library(DT)
  source("R/nwis_data_v2.R")
  source("R/daymet_data_v2.R")
  source("R/rundata_v1.R")
  source("R/env_Canada_data.R")
  source("R/plotFunctions.R")
  #for app functions
  source("serverFunctions.R")
  source("uiFunctions.R")

})


