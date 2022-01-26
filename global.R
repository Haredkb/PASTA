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
  #library(shinycssloaders)
  # library(shinyWidgets)
  library(plotly)
  library(ggplot2)
  #library(dygraphs)
  
  
  #library(DT)
  source("R/nwis_data_v2.R")
  source("R/daymet_data_v2.R")
  source("R/rundata_v1.R")
  
  #for app functions
  source("serverFunctions.R")
  source("uiFunctions.R")

})


