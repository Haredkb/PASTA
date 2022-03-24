### USE NWIS AND DAYMET DATA 


#could be useful https://stackoverflow.com/questions/35720660/how-to-use-an-r-script-from-github

### load and install packages
# install.packages("dataRetrieval")
library(dataRetrieval)
# install.packages("tidyverse")
library(tidyverse)
# # install.packages("ggplot2")
# library(ggplot2)
library(lubridate)
#http://www.jumpingrivers.com/blog/personal-r-package/
# path = file.path(tempdir(), "AnnualTSignals")
# usethis::create_package(path)
library(smwrBase) #for water year
source("R/paired_annualT_signals.R")
source("R/stream_thermal_sensitivity.R")
#source("R/baseflow_regression.R")


############
## Data Retrival package has this function, but just to raw USGS files. 
############

add_waterYear <- function(df_tem){
  #rename temperature dataframe
      df_tem <- df_tem %>%
        dplyr::rename("site_id" =1, "date" = 2 , "tavg_wat_C"=3, "tavg_air_C" = 4)
      
      df_tem$year_water <-waterYear(as.POSIXct(as.character(df_tem$date), format="%Y-%m-%d"))
      
      return(df_tem)
}

##############
##Retrieve HydroShare Data
##########
filelist_retrieval <- function(resource_id, username, password){
  
  #create url
  filelist_get_url <- paste0("http://www.hydroshare.org/hsapi/resource/", resource_id, "/files") #/?page=1")
  
  filelist_response = GET(filelist_get_url, authenticate(username,password, type = "basic"))
  
  #read content
  json_text <- content(filelist_response,"text")
  json <- content(filelist_response)
  json$text
  # Converting content into Dataframe
  filelist <- jsonlite::fromJSON(json_text)
  
  #Create list of available Files
  files_avail <- as.data.frame(filelist$results)
}
############'
### CORE PREP ANALYSIS for Thermal Metrics(TM)
############
therm_analysis <- function(df_tem){#, df_loc){ 
  #df(3col) must have first column siteID, second column date, and third stream tempC; df_loc(3 col) must have siteID and lat long
  Start_time = Sys.time()

  df_temp <- df_tem %>%
    dplyr::rename("site_id" =1, "date" = 2 , "tavg_wat_C"=3, "tavg_air_C" = 4)
  
  ### ADD IN MAKING SURE ITS DAILY TIME STEPS
  
  ### Add in sepate function 
  # df_loc <- df_loc %>%
  #   rename("site_id" =1, "lat" =2 , "long"=3, "start.date" = 4, "end.date" = 5)%>%
  #   dplyr::select(2:5,1)#reorder so lat and long are 1 and 2

  #make station ids factors for grouping
  df_temp$site_id <- as.factor(df_temp$site_id)
        
        
  #create list of each unique station id dataframe 
  df_temp_l <- split(df_temp, df_temp$site_id)
        
        
        ########----------------Thermal Senstivity Analysis ----------------#########
        #### Conduct Temperature Sensitivity - Kelleher 2012
        TS_lm_fit <- lapply(names(df_temp_l), function(x){
          T_lm_fit <- fit_ThermalSens(df_temp_l[[x]][,"date"], 
                                      df_temp_l[[x]][,"tavg_air_C"], 
                                      df_temp_l[[x]][,"tavg_wat_C"])
        }
        )
        #name the lists based on site name
        names(TS_lm_fit) <- names(df_temp_l)
        
        ##Merge station metrics to a dataframe and add site name column
        TS_metrics <- do.call("rbind", TS_lm_fit)%>%
          mutate(site_id = attr(., "row.names"))
        
        ############## -----------------------------------------------------##########
        
        
        #####----------Paired Air SW Temperature Annual Signal Analysis ----######## 
        
        ## Calculate Temperature Annual Signal Fit for both Air and Water Temperature 
        TAS_sin_fit <- lapply(names(df_temp_l), function(x){
          # df_temp_l[[x]] %>%
          #   mutate(radian_day = rad_day(.[,"Date"]))
          
          Tair_fit <- fit_TAS(df_temp_l[[x]][,"date"], df_temp_l[[x]][,"tavg_air_C"])
          Tair_fit$medium <- "air"
          Twat_fit <- fit_TAS(df_temp_l[[x]][,"date"], df_temp_l[[x]][,"tavg_wat_C"])
          Twat_fit$medium <- "water"
          
          #combine the water and air fits into one dataframe
          T_sin_fit <- rbind(Tair_fit, Twat_fit)
          T_sin_fit$site_id <- x
          
          return(T_sin_fit)
          }
          )
        #rename lists with the original input from df_temp_l
        names(TAS_sin_fit) <- names(df_temp_l)
        
        ##Calculate Amp Ratio and Phase Lag
        TAS_metrics <- do.call("rbind", TAS_sin_fit) %>%
          group_by(site_id) %>% #last is water first is air 
          summarize(AmpRatio = round(last(amplitude_C)/first(amplitude_C),2),
                    PhaseLag_d = round(last(phase_d) - first(phase_d),2),
                    Ratio_Mean = round(last(YInt) / first(YInt),2))
        
        
        ################################################################
        
        ########## Mergeing all Metric Analyses together #################
        
        
        Metric_Output <- left_join(TAS_metrics, TS_metrics[,c(1,2,4,5)], #TS_slope, Rsq, Yint, and Site_id
                                               by = "site_id") #%>%
        #do in a seperate analysis                   
        #left_join(., df_loc, by = "site_id")
        
        
        output <- left_join(df_temp, TAS_metrics, by = "site_id")
        
        
      End_time <- Sys.time()
        
        print(End_time - Start_time)
        
  return(Metric_Output)
}


# #Temperature Norwest Data
library(readxl)
library(tidyverse)
library(foreign) #for reading dbf
library(plyr) #rbind.fill


#PU <- c("eastern-montana", "SnakeBear")


###############################################
##      NorWeST Data Scraping Sites      ##
###############################################
getNorwestData <- function(NorweStProcessingUnit){
  for(i in NorweStProcessingUnit){
    #find link to access resource list
    link <- "https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/StreamTemperatureDataSummaries.shtml"
    df_link <- "https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/"
    
    html <- read_html(link)
    #find links with "resource"
    fils_url <- as_tibble(html_attr(html_nodes(html, xpath= sprintf(".//a[contains(@href,'%s')]", i)), "href"))
    #fils_df <- html_nodes(html, xpath= ".//a[contains(@href,'Daily')]")
    #fils_shp <- html_nodes(html, xpath=".//a[contains(@href,'ObservedTempPoints')]")
    
    fils_df <- fils_url%>%
      dplyr::filter(grepl("Daily",value))
    
    fils_shp <- fils_url%>%
      dplyr::filter(grepl("ObservedTempPoints",value))
    
    #link_end <- (xml_attrs(fils))
    
    link_df = sprintf("https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/%s", fils_df[1,])
    link_shp = sprintf("https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/%s", fils_shp[1,])
    
    #download files from web
    temp <- tempfile()
    temp2 <- tempfile()
    download.file(link_df,temp)
    data <- readxl::read_excel(unzip(temp))
    
    names(data)[2] <- tolower(names(data)[2])#trying to find some consistency is Norwest _id always 2? the cases are 
    #-----------------------------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------------------------#
    download.file(link_shp,temp)
    unzip(zipfile = temp, exdir = temp2)
    #finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
    #the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml 
    SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)
    #read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
    data_loc <- sf::read_sf(SHP_file)
    
    
    data_loc$NorPU <- gsub("\\_.*","", data$norwest_id[1]) #add processing unit using the nomenclature from data file
    data_loc$norwest_id <- paste0(data_loc$NorPU,"_",data_loc$OBSPRED_ID)
    data_loc <- data_loc %>% #transform to wgs84 and make lat and long columns
      st_transform(., CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%#'+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs')
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    
    #Add PERMA_FID to data file
    data <- dplyr::select(data_loc, "PERMA_FID", "norwest_id")%>%
      inner_join(data, .)
    
    #create loc dataframe
    if (exists("df_loc") ==TRUE){
      df_loc <- plyr::rbind.fill(df_loc, data_loc) #woohoo! fill allows for nonmatching columns - retaining them and putting NA!
    }else{
      df_loc <- data_loc
    }
    
    #create data dataframe
    if (exists("df_dat") ==TRUE){
      df_dat <- plyr::rbind.fill(df_dat, data) #woohoo! fill allows for nonmatching columns - retaining them and putting NA!
    }else{
      df_dat <- data
    }
    
  }
  
  nor_list <- list(df_dat, df_loc)
  return(nor_list)
}
