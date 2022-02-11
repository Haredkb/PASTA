### USE NWIS AND DAYMET DATA 


#could be useful https://stackoverflow.com/questions/35720660/how-to-use-an-r-script-from-github

### load and install packages
#
# install.packages("dataRetrieval")
library(dataRetrieval)

# install.packages("tidyverse")
library(tidyverse)
# 
# # install.packages("ggplot2")
# library(ggplot2)

library(lubridate)

#https://www.jumpingrivers.com/blog/personal-r-package/
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
        rename("site_id" =1, "date" = 2 , "tavg_wat_C"=3, "tavg_air_C" = 4)
      
      df_tem$year_water <-waterYear(as.POSIXct(as.character(df_tem$date), format="%Y-%m-%d"))
      
      return(df_tem)
}


############'
### CORE PREP ANALYSIS for Thermal Metrics(TM)
############
therm_analysis <- function(df_tem){#, df_loc){ 
  #df(3col) must have first column siteID, second column date, and third stream tempC; df_loc(3 col) must have siteID and lat long
  Start_time = Sys.time()

  df_temp <- df_tem %>%
    rename("site_id" =1, "date" = 2 , "tavg_wat_C"=3, "tavg_air_C" = 4)
  
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
