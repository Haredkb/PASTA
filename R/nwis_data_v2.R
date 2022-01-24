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



source("R/daymet_data_v1.R")
source("R/paired_annualT_signals.R")
source("R/stream_thermal_sensitivity.R")
source("R/baseflow_regression.R")

#### ------ USER SPECIFIED VALUES --------- #####
### Connecting to NWIS data 
##Not Run
# siteNo <- read.delim("C:/Users/hared/Dropbox/UConn/Projects/300_Network_GW_Temp/200_Data_Input/Station_Info/USGS_all_T_2010_2018_Station_ID.txt",
#                      colClasses=c("site_no" ="character")) %>% ## needed to keep leading zeros in the names
#           select(site_no) #only keep names of the stations (even though lat long is here too)
# 
# siteNo <- siteNo[['site_no']] #convert to a vector 
# siteNo <- siteNo[100:110]
#   ##Working example of three 
#   #siteNo <-c( "01193500", "01435000","12115000") #12178000")
#   #start.date <- as.POSIXct("2015-03-03")
#   #end.date <- as.POSIXct("2019-03-03")
# 
# ### start and end date
# start.date <- as.POSIXct("2015-03-03")
# end.date <- as.POSIXct("2019-03-03")
# 
# test <- therm_analysis_nwis(siteNo, start.date, end.date)



##### ------- Internal Script ----------- #################

#right now start and end dates are values and SiteNo is a string of characters - 
#want each siteno to have its own site date - but that will be another day
#siteNo is a character vector of USGS site IDs, start.date and end date are Date or POSIX values, currently are only single dates, 
#need incorporate different dates for each site ID. 
therm_analysis_nwis <- function(siteNo, start.date, end.date, run_bfi){
  Start_time = Sys.time()
  
  
  #######################################################
  ####### ------ NWIS Download --------##################
  #######################################################
  
  #Include choice if bfi is run or not. 
  if(missing(run_bfi)){
    run_bfi <- FALSE
  }

  # #set up parameter codes
  # if(includeQ == TRUE){
  #   ### parameter code - temp and water
  #   pCode <- c("00010", "00060")#, "00095", "00300", "00400", "32315", "63680")
  # } else {
  #   pCode <- c("00010")
  # }
    
  pCode <- c("00010")
    
    
        ###ALL PARAMETERS NOT USED IN THIS Analysis BUT WANT TO KEEP 
        nwis_codes = data.frame(code=c("00060", "00010", "00095", "00300", "00400", "32315", "63680"), 
                                name=c( "Q_cfs", "Temp_C", "SPC_uScm", "DO_persat", "pH", "fCHL_RFU", "Turbidity_FNU"), 
                                stringsAsFactors=FALSE)
        #spc is corected for 25C
        
        # pCodeName <- c("Discharge", "TempC", "SPC", "DO", "pH", "fChl", "Turbidity")
        
        
        
        ### build a time series data frame using the readNWISuv function
        ### Download Stream Temperature- needs to be done seperate from 00060 pCode for Baseflow
          df_stream <- tryCatch( ##need to put in if statment so only runs if temp is available and Q runs if is there but not required. 
            {
            readNWISdv(siteNumbers = siteNo,
                                    parameterCd = pCode,
                                    startDate = start.date,
                                    endDate = end.date) %>%
              dplyr::select(-contains("cd"))%>%
              rename(tavg_water_C = "X_00010_00003")%>%
              filter(tavg_water_C > 1)
              },
          error=function(cond) {
            message(paste("Data does not exist or error reading data", siteNo, start.date, end.date))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            return(NA)
          }

          )
          
          ####Runnign Baseflow analysis 
          if(run_bfi == TRUE){
            
            df_stream <- tryCatch( ##need to put in if statment so only runs if temp is available and Q runs if is there but not required. 
              {
                df_Q <- readNWISdv(siteNumbers = siteNo,
                           parameterCd = "00060",
                           startDate = start.date,
                           endDate = end.date) %>%
                  dplyr::select(-contains("cd"))%>%
                  rename(Q_cfs = "X_00060_00003")
                
                df_stream <- left_join(df_stream, df_Q)
              },
              error=function(cond) {
                message(paste("Data does not exist or error reading Q data", siteNo, start.date, end.date))
                message("Here's the original error message:")
                message(cond)
                # Choose a return value in case of error
                return(df_stream)
              }
              
              
            )
          }

        
        # want this to be better but need it for the time being. 
        # names(data)[match(nwis_codes[,"code"], names(data))] = nwis_codes[,"name"]
        # 
        # test <- data %>%
        #   select(contains(nwis_codes$code))%>%
        #   rename()
        
        ##extract lat/long for daymet data mining
        df_loc <- attr(df_stream, "siteInfo") %>% #"Secret attributes" - want to know more about these
          dplyr::select(site_no, dec_lat_va, dec_lon_va, station_nm) %>%
          cbind(., start.date, end.date)#add column names of start and end date. 
        
        datum <- attr(df_stream, "siteInfo")$srs # would like to keep this around. 
        
        #################3
        
        ##########----- Download Daymet Data - Air Temperature ----------#####
        df_climate <- batch_daymet(df_loc)
        
        #df_loc$yday <-yday(df_loc$start.date)
        
        #convert year and yday column into a date 
        df_climate$Date <- as.Date(paste0(as.character(df_climate$year),"-01-01"))+ df_climate$yday -1
        climate_var <- unique(df_climate$measurement)
        temp_var <- c("tmax..deg.c.", "tmin..deg.c.")
        
        
        
        df_temp_air <- df_climate %>%
          rename("site_no" = "site") %>%
          filter(measurement %in% temp_var) %>%
          spread(measurement, value) %>%
          mutate(
            tavg_air_C = (tmax..deg.c. + tmin..deg.c.)/2
          )
        
        ####################
        
        ######## Create dataframe with air and water temperatures #######
        df_temp <- left_join(df_stream, df_temp_air, by = c("site_no", "Date"))
        df_temp$site_no <- as.factor(df_temp$site_no)
        
        
        #create list of each station dataframe 
        df_temp_l <- split(df_temp, df_temp$site_no)
        
        #####--------------------Baseflow Regression ----------------------#########

        #Only run if discharge column exists 
        if(run_bfi == TRUE){
      
          #Perform Baseflow Regression (output is volumetric baseQ)
          n <- names(df_temp_l) #keep list name lapply(setNames(n, n), function(nameindex)
          df_temp_l <- lapply(setNames(n, n), function(x){
            print(x)
            BaseQ <-tryCatch(baseflow_calc(
                                      df_temp_l[[x]][,"Date"], 
                                      df_temp_l[[x]][,"Q_cfs"],
                                      x), #station id
                                      error = function(cond){
                                        return(NA)
                             }
            )
            
            if(is.numeric(BaseQ) == TRUE){
              #add baseflow column to the main datalist
              df_temp_l[[x]] <- cbind(df_temp_l[[x]], BaseQ)
            } else{
              df_temp_l[[x]]
            }
          
          }
          )
          
          ## Perform Baseflow Index for each site, not for the same time period  - consider doing this later in the analysis if clipping for temp
          bfi_metric <- lapply(setNames(n, n), function(x){
            BFI_Score <- tryCatch(
              bfi <- round(mean(df_temp_l[[x]][,"BaseQ"], na.rm = T)/mean(df_temp_l[[x]][,"Q_cfs"] , na.rm = T), digits = 2),
            error = function(cond){
              return(NaN)
              }
              )
            }
          )
          
          
          BF_metrics <- as.data.frame(do.call("rbind", bfi_metric)) %>%
            rename(BFI = V1) %>%
            mutate(site_no = attr(., "row.names"))
        }

        
        #############################################################################
        ########----------------Thermal Senstivity Analysis ----------------#########
        #############################################################################
        
        #### COnduct Temperature Sensitivity - Kelleher 2012
        TS_lm_fit <- lapply(names(df_temp_l), function(x){
          T_lm_fit <- fit_ThermalSens(df_temp_l[[x]][,"Date"], 
                                      df_temp_l[[x]][,"tavg_air_C"], 
                                      df_temp_l[[x]][,"tavg_water_C"])
          #return(T_lm_fit)
        }
        )
        #name the lists based on site name
        names(TS_lm_fit) <- names(df_temp_l)
        
        ##Merge station metrics to a dataframe and add site name column
        TS_metrics <- do.call("rbind", TS_lm_fit)%>%
          mutate(site_no = attr(., "row.names"))
        
        ############## -----------------------------------------------------##########
        
        ############################################################################
        #####----------Paired Air SW Temperature Annual Signal Analysis ----######## 
        ############################################################################
        
        ## Calculate Temperature Annual Signal Fit for both Air and Water Temperature 
        TAS_sin_fit <- lapply(names(df_temp_l), function(x){
          # df_temp_l[[x]] %>%
          #   mutate(radian_day = rad_day(.[,"Date"]))
          
          Tair_fit <- fit_TAS(df_temp_l[[x]][,"Date"], df_temp_l[[x]][,"tavg_air_C"])
          Tair_fit$medium <- "air"
          Twat_fit <- fit_TAS(df_temp_l[[x]][,"Date"], df_temp_l[[x]][,"tavg_water_C"])
          Twat_fit$medium <- "water"
          
          #combine the water and air fits into one dataframe
          T_sin_fit <- rbind(Tair_fit, Twat_fit)
          T_sin_fit$site_no <- x
          
          return(T_sin_fit)
          }
          )
        #rename lists with the original input from df_temp_l
        names(TAS_sin_fit) <- names(df_temp_l)
        
        ##Calculate Amp Ratio and Phase Lag
        TAS_metrics <- do.call("rbind", TAS_sin_fit) %>%
          group_by(site_no) %>%
          summarize(AmpRatio = round(last(amplitude_C)/first(amplitude_C),2),
                    PhaseLag_d = round(last(phase_d) - first(phase_d),2))
        
        
        ################################################################
        ########## Merging all Metric Analyses together ###############
        ################################################################
        
        Metric_Output <- left_join(TAS_metrics, TS_metrics[,c(1,2,5)], 
                                               by = "site_no") %>%
                          left_join(., df_loc, by = "site_no")
        
        if(run_bfi == TRUE){
          Metric_Output <- left_join(Metric_Output, BF_metrics, by = "site_no")
        }
        
        output <- left_join(df_temp, TAS_metrics, by = "site_no")
        
        
      End_time <- Sys.time()
        
        print(End_time - Start_time)
        
  return(Metric_Output)
}
