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


  #######################################################
  ####### ------ NWIS Download --------##################
  #######################################################

 readNWIS_Temp <- function(siteNo, start.date, end.date){
   
   pCode <- c("00010")#temp
    
        ### build a time series data frame using the readNWISuv function
        ### Download Stream Temperature- needs to be done seperate from 00060 pCode for Baseflow
          df_stream <- tryCatch( ##need to put in if statment so only runs if temp is available and Q runs if is there but not required. 
            {
              
            df <- readNWISdv(siteNumbers = siteNo,
                                    parameterCd = pCode,
                                    startDate = start.date,
                                    endDate = end.date)%>%
              dplyr::select(-contains("cd"))%>%
              dplyr::rename(tavg_wat_C = "X_00010_00003",
                     "site_id" = "site_no",
                     "date" = "Date")%>%
              filter(tavg_wat_C > 1)%>%
                dplyr::select(site_id, date, tavg_wat_C)
            
            ##extract lat/long for daymet data mining
            df_loc <- attr(df, "siteInfo") %>% #"Secret attributes" - want to know more about these
              dplyr::select(site_no, dec_lat_va, dec_lon_va, station_nm, srs, timeZoneOffset) %>%
              cbind(., start.date, end.date) %>%#add column names of start and end date. 
              dplyr::select(1,2,3,7,8,4:6) #to reorder for streamline use of daymet (site, lat, long, start, end)
            
            
            output <- list(df, df_loc)
            
            return(output)
            
              },
            
          error=function(cond) {
            
          if(grepl("X_00010_00003", cond, fixed = TRUE)){
            text <- paste("Data does not exist at", siteNo,"for", start.date, "to", end.date, 
                          "often this is due to a >yearly gap in data collection")
            message(text)
              return(text)
            
            }else({
              message(paste("There is a data download error with", siteNo))
              message(cond)
            # Choose a return value in case of error
              return(NA)
            })
                }#end error 
          )
          
        }
        
        #################

 
 readNWIS_Q <- function(siteNo, start.date, end.date){
   
   pCode <- c("00060")#discharge
   
   ### build a time series data frame using the readNWISuv function
   ### Download Stream Temperature- needs to be done seperate from 00060 pCode for Baseflow
   df_stream <- tryCatch( ##need to put in if statment so only runs if temp is available and Q runs if is there but not required. 
     {
       
       df <- readNWISdv(siteNumbers = siteNo,
                        parameterCd = pCode,
                        startDate = start.date,
                        endDate = end.date)%>%
         dplyr::select(-contains("cd"))%>%
         dplyr::rename(flow = "X_00060_00003",
                       "site_id" = "site_no",
                       "date" = "Date")%>%
         dplyr::select(site_id, date, flow)
       
       output <- list(df)
       
       return(output)
       
     },
     
     error=function(cond) {
       
       if(grepl("X_00060_00003", cond, fixed = TRUE)){
         text <- paste("Discharge Data does not exist at", siteNo,"for", start.date, "to", end.date, 
                       "often this is due to a >yearly gap in data collection")
         message(text)
         return(text)
         
       }else({
         message(paste("There is a discharge data download error with", siteNo))
         message(cond)
         # Choose a return value in case of error
         return(NA)
       })
     }#end error 
   )
   
 }
 
 #################
