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
            readNWISdata(siteNumbers = siteNo,
                                    parameterCd = pCode,
                                    startDate = start.date,
                                    endDate = end.date) %>%
              dplyr::select(-contains("cd"))%>%
              rename(tavg_wat_C = "X_00010_00003",
                     "site_id" = "site_no",
                     "date" = "dateTime")%>%
              filter(tavg_wat_C > 1)%>%
                dplyr::select(site_id, date, tavg_wat_C)
              },
          error=function(cond) {
            message(paste("Data does not exist or error reading data", siteNo, start.date, end.date))
            message("Here's the original error message:")
            message(cond)
            # Choose a return value in case of error
            return(NA)
          }

          )
          
        ##extract lat/long for daymet data mining
        df_loc <- attr(df_stream, "siteInfo") %>% #"Secret attributes" - want to know more about these
          dplyr::select(site_no, dec_lat_va, dec_lon_va, station_nm, srs, timeZoneOffset) %>%
          cbind(., start.date, end.date) %>%#add column names of start and end date. 
          dplyr::select(1,2,3,7,8,4:6) #to reorder for streamline use of daymet (site, lat, long, start, end)
        
        
        output <- list(df_stream, df_loc)
        }
        
        #################

