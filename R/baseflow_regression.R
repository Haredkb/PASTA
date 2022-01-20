##USGS hydrograph seperation 
##2021-11-22
##libraries
library(dplyr)
library(smwrBase)
library(tidyr)
library(lubridate) 
library(DVstats) #for baseflow

### bfi

baseflow_calc <- function(date, flow, site_no){
  #create dataframe
  df <- as.data.frame(flow) %>%
    cbind(., date)  #date can only be added as.date once the datafram has already been created

  # # Calculate if any missing dates, will not run if so
      df$flowFill <- fillMissing(df$flow, max.fill = 14) 
      #Trim any NAs from the end or beginning - doing this after fill will remove any trailing NAs for data that begins or ends at off times or throw an error for sets missing more than 2 weeks of data (combined with "fillMissing")
      # %>% drop_na(df, any_of("flowFill"))
      
      # ggplot(df) +
      #geom_point(aes(x = date, y = flow))
        
      ### RUN BASEFLOW ANALYSIS 
      if(any(is.na(df$flowFill))== TRUE){ #IF datagraps are too large
        message(paste(x, " has data Gaps larger than 14 days, baseflow analysis not conducted"))
        return("no BF")
        #df_bfi$BaseQ <- rep(NA, length = length(flow))
        
      } else { 
      
          #Perform BFI calculation with bfi
        try(
          df_bfi <- with(df, #dont need the with statment when using this fuction with apply - investigate....
                           bfi(df$flowFill, df$date, 
                               by="continuous", 
                               STAID= "Unknown"))
                            #site_no))
          )
        
        #df <- df_bfi
      }
          #try( df_bfi <- na.omit (df_bfi))#remove rows with NA for stat calculations
          #try( df_bfi$site_no <- id)#add site id column to bfi dataframe
          #try(BFI_Score <- round(mean(df_bfi$BaseQ)/mean(df_bfi$Flow), digits = 2))
          
      return(df_bfi$BaseQ) # a vector the same length as entered
} 

###hysep