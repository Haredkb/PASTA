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


##################################################
##################################################
##### Collect BFI ################################
##################################################
##################################################

#would be nice to add functionality to choose nearest USGS gauge station

# ####Runnign Baseflow analysis 
# if(run_bfi == TRUE){
#   
#   df_stream <- tryCatch( ##need to put in if statment so only runs if temp is available and Q runs if is there but not required. 
#     {
#       df_Q <- readNWISdv(siteNumbers = siteNo,
#                          parameterCd = "00060",
#                          startDate = start.date,
#                          endDate = end.date) %>%
#         dplyr::select(-contains("cd"))%>%
#         rename(Q_cfs = "X_00060_00003")
#       
#       df_stream <- left_join(df_stream, df_Q)
#     },
#     error=function(cond) {
#       message(paste("Data does not exist or error reading Q data", siteNo, start.date, end.date))
#       message("Here's the original error message:")
#       message(cond)
#       # Choose a return value in case of error
#       return(df_stream)
#     }
#     
#     
#   )
# }
# 
# #Only run if discharge column exists 
# runBFRegression <- function(run_bfi)
#       {
#         if(run_bfi == TRUE){
#         #Perform Baseflow Regression (output is volumetric baseQ)
#         n <- names(df_temp_l) #keep list name lapply(setNames(n, n), function(nameindex)
#         df_temp_l <- lapply(setNames(n, n), function(x){
#           print(x)
#           BaseQ <-tryCatch(baseflow_calc(
#             df_temp_l[[x]][,"Date"], 
#             df_temp_l[[x]][,"Q_cfs"],
#             x), #station id
#             error = function(cond){
#               return(NA)
#             }
#           )
#           
#           if(is.numeric(BaseQ) == TRUE){
#             #add baseflow column to the main datalist
#             df_temp_l[[x]] <- cbind(df_temp_l[[x]], BaseQ)
#           } else{
#             df_temp_l[[x]]
#           }
#           return(df_temp_l)
#         }
#         )
#         }else(
#           return(NULL)
#         )
# }
#   
#   CalcBFIndex <- function(list_Q)
#         {
#         ## Perform Baseflow Index for each site, not for the same time period  - consider doing this later in the analysis if clipping for temp
#         bfi_metric <- lapply(setNames(n, n), function(x){
#           BFI_Score <- tryCatch(
#             bfi <- round(mean(df_temp_l[[x]][,"BaseQ"], na.rm = T)/mean(df_temp_l[[x]][,"Q_cfs"] , na.rm = T), digits = 2),
#             error = function(cond){
#               return(NaN)
#             }
#           )
#         }
#         )
#       
#       #COnvert List to Dataframe
#         BF_metrics <- as.data.frame(do.call("rbind", bfi_metric)) %>%
#           rename(BFI = V1) %>%
#           mutate(site_no = attr(., "row.names"))
#         return(BF_metrics) # site names and BFI index
#         
#       }
# 
