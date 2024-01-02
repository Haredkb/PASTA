### Thermal Sensitivity Functions
# ### based off of kellher et al. 2012
# library(dplyr)
# library(lubridate)


fit_ThermalSens <- function(date, a_temp, w_temp, y){
  
  #create dataframe
  df <- as.data.frame(cbind(unlist(w_temp), unlist(a_temp))) %>%
    dplyr::rename("w_temp" = 1, "a_temp" = 2)%>%
    cbind(., date) %>% #date can only be added as.date once the datafram has already been created%>%
    dplyr::filter(w_temp > 1)%>% #& a_temp > 0) #still have - stream values
    na.omit()
  
  rownames(df)<-NULL
    
  #do not know why it is a list NWIS does not throw these errors...
  
  #daily or weekly averages
  if(missing(y)) {
    #Average Daily Linear Thermal Sensitivity 
    TS_fit.lm <- lm(w_temp ~ a_temp, data = df)
    
  } else if (y == "weekly"){ 
    #summarize data weekly
    df <- df %>% 
      group_by(year = year(date), week = week(date)) %>% 
      summarise(a_temp = mean(a_temp),
                w_temp = mean(w_temp))
                
      #Average Weekly Linear Thermal Sensitivity 
      TS_fit.lm <- lm(w_temp ~ a_temp, data = df)
      
  } else {
      stop(message(cat("Error in specified summarized time")))
    }
    
    
  
  # ##create plot for raw data
  # p <- plot(df$a_temp, df$w_temp)
  # print(p)
  # 
  
  
  TS_output <- data.frame(TS__Slope =round(coef(TS_fit.lm)[2],2), 
                          AdjRsqr=round(summary(TS_fit.lm)$adj.r.squared,2),
                          RMSE=sqrt(mean(resid(TS_fit.lm)^2)),
                          YInt=round(coef(TS_fit.lm)['(Intercept)'],2)
                          )
  return(TS_output)
  
}
