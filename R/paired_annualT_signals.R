## Annual Signal Functions
library(lubridate)
## ----------- Calcuate radian date from date------ ##
rad_day <- function(x){ #input date vector
  d <- yday(as.POSIXct(x))
               
  rad_d <- 2*pi*d/365
  return(rad_d)
}

#TAS: Temperature Annual Signal
#can be used for air temp and surface water temperature extraction of annual signal 
fit_TAS <- function(date, temp){
  
  df <- as.data.frame(unlist(temp)) %>%
    cbind(., date) %>%#has to be done second to keep format (?)
    rename("temp" = 1)
  #convert to radian date for sinsoidal extract
  df$rday <- rad_day(df$date)
  
  #to convert back to Phase Days
  units_day <- 365
  
  #conduct linear fit to a sinsddial function 
  Tfit.lm <- lm(temp ~ sin(rday) + cos(rday), data = df)
  
  #extract equation for the fit 
  Tsin.lm <- coef(Tfit.lm)['sin(rday)']*sin(df$rday) +
    coef(Tfit.lm)['cos(rday)']*cos(df$rday) +
    coef(Tfit.lm)['(Intercept)']
  
  #Calculate Phase of the signal in days
  Phase <- (units_day/(2*pi))*((3*pi/2) -atan(coef(Tfit.lm)['cos(rday)']/
                                                   coef(Tfit.lm)['sin(rday)']))
  
  #Calculate Amplitude of the signal 
  Amp <- sqrt((coef(Tfit.lm)['sin(rday)']^2) + (coef(Tfit.lm)['cos(rday)']^2))
  
  #remove names to make single values
  names(Phase) <- NULL; names(Amp) <- NULL
  
  #create dataframe output summary data
  lmStats <- data.frame(amplitude_C = Amp,
                          phase_d = Phase,
                        AdjRsqr=summary(Tfit.lm)$adj.r.squared,
                            RMSE=sqrt(mean(resid(Tfit.lm)^2)),
                            sinSlope=coef(Tfit.lm)['sin(rday)'],
                            cosSlope=coef(Tfit.lm)['cos(rday)'],
                            YInt=coef(Tfit.lm)['(Intercept)'])#; rownames(lmStats) <- "Air" #would like site_no
  
  
return(lmStats)
  
  
  }


#test <- lapply(list, function)

