# ### load and install packages
# library(dataRetrieval)
# library(tidyverse)
# library(lubridate)


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
              dplyr::rename(tavg_wat_C = tidyselect::any_of(c("X_00010_00003", "X_00010_00001", "X_00010_00008")),
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
         dplyr::rename(flow = tidyselect::any_of(c("X_00060_00003", "X_00060_00001", "X_00060_00008")),
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

# Estimate daily baseflow and BFI from discharge using a Lyne-Hollick filter.
calc_baseflow_daily <- function(df_flow, alpha = 0.925, passes = 3) {
  if (!all(c("site_id", "date", "flow") %in% names(df_flow))) {
    return(data.frame())
  }

  qf_pass <- function(q, alpha_val) {
    n <- length(q)
    qf <- rep(0, n)
    if (n < 2) {
      return(qf)
    }

    for (i in 2:n) {
      qf[i] <- alpha_val * qf[i - 1] + ((1 + alpha_val) / 2) * (q[i] - q[i - 1])
      qf[i] <- max(0, min(qf[i], q[i]))
    }
    qf
  }

  lyne_hollick <- function(q, alpha_val, n_passes) {
    q <- as.numeric(q)
    q[is.na(q) | q < 0] <- NA
    if (sum(!is.na(q)) < 3) {
      return(rep(NA_real_, length(q)))
    }

    q_filled <- q
    na_idx <- which(is.na(q_filled))
    if (length(na_idx) > 0) {
      q_filled <- stats::approx(x = which(!is.na(q_filled)),
                                y = q_filled[!is.na(q_filled)],
                                xout = seq_along(q_filled),
                                method = "linear",
                                rule = 2)$y
    }

    q_base <- q_filled
    for (k in seq_len(max(1, n_passes))) {
      if (k %% 2 == 1) {
        qf <- qf_pass(q_base, alpha_val)
      } else {
        qf <- rev(qf_pass(rev(q_base), alpha_val))
      }
      q_base <- pmax(0, q_base - qf)
    }

    b <- pmax(0, q_filled - qf)
    b <- pmin(b, q_filled)
    b <- pmin(b, q)
    b[is.na(q)] <- NA_real_
    b
  }

  df_out <- df_flow %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::arrange(site_id, date) %>%
    dplyr::group_by(site_id) %>%
    dplyr::group_modify(~{
      x <- .x %>% dplyr::arrange(date)
      bf <- lyne_hollick(x$flow, alpha, passes)
      x$baseflow <- pmax(0, pmin(bf, x$flow))
      x$bfi_daily <- ifelse(is.na(x$flow) | x$flow <= 0, NA_real_, pmin(1, x$baseflow / x$flow))
      x
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(site_id, date, baseflow, bfi_daily)

  df_out
}

calc_bfi_summary <- function(df_flow_with_baseflow) {
  if (!all(c("site_id", "flow", "baseflow") %in% names(df_flow_with_baseflow))) {
    return(data.frame())
  }

  df_flow_with_baseflow %>%
    dplyr::mutate(
      flow = as.numeric(flow),
      baseflow = pmax(0, pmin(as.numeric(baseflow), as.numeric(flow)))
    ) %>%
    dplyr::group_by(site_id) %>%
    dplyr::summarise(
      BFI = {
        den <- sum(flow[flow > 0], na.rm = TRUE)
        if (is.na(den) || den <= 0) NA_real_ else round(pmin(1, sum(baseflow, na.rm = TRUE) / den), 3)
      },
      .groups = "drop"
    )
}
