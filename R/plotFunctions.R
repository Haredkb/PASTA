####################################
##Plot Functions
####################################

#create df for plot functions
create_TMplot_df <- function(df, yr_type){
      df_temp_l <- df %>% #Tem_df() %>%
          dplyr::filter(tavg_air_C < 120 | tavg_wat_C < 60) %>%
          split(f = as.factor(.$site_id))
        # group_by(site_id, .add = TRUE) %>%
        # group_split(.) 
        
        #water sin fit coefficients
        sin_wfit_coef <- lapply(names(df_temp_l), function(x){
          y <- fit_TAS(df_temp_l[[x]][,"date"], df_temp_l[[x]][,"tavg_wat_C"], yr_type)
          z <- mutate(y, site_id = x)#add column with site_id as it is droped in the lapply process
        })%>% 
          do.call("rbind", .)#make dataframe for sin coefficients
      
        #stream data - with full set
        p_df_w <- df %>%
          mutate(site_id = as.character(site_id)) %>%
          left_join(., sin_wfit_coef, by = "site_id") %>%
          mutate(sin_fit_w = (sinSlope * sin(rad_day(date, yr_type))) + (cosSlope * cos(rad_day(date, yr_type))) + YInt)
        
        #sin coeffiecients
        sin_afit_coef <- lapply(names(df_temp_l), function(x){
          y <- fit_TAS(df_temp_l[[x]][,"date"], df_temp_l[[x]][,"tavg_air_C"], yr_type)
          z <- mutate(y, site_id = x)#add column with site_id as it is droped in the lapply process
        })%>% 
          do.call("rbind", .)#make dataframe for sin coefficients
        
        #air data only keep to bind
        p_df_a <- df %>%
          mutate(site_id = as.character(site_id)) %>%
          left_join(., sin_afit_coef, by = "site_id") %>%
          mutate(sin_fit_a = (sinSlope * sin(rad_day(date, yr_type)) + (cosSlope * cos(rad_day(date, yr_type))) + YInt))%>%
          dplyr::select("site_id", "date", "sin_fit_a")
          
        p_df <- left_join(p_df_w, p_df_a, by = c("site_id", "date"))%>%
          dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))
        
      return(p_df)
}
  
#Plot Raw and Fit Temperature Data  - TS is timeseries here
p_dataTS <- function(p_df){#p_df is a reactiveValue
  #trying to remove line connections with following code (NA should make geom_path have gaps)
  p_df <- p_df %>% group_by(site_id) %>%
    group_by(grp = cumsum(difftime(date, lag(date, default = first(date)), units = 'days') > 30), .add = TRUE) %>%
    complete(date = seq(min(date), max(date), by = 'days')) %>%
    ungroup
  
  
  p <- ggplot(data = p_df) +
    geom_point(aes(x = date, y = tavg_air_C, color = "Air Raw"))+
    geom_point(aes(x = date, y = tavg_wat_C,color = "Water Raw"))+
    geom_path(aes(x = date, y = sin_fit_a, group = grp, color = "Air Fit") )+
    geom_path(aes(x = date, y = sin_fit_w, group = grp, color = "Water Fit"))+
    xlab("Date")+
    ylab("Temperature (C)")+
    theme_bw()+
    facet_grid(rows = vars(site_id))+
    scale_color_manual(name = "Temperature Data",
                       breaks=c("Air Raw", "Air Fit", "Water Raw", "Water Fit"),
                       values=c("Air Raw" = "orange", "Air Fit" = "red", "Water Raw" = "lightblue", "Water Fit" = "blue"))+
    theme(legend.title=element_text(size=20),
          legend.text=element_text(size=14),
          legend.position = "top")

    p
}

#plot linear regression
plot_TMlm <- function(p_df){
  p <- p_df %>%
    filter(tavg_wat_C > 1)%>%
    ggplot(aes(x = tavg_air_C, y = tavg_wat_C, color = factor(year(date)))) +
    geom_point()+
    stat_smooth(method = "lm", col = "red")+
    geom_abline(slope = 1, intercept = 0, lty= 2)+
    # Add a legend to the plot
    #geom_text("Black Line is 1:1", colour = "black")+
    scale_color_viridis(discrete=TRUE, option = "turbo")+
    labs(x = "Air Temperature (C)", y= "Water Temperature (C)", colour="Year")+
    xlim(0, NA)+
    theme_bw()+
    facet_grid(rows = vars(site_id)) #rows = vars(site_id))
}
# 

#plot annual sinusoidal results
plot_TMas <-function(TM_data){
  p <- ggplot(TM_data) +
    geom_point(aes(x = PhaseLag_d, y = AmpRatio, shape = factor(site_id), color = factor(year)))+
    #ggtitle("test")+
    #scale_shape_manual(values=seq(0,15))+
    scale_color_viridis(discrete=TRUE, option = "turbo")+
    labs(x = "Phase Lag (days)", y= "Amplitude Ratio", shape="Site ID")+
    ylim(0,1.2)+
    theme_bw()
}


###################################
#Leaflet Functions provided by Zach Johnson
###################################

# Custom functions for leaflet map
zSetupMap <- function(TM_data){
  result <- leaflet(data=TM_data) %>% addTiles(group='OpenStreetMap') %>% # create leaflet map & add base layer
    addProviderTiles(providers$OpenTopoMap,group="OpenTopoMap") %>% # add OpenTopoMap base layer
    addProviderTiles(providers$Esri.WorldImagery,group="Esri.WorldImagery")
  return(result)
} # end fxn

zAddLegend <- function(map,leg.pal,table,data.cols,bins,name,unit){
  result <- map %>%
    addLegend('bottomright',pal=colorBin(palette=leg.pal,
                                         domain=unlist(table[,data.cols]),bins=bins),
              values=unlist(table[,data.cols]),
              title=paste0(name,' (',unit,')'), opacity=0.75)
  return(result)
} # end fxn
