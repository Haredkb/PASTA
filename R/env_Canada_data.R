
#Temperature Environmental Canada Data

getEnvCanStations <- function(x){
  ###############################################
  ##      ENV Canada Data Scraping Sites      ##
  ###############################################
  #link to access resource list
  
  link <- "https://open.canada.ca/data/dataset/f258b0c8-7871-4572-b567-1ba2bd55f1b6/resource/5791e913-16ee-4d33-823a-f11fed5a15de?inner_span=True"
  
  #beginning link for data
  df_link <- "https://data-donnees.ec.gc.ca/data/substances/monitor/automated-fresh-water-quality-monitoring-and-surveillance-data/"
  #example built from https://stackoverflow.com/questions/46673331/using-r-to-get-download-url-by-link-name
  
  html <- read_html(link)
  #find links with "resource"
  fils <- html_nodes(html, xpath=".//a[contains(@href, 'resource')]")
  
  #read and create table
  xdf <-tibble(
    filename = html_text(fils),
    #link = sprintf("https://open.canada.ca%s", html_attr(fils, "href"))
    link = sprintf(paste0(df_link, "%s"), substr(filename,1,nchar(filename)-4)),#remove extra CSV from end
    data_name = gsub('.{8}$', '', sub(".*eau-", "", filename))#remove last 8 and keep from eau
  ) 
  
  #
  xdf <- xdf[grepl('csv', xdf$filename) & grepl('-', xdf$data_name),]
  ##create column of station id for combining with station id table 
  xdf$STATION_NO <- map(strsplit(xdf$data_name,"-"),1)# needs purrr
  xdf$STATION_NO <- sub("\\_.*", "", xdf$STATION_NO) #one errenous staion id seperatred with "_" 
  
  
  envCan_stations <- read.csv("https://data-donnees.ec.gc.ca/data/substances/monitor/automated-fresh-water-quality-monitoring-and-surveillance-data/auto-water-qual-eau-stations.csv")
  
  output <- left_join(xdf, envCan_stations)
  #this is what I want the table to plot
  #station_id <- unique(xdf$station_id)
  
}

#site_names <- unique(getEnvCanStations()$STATION_NO)[1:4]

getEnvCanData <- function(site_names, variable = "TEMPERATURE WATER"){
   station_list <- getEnvCanStations()%>%#
     dplyr::filter(STATION_NO %in% site_names)
   
   station_data <- lapply(station_list$link, read.csv)
   
   output <- do.call("rbind", station_data)%>%
       dplyr::filter(grepl('Temperature|temperature|temp', VARIABLE))%>%
        mutate(site_id = STATION_NO,
               date = as.Date(DATE_TIME_HEURE, "%d/%m/%Y"))%>%
     group_by(site_id) %>%
     timetk::summarise_by_time( #for daily time steps from hourly.
       .date_var = date,
       .by       = "day", # Setup for monthly aggregation
       # Summarization
       tavg_wat_C = mean(as.numeric(VALUE_VALEUR)))
   #need to get out of data to call select on group_by
   output <- output %>%
        dplyr::select(site_id, date, tavg_wat_C)
   
   return(output)
     
}

