# #Temperature Norwest Data
library(readxl)
library(tidyverse)
library(foreign) #for reading dbf
library(plyr) #rbind.fill


#PU <- c("eastern-montana", "SnakeBear")


###############################################
##      NorWeST Data Scraping Sites      ##
###############################################
getNorwestData <- function(NorweStProcessingUnit){
  for(i in NorweStProcessingUnit){
  #find link to access resource list
    link <- "https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/StreamTemperatureDataSummaries.shtml"
    df_link <- "https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/"

    html <- read_html(link)
    #find links with "resource"
    fils_url <- as_tibble(html_attr(html_nodes(html, xpath= sprintf(".//a[contains(@href,'%s')]", i)), "href"))
    #fils_df <- html_nodes(html, xpath= ".//a[contains(@href,'Daily')]")
    #fils_shp <- html_nodes(html, xpath=".//a[contains(@href,'ObservedTempPoints')]")
    
    fils_df <- fils_url%>%
      dplyr::filter(grepl("Daily",value))
    
    fils_shp <- fils_url%>%
      dplyr::filter(grepl("ObservedTempPoints",value))
    
    #link_end <- (xml_attrs(fils))

    link_df = sprintf("https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/%s", fils_df[1,])
    link_shp = sprintf("https://www.fs.fed.us/rm/boise/AWAE/projects/NorWeST/%s", fils_shp[1,])
    
    #download files from web
    temp <- tempfile()
    temp2 <- tempfile()
    download.file(link_df,temp)
    data <- readxl::read_excel(unzip(temp))
    
    names(data)[2] <- tolower(names(data)[2])#trying to find some consistency is Norwest _id always 2? the cases are 
    #-----------------------------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------------------------#
    #-----------------------------------------------------------------------------------------------#
    download.file(link_shp,temp)
    unzip(zipfile = temp, exdir = temp2)
    #finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
    #the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml 
    SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)
    #read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
    data_loc <- sf::read_sf(SHP_file)
    
    
    data_loc$NorPU <- gsub("\\_.*","", data$norwest_id[1]) #add processing unit using the nomenclature from data file
    data_loc$norwest_id <- paste0(data_loc$NorPU,"_",data_loc$OBSPRED_ID)
    data_loc <- data_loc %>% #transform to wgs84 and make lat and long columns
      st_transform(., CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%#'+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs')
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    
    #Add PERMA_FID to data file
    data <- dplyr::select(data_loc, "PERMA_FID", "norwest_id")%>%
      inner_join(data, .)
    
    #create loc dataframe
    if (exists("df_loc") ==TRUE){
      df_loc <- plyr::rbind.fill(df_loc, data_loc) #woohoo! fill allows for nonmatching columns - retaining them and putting NA!
    }else{
      df_loc <- data_loc
    }
    
    #create data dataframe
    if (exists("df_dat") ==TRUE){
      df_dat <- plyr::rbind.fill(df_dat, data) #woohoo! fill allows for nonmatching columns - retaining them and putting NA!
    }else{
      df_dat <- data
    }
  
  }
  
  PU_dat <- list(df_dat, df_loc)
  return(PU_dat)
}
# 
# 
# 
# 
