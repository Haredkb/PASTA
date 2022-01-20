### Server Functions

###########################################################################
###########################################################################
###                                                                     ###
###                             NWIS SERVER                             ###
###                                                                     ###
###########################################################################
###########################################################################
nwisServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
                #Define State - State Selection is Default for NWIS 
                state <- reactive({
                  as.character(input$state)
                })
                
                #################################################################
                ##                   Chosing Input type nwis                   ##
                #################################################################
                #-----------------------------------------------#
                #       Map Extent vs. State Selection          #
                #-----------------------------------------------#
                NWIS_sites <-  eventReactive(input$searchsites, {
                  if(input$mapextent == TRUE){
                    #extract map bounds
                    bounds <- input$dataavailmap_bounds
                    print(bounds)
                    print(class(bounds))
                    #bounds <- as.numeric(bounds)
                    bbox_in<- c(round(bounds$west,2), round(bounds$south,2), round(bounds$east,2), round(bounds$north,2))
                    print(bbox_in)
                    
                    #print the Area within the sidebar
                    output$AOI <- renderText({ bbox_in })
                    
                    #Bounding Box too large [13.3x3.6 degrees]. Your requested width must be less than or equal to  8.4 degrees at latitude 33.6 with requested height of 3.6 degrees.
                    
                    #------Use Bounding Box to define NWIS data pull---------------------#
                    
                    df <- whatNWISdata(
                      bBox = bbox_in, #min/max lng/lat c(bounds$west, bounds$south, bounds$east, bounds$north)
                      siteType="ST",
                      parameterCd=c("00010"), #temp
                      service="dv",
                      statCd="00003")%>%
                      mutate_at(vars(contains('date')), ~ as.Date(., "%Y-%m-%d"))
                    
                    
                    #------Use State to define NWIS data pull---------------------#
                    
                  } else{ #if bounding box is not checked will use state input
                    
                    df <- whatNWISdata(
                      stateCd= state(),
                      siteType="ST",
                      parameterCd=c("00010"), #temp
                      service="dv",#daily values 
                      statCd="00003")%>% #daily values
                      mutate_at(vars(contains('date')), ~ as.Date(., "%Y-%m-%d"))
                  }
                  
                  #DATA UST BE AVAILBLE FOR THE WHOLE RECORD REQUESTED - not just 2 years, range has to be adjusted if less than range is accepatble for analysis 
                  df <- df %>%
                    filter(as.Date(begin_date) < input$date.range[1])%>%#(input$date.range[2] - years(2))) %>% #minimum 2 years
                    #filter(between(as.Date(begin_date), input$date.range[1], input$date.range[2])) %>%
                    filter(end_date > input$date.range[2]) #end date greater than input end date 
                  
                  df
                })
                
                #Create Table - use this to select sites to analyze
                output$site_table <- renderDataTable({
                  NWIS_sites()%>%
                    dplyr::select(2:3,"begin_date", "end_date", 1)
                })
                
                
                #------------------------------------------------------------------------#
                #### Map for exploratory analysis of the data available for each state####
                #========================================================================#
                
                #Create points for plotting
                points_explore <- reactive({
                  df <- NWIS_sites()%>%
                    dplyr::select(dec_lat_va, dec_long_va, site_no)%>%#create simple dataframe with lat and long first #THIS IS LONG AND WHEN DATA IS DOWNLOADED ITS LON (?)
                    rename("lat" = dec_lat_va, "lng" = dec_long_va)
                })
                
                ##Create the output map with the available stream temperature data 
                output$dataavailmap <- renderLeaflet({ 
                  
                  #center orginal map extent on CT
                  lng_int <- c(-72, -74)
                  lat_int <- c(40.5, 42.5)
                  
                  leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
                    addTiles() %>%
                    addCircleMarkers(lng = lng_int, lat = lat_int, fill = FALSE, #radius
                                     stroke = FALSE, fillOpacity = 1) #make a false point so the zoom works when adding points
                  
                })
                
                observeEvent(input$searchsites, {
                  
                  lat_min <- min(points_explore()$lat)
                  lat_max <- max(points_explore()$lat)
                  lng_min <- min(points_explore()$lng)
                  lng_max <- max(points_explore()$lng)
                  
                  leafletProxy("dataavailmap")   %>%
                    clearShapes() %>%
                    #clearBounds() %>%
                    clearMarkers()%>%
                    addCircleMarkers(data = points_explore(), label = points_explore()$site_no) %>%
                    fitBounds(lat1 = lat_min, lat2 = lat_max, lng1 = lng_min, lng2 = lng_max)
                })
                
                ##highlights points based on user selected rows
                observeEvent(input$site_table_rows_selected, {
                  row_selected = points_explore()[input$site_table_rows_selected,]
                  proxy <- leafletProxy("dataavailmap")
                  print(row_selected)
                  proxy %>%
                    addCircleMarkers(#popup=as.character(row_selected$mag),
                      #layerId = as.character(row_selected$id),
                      lng=row_selected$lng, 
                      lat=row_selected$lat,
                      color = "red",
                      label = row_selected$site_no
                      #icon = 
                    )
                })
                
                
                ##change view to resutls panel
                observeEvent(input$gobutton, {
                  updateTabsetPanel(session, "nwis_calc", #id of tabset in ui, 
                                    selected = "Results: Metric Table and Plots")
                })
                
                #++++++++++++++++++++++++++++++++++++++++#
                ######## conduct thermal analysis ########
                #++++++++++++++++++++++++++++++++++++++++#
                
                data <- eventReactive(
                  #rerun when button is pressed
                  input$gobutton,{ #https://stackoverflow.com/questions/46521026/r-shiny-action-button-and-data-table-output
                    
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    progress$set(message = "Calculating Metric Values", value = 0)
                    
                    #Conduct NWIS thermal analysis 
                    #using selected rows from site table
                    
                    s = input$site_table_rows_selected #indices of the selected rows
                    
                    ##creat list of site names to be used in nwis analysis 
                    if(length(s)>1){ #less than one cannot use pull 
                      site_table_s <- NWIS_sites()[s, , drop = TRUE] %>% #create dataframe with only the selected rows
                        pull("site_no")#extract site id column as a vector
                    } else{
                      site_table_s <- NWIS_sites()[s,]$site_no
                    }
                    
                    
                    
                    #print(c(input$siteNo, format(input$date.range[1]), format(input$date.range[2])))
                    #create output dataframe
                    #Analysis 
                    df <- therm_analysis_nwis(site_table_s, #input$siteNo,#for use of dropdown 
                                              format(input$date.range[1]), #start date
                                              format(input$date.range[2]), #end date
                                              TRUE) #bfi TRUE or FALSE
                    df
                    
                  })
                
                # Create output table 
                output$metric_table <-DT::renderDT({
                  datatable(data()) %>% formatStyle(
                    c('AmpRatio', "PhaseLag_d"),
                    backgroundColor = styleInterval(40, c('lightgray', 'red'))) %>% #above 40 indicates dam influenced 
                    formatStyle(
                      c('TS__Slope', "AdjRsqr"),
                      backgroundColor = 'lightblue')
                  
                })
                
                
                
                ##Download Metric Output Table 
                output$downloadData <- downloadHandler(
                  filename = function() {
                    paste("ThermalMetrics_NWIS.csv")
                  },
                  content = function(file) {
                    write.csv(data(), file, row.names = FALSE)
                  }
                )
                
                
                
                ##############
                ###MAP DATA###
                ##############
                # Example of reacting to a button! 
                # points <- eventReactive(input$recalc, {
                #         df <- data()%>%
                #             select(dec_lat_va, dec_lon_va, site_no)%>%#create simple dataframe with lat and long first 
                #             rename("lat" = dec_lat_va, "lng" = dec_lon_va)
                # }, ignoreNULL = FALSE)
                
                #dont want to have to press button to update map points - so just reactive not event reactive
                points <- reactive({
                  df <- data()%>%
                    dplyr::select(dec_lat_va, dec_lon_va, site_no)%>%#create simple dataframe with lat and long first
                    rename("lat" = dec_lat_va, "lng" = dec_lon_va)
                })
                
                
                output$metricmap <- renderLeaflet({
                  leaflet() %>%
                    addTiles() %>%
                    #addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
                    addCircleMarkers(data = points(), label = points()$site_no) 
                  #addLayersControl(baseGroups = c(as.character(providers$Stamen.TonerLite), "Esri.WorldImagery"))
                })
                
                ##attempt at highlighting points selected rows
                observeEvent(input$metric_table_rows_selected, {
                  row_selected = points()[input$metric_table_rows_selected,]
                  proxy <- leafletProxy("metricmap")
                  print(row_selected)
                  proxy %>%
                    addCircleMarkers(#popup=as.character(row_selected$mag),
                      #layerId = as.character(row_selected$id),
                      lng=row_selected$lng, 
                      lat=row_selected$lat,
                      color = "red", label = row_selected$site_no
                      #icon = 
                    )
                })
    }
    )#end module server
  }#end nwisServer

###########################################################################
###########################################################################
###                                                                     ###
###                             User-Defined SERVER                     ###
###                                                                     ###
###########################################################################
###########################################################################
# 
# userDefinedServer <- function(id) {
#   moduleServer(
#     id,
# 
#     ## Below is the module function
#     function(input, output, session) {
#       user_data <-  eventReactive(input$upload_water, {
#         # input$file1 will be NULL initially. After the user selects
#         # and uploads a file, it will be a data frame with 'name',
#         # 'size', 'type', and 'datapath' columns. The 'datapath'
#         # column will contain the local filenames where the data can
#         # be found.
#         inFile <- input$upload_water
#         
#         if (is.null(inFile))
#           return(NULL)
#         
#         read.csv(inFile$datapath, skip = input$colnm_row -1)
#       }
#       )
#       
#       
#       #create initial dataframe for column selection 
#       output$user_datainput <-DT::renderDataTable(
#         user_data(), server = FALSE, selection = list(target = 'column'),
#         caption = 'Original Input Data Table'
#       )
#       
#       #update column name selection based on user column select
#       colnm_input <- c("T_colnm", "ID_colnm", "date_colnm")
#       
#       #update all the colnames choices for user to select correct match
#       observe(lapply(colnm_input, function(x){
#         updateSelectInput(session, x,
#                           #label = paste("Select input label", length(x)),
#                           choices = names(user_data())#,#[,input$user_datainput_columns_selected]),#therefore columns dont need to be selected
#                           #selected = tail(x, 1)
#         )
#       }
#       )#end lapply
#       )#end observe 
#       
#       #Update stream temperature (sTem) dataframe to clean consistent format
#       sTem_df <- eventReactive(input$colselect, {
#         df <- user_data()%>% #[,input$user_datainput_columns_selected] 
#           rename("site_id" = input$ID_colnm , "date_raw" = input$date_colnm, "T_stream"  = input$T_colnm)%>%
#           mutate(date = as.Date(date_raw, format = input$date_format, tryFormats = c("%m/%d/%Y"))) %>%
#           dplyr::select(site_id, date, T_stream, date_raw)%>%
#           group_by(site_id) %>%
#           timetk::summarise_by_time( #for daily time steps from hourly.
#             .date_var = date,
#             .by       = "day", # Setup for monthly aggregation
#             # Summarization
#             tavg_wat_C = mean(T_stream)
#           )
#         #### Making sure input in in Celsius or converts it
#         #using radio buttons
#         sTem_units <- switch(input$temp_unit,
#                              cel = function(x){ #this seems silly, but want it as a option
#                                x * 1 
#                              },
#                              fhr = function(x){
#                                ((x-32) * (5/9))
#                              },
#                              kel = function(x){
#                                x - 273.15 
#                              })
#         
#         #convert the input temperature values to celisus 
#         df$tavg_wat_C <- sTem_units(df$tavg_wat_C)
#         
#         df
#       })
#       
#       # show updated table based on column selected
#       observeEvent(input$colselect, { #stream temperature df = sT_df
#         #output new table with colums
#         output$user_datainput <-DT::renderDataTable(
#           sTem_df(), server = FALSE, selection = list(target = 'column'),
#           caption = 'Updated Input Data Table')
#       })
#       
#       ######################################################
#       #### Recover Geo data for Daymet and Plotting Purposes
#       #read datatable
#       user_loc <-  eventReactive(input$upload_loc, {
#         # input$file1 will be NULL initially. After the user selects
#         # and uploads a file, it will be a data frame with 'name',
#         # 'size', 'type', and 'datapath' columns. The 'datapath'
#         # column will contain the local filenames where the data can
#         # be found.
#         inFile <- input$upload_loc
#         
#         if (is.null(inFile))
#           return(NULL)
#         
#         read.csv(inFile$datapath)
#       }
#       )
#       
#       #create initial dataframe for column selection 
#       output$user_dataloc <- DT::renderDataTable(
#         user_loc(), server = FALSE, selection = list(target = 'column'),
#         caption = 'Original Input Data Table'
#       )
#       
#       #update column name selection based on user column select
#       colnm_locinput <- c("lat_colnm", "IDloc_colnm", "long_colnm")
#       
#       #update all the colnames choices for user to select correct match
#       observe(lapply(colnm_locinput, function(x){
#         updateSelectInput(session, x,
#                           #label = paste("Select input label", length(x)),
#                           choices = names(user_loc())#all column name (so dont need to column select)
#         )
#       }
#       )#end lapply
#       )#end reactive
#       
#       ###Create Reactive clean consistent Datatable for location inputs
#       sLoc_df <- eventReactive(input$locselect, {
#         user_loc() %>%
#           rename("site_id" = input$IDloc_colnm , "lat" = input$lat_colnm, "long" = input$long_colnm)%>%
#           dplyr::select("site_id", "lat", "long") #to get column order correct and drop unused columns
#       })
#       
#       # show updated table based on column selected
#       observeEvent(input$locselect, { #stream temperature df = sT_df
#         #output new table with colums
#         output$user_dataloc <-DT::renderDataTable(
#           sLoc_df(), server = FALSE, selection = list(target = 'column'),
#           caption = 'Updated Input Data Table')
#         
#       }) #end ObserveEvent
#       
#       
#       #####################################################
#       ###       Join data for air combination 
#       ######################################3
#       
#       ### add start and end date for each station id
#       loc_df <- eventReactive(input$locselect, {
#         sTem_df() %>%
#           group_by(site_id)%>%
#           summarise(start_date = min(date),
#                     end_date = max(date))%>%
#           inner_join(sLoc_df(), .)
#       }) #end date_group reactive
#       
#       
#       #output location table
#       output$user_dataavail <-DT::renderDataTable(
#         loc_df(), server = FALSE, selection = list(target = 'column'),
#         caption = 'Joined Input Data Table'
#       )
#       
#       
#       ####Daymet Air collection 
#       aTem_df <- eventReactive(input$daymet_select, { 
#         #run batch collection from daymet
#         aTem <- batch_daymet_u(loc_df())
#         #clean data and pull out avgdaily air temperature 
#         aTem <- clean_daymet(aTem)%>%
#           dplyr::select(site_id, date, tavg_air_C)
#         
#         aTem
#       })
#       
#       ####Join Air and Stream ### 
#       ##dataframe temperature = Tem_df##
#       Tem_df <- reactive({
#         left_join(sTem_df(), aTem_df(), by = c("site_id", "date"))
#       })
#       
#       #output table
#       output$user_dataair <-DT::renderDataTable( #https://rstudio.github.io/DT/server.html for largr data
#         head(Tem_df()), server = FALSE, selection = list(target = 'column'),
#         caption = 'Daymet Raw Data Table'
#       )
#       
#       
#       
#       ##Download Metric Output Table 
#       output$downloadInputdata <- downloadHandler(
#         filename = function() {
#           paste("DataInput_UserData.csv")
#         },
#         content = function(file) {
#           write.csv(Tem_df(), file, row.names = FALSE)
#         }
#       )
#       
#       
#       ### conduct thermal analysis
#       TM_data <- eventReactive(input$calc_metric_u,{
#         #conduct therm analysis 
#         therm_analysis(Tem_df(), loc_df())    
#       })
#       
#       #output location table
#       output$user_dataTM <-DT::renderDataTable(
#         TM_data(), server = FALSE, selection = list(target = 'column'),
#         caption = 'Thermal Metrics DataTable'
#       )
#     }#end user function
#   )#end module server
# }#end userDefinedServer
      
      