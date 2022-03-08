### Server Functions
###########################################################################
###########################################################################
###                                                                     ###
###                             envCan SERVER                             ###
###                                                                     ###
###########################################################################
###########################################################################
envCanServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
    #Define Stations to choose
      stations_df <- reactive({
        getEnvCanStations()
      })
      
    
    #Export Station Bounding Box
      stations_bb <- reactive({
        bounds <- input$dataavailmap_bounds
        #bounds <- as.numeric(bounds)
        bbox_in<- c(round(bounds$west,2), round(bounds$south,2), round(bounds$east,2), round(bounds$north,2))

        stations_df() %>% #WILL ONLY WORK FOR NORTH AND WESTERN HEMPISPHERE( but as this module is canada data is fine)
        filter(between(LATITUDE, bbox_in[2], bbox_in[4])) %>%
        filter(between(LONGITUDE, bbox_in[1], bbox_in[2]))
      })
      
    
    #   
    #   #################################################################
    #   ##                   Chosing Input type nwis                   ##
    #   #################################################################
    #   #-----------------------------------------------#
    #   #       Map Extent vs. State Selection          #
    #   #-----------------------------------------------#
      Tem_df <-  eventReactive(input$getData, {
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
          show_modal_spinner() # show the modal window
          
          #Bounding Box too large [13.3x3.6 degrees]. Your requested width must be less than or equal to  8.4 degrees at latitude 33.6 with requested height of 3.6 degrees.

          #------Use Bounding Box to define NWIS data pull---------------------#


            df <- getEnvCanData(unique(stations_bb()$STATION_NO))


          #------Use station input data pull---------------------#

        } else{ #if bounding box is not checked will use state input

          df <- getEnvCanData(input$station)
        }
        
      
      #get Daymet Data too 
      if(input$mapextent == TRUE){
          loc_df <- stations_df()%>%
            filter(unique(stations_bb()$STATION_NO))}
      else{
            loc_df <- stations_df()%>%
              dplyr::rename(site_id = STATION_NO)%>%
              dplyr::filter(site_id %in% input$station)
      }
        
        loc_df <-loc_df[!duplicated(loc_df$site_id),]
        
        loc_df <- loc_df %>%
          dplyr::select(site_id, LATITUDE, LONGITUDE)%>%
          mutate(start = min(df$date),
                 end = max(df$date))
        
        #run batch collection from daymet
        aTem <- batch_daymet(loc_df)
        #clean data and pull out avgdaily air temperature 
        aTem <- clean_daymet(aTem)%>%
          dplyr::select(site_id, date, tavg_air_C)
      
        #join air and stream temperature data to make data frame
        output <- left_join(df, aTem, by = c("site_id", "date"))%>%
          na.omit() %>%#clean out temperatures that are not paired
          dplyr::filter(tavg_air_C < 120 | tavg_wat_C < 60) #outof range.
        
        output
      })
      
    #
    #   #Create Table - use this to select sites to analyze
      output$site_table <- renderDataTable({
        Tem_df()
      })
      
      ##Download Metric Output Table
      output$download_rawdata <- downloadHandler(
        filename = function() {
          paste("RawData_envCan.csv")
        },
        content = function(file) {
          write.csv(Tem_df(), file, row.names = FALSE)
        }
      )
      
    #   
    #   
    #   #------------------------------------------------------------------------#
    #   #### Map for exploratory analysis of the data available for each state####
    #   #========================================================================#
    #   
      #Create points for plotting
      points_explore <- reactive({
        df <- stations_df()%>%
          dplyr::select(LATITUDE, LONGITUDE, STATION_NO, STATION_NAME)%>%#create simple dataframe with lat and long first #THIS IS LONG AND WHEN DATA IS DOWNLOADED ITS LON (?)
          rename("lat" = LATITUDE, "lng" = LONGITUDE)
      })

      ##Create the output map with the available stream temperature data
      output$dataavailmap <- renderLeaflet({
        lat_min <- min(points_explore()$lat)
        lat_max <- max(points_explore()$lat)
        lng_min <- min(points_explore()$lng)
        lng_max <- max(points_explore()$lng)
        
        leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
          addTiles() %>%
          addCircleMarkers(data = points_explore(), 
                           label = paste(points_explore()$STATION_NO, "//", points_explore()$STATION_NAME)) #make a false point so the zoom works when adding points

      })

      #
      
    # #   
    #   observeEvent(input$searchsites, {
    # 
    #     leafletProxy("dataavailmap")   %>%
    #       clearShapes() %>%
    #       #clearBounds() %>%
    #       clearMarkers()%>%
    #       addCircleMarkers(data = points_explore(), label = c(points_explore()$STATION_NO, points_explore()$STATION_NAME) %>%
    #       fitBounds(lat1 = lat_min, lat2 = lat_max, lng1 = lng_min, lng2 = lng_max))
    #   })
    
    #   
    #   ##highlights points based on user selected rows
    #   observeEvent(input$site_table_rows_selected, {
    #     row_selected = points_explore()[input$site_table_rows_selected,]
    #     proxy <- leafletProxy("dataavailmap")
    #     print(row_selected)
    #     proxy %>%
    #       addCircleMarkers(#popup=as.character(row_selected$mag),
    #         #layerId = as.character(row_selected$id),
    #         lng=row_selected$lng, 
    #         lat=row_selected$lat,
    #         color = "red",
    #         label = row_selected$site_no
    #         #icon = 
    #       )
    #   })
    #   
    #   
    #   # ##change view to resutls panel
    #   # observeEvent(input$gobutton, {
    #   #   updateTabsetPanel(session, "nwis_calc", #id of tabset in ui, 
    #   #                     selected = "Results: Metric Table and Plots")
    #   # })
    #   
      #++++++++++++++++++++++++++++++++++++++++#
      ######## conduct thermal analysis ########
      #++++++++++++++++++++++++++++++++++++++++#

      TM_data <- eventReactive(
        #rerun when button is pressed
        input$gobutton,{
                TMy_output(Tem_df()) #create yearly annual signal analysis 
          })

      # Create output table
      output$metric_table <-DT::renderDT({
        datatable(TM_data()) %>% formatStyle(
          c('AmpRatio', "PhaseLag_d"),
          backgroundColor = styleInterval(40, c('lightgray', 'red'))) %>% #above 40 indicates dam influenced
          formatStyle(
            c('TS__Slope', "AdjRsqr"),
            backgroundColor = 'lightblue')

      })

      ##Download Metric Output Table
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("ThermalMetrics_envCan.csv")
        },
        content = function(file) {
          write.csv(TM_data(), file, row.names = FALSE)
        }
      )
    #
      
      ###########################'
      ###########################'
      ##      PLOTS #############'
      ###########################'
      ###########################'
      
      #######
      #------Plots
      
      p_df <- reactive({
        df_temp_l <- Tem_df()%>% #Tem_df() %>%
          split(f = as.factor(.$site_id))
        # group_by(site_id, .add = TRUE) %>%
        # group_split(.) 
        
        saveRDS(df_temp_l, "df_temp_l_envC.RDS")
        
        sin_wfit_coef <- lapply(names(df_temp_l), function(x){
          y <- fit_TAS(df_temp_l[[x]][,"date"], df_temp_l[[x]][,"tavg_wat_C"])
          z <- mutate(y, site_id = x)#add column with site_id as it is droped in the lapply process
        })%>% 
          do.call("rbind", .)#make dataframe for sin coefficients
        
        saveRDS(sin_wfit_coef, "sin_wfit_coef.RDS")
        saveRDS(Tem_df(), "Tem_df_r.RDS")
        saveRDS(TM_data(),  "TM_data_envC.RDS")
        #data
        p_df <- Tem_df() %>%
          left_join(., sin_wfit_coef, by = "site_id") %>%
          mutate(sin_fit_w = (sinSlope * sin(rad_day(date))) + (cosSlope * cos(rad_day(date))) + YInt)
        
        #print(p_df)
        saveRDS(p_df, "p_df.RDS")
        p_df
      })
      
      output$downloadSinData <- downloadHandler(
        filename = function() {
          paste("DataFit_envCan.csv")
        },
        content = function(file) {
          write.csv(p_df(), file, row.names = FALSE)
        }
      )
      
      output$plot_tempdata <-renderPlotly({
        p <- ggplot(p_df()) +
          geom_line(aes(x = date, y = tavg_air_C), color = "orange")+
          geom_point(aes(x = date, y = tavg_wat_C),color = "lightblue")+
          geom_line(aes(x = date, y = sin_fit_w), color = "blue")+
          #ggtitle("test")+
          xlab("Date")+
          ylab("Water Temperature (C)")+
          theme_bw()+
          facet_grid(rows = vars(site_id)) #rows = vars(site_id))
        
        rows <- length(unique(p_df()$site_id))*200 #~600px before you scroll
        
        ggplotly(p, height = rows)
        
      })
      
      
      output$plot_TS <-renderPlotly({
        p <- p_df() %>%
          dplyr::filter(tavg_wat_C > 1)%>%
          dplyr::filter(tavg_air_C < 120 | tavg_wat_C < 60)%>%
          ggplot(aes(x = tavg_air_C, y = tavg_wat_C, color = factor(year(date)))) +
          geom_point()+
          stat_smooth(method = "lm", col = "red")+
          geom_abline(slope = 1, intercept = 0)+
          annotate(geom = "text", x = 1, y = 5, label = "1:1", color = "black",
                   angle = 1)+
          #ggtitle("test")+
          scale_color_viridis(discrete=TRUE)+
          labs(x = "Air Temperature (C)", y= "Water Temperature (C)", colour="Year")+
          xlim(0, NA)+
          theme_bw()+
          facet_grid(rows = vars(site_id)) #rows = vars(site_id))
        
        rows <- length(unique(p_df()$site_id))*200 #~600px before you scroll
        
        ggplotly(p, height = rows)
        
      })
      
      output$plot_TAS <-renderPlotly({
        p <- ggplot() +
          geom_point(data = TM_data(), aes(x = PhaseLag_d, y = AmpRatio, colour = factor(site_id)))+
          #ggtitle("test")+
          #scale_shape_manual(values=seq(0,15))+
          scale_color_viridis(discrete=TRUE, option = "turbo")+
          labs(x = "Phase Lag (days)", y= "Amplitude Ratio", colour="Mean Ratio")+
          ylim(0,1.2)+
          theme_bw()
        
        ggplotly(p, height = 600)
        
      })
      
      
    #   
    #   
    #   ##############
    #   ###MAP DATA###
    #   ##############
    #   #dont want to have to press button to update map points - so just reactive not event reactive
    #   points <- reactive({
    #     df <- data()%>%
    #       dplyr::select(LATITUDE, LONGITUDE, site_id)%>%#create simple dataframe with lat and long first
    #       rename("lat" = LATITUDE, "lng" = LONGITUDE)
    #   })
    #   
    #   
    #   output$metricmap <- renderLeaflet({
    #     leaflet() %>%
    #       addTiles() %>%
    #       #addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
    #       addCircleMarkers(data = points(), label = points()$site_id) 
    #     #addLayersControl(baseGroups = c(as.character(providers$Stamen.TonerLite), "Esri.WorldImagery"))
    #   })
    #   
    #   ##attempt at highlighting points selected rows
    #   observeEvent(input$metric_table_rows_selected, {
    #     row_selected = points()[input$metric_table_rows_selected,]
    #     proxy <- leafletProxy("metricmap")
    #     print(row_selected)
    #     proxy %>%
    #       addCircleMarkers(#popup=as.character(row_selected$mag),
    #         #layerId = as.character(row_selected$id),
    #         lng=row_selected$lng, 
    #         lat=row_selected$lat,
    #         color = "red", label = row_selected$site_id
    #         #icon = 
    #       )
    #   })
    }
  )#end module server
}#end enCanServer

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
                
                site_tbl <- reactive({
                  tbl <- NWIS_sites()%>%
                    dplyr::select(2:3,"begin_date", "end_date", 1, "count_nu")%>%
                    #make column with data missing table (cant do exact years missing yet)
                    mutate(years_missing = round(as.numeric((difftime(end_date, begin_date, units = c("days"))) - count_nu)/365),1)%>%
                    dplyr::select(-"count_nu")
                  
                })
                
                #Create Table - use this to select sites to analyze
                output$site_table <- renderDataTable({
                    datatable(site_tbl(), options=list(stateSave = TRUE))%>%
                    formatStyle(c('years_missing'),
                                backgroundColor = styleInterval(5, c('lightgray', 'yellow'))) # multi yer data gaps
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
                      label = paste(row_selected$site_no, "/", row_selected$station_nm)
                      #icon = 
                    )
                })
                
                
                ##change view to resutls panel
                observeEvent(input$gobutton, {
                  updateTabsetPanel(session, "nwis_calc", #id of tabset in ui, 
                                    selected = NS(id, 'results_tbl'))
                })
                
                #++++++++++++++++++++++++++++++++++++++++#
                ######## conduct thermal analysis ########
                #++++++++++++++++++++++++++++++++++++++++#
                
                #download nwis temperature data
                download_data <- eventReactive(
                  #rerun when button is pressed
                  input$gobutton,{ #https://stackoverflow.com/questions/46521026/r-shiny-action-button-and-data-table-output
                    
                    progress <- shiny::Progress$new()
                    # Make sure it closes when we exit this reactive, even if there's an error
                    on.exit(progress$close())
                    progress$set(message = "Calculating Metric Values", value = 0)
                    
                    
                    #Conduct NWIS thermal analysis 
                    #using selected rows from site table
                    
                    s = input$site_table_rows_selected #indices of the selected rows
                    
                    ##create list of site names to be used in nwis analysis 
                    if(length(s)>1){ #less than one cannot use pull 
                      site_table_s <- NWIS_sites()[s, , drop = TRUE] %>% #create dataframe with only the selected rows
                        pull("site_no")#extract site id column as a vector
                    } else if (length(s) == 1){
                      site_table_s <- NWIS_sites()[s,]$site_no
                    } else if (!exists(s)){
                      site_table_s <- NWIS_sites()$site_no
                    } else{ #should catch if they dont select anything will do everything
                      site_table_s <- NWIS_sites()$site_no
                    }
                    
                    #get stream temperature data (and location data)
                    nwis_l <- readNWIS_Temp(site_table_s, #input$siteNo,#for use of dropdown 
                                            format(input$date.range[1]), #start date
                                            format(input$date.range[2]))#end date)
                    #list [1] is stream temperature data, and [2] is location information
                    
                    if (!exists("nwis_l")) {
                      output$datafail <- renderText({
                        paste0("Data does not exist for time period requested - often this is due to a >yearly gap in data collection")
                      })
                      
                    }
                    
                    return(nwis_l)
                   
                  })
                
                  #create dataframe with air and stream temperature 
                
                data <- reactive({
                  print(download_data()[2])
                  print("batch aTem")
                    #get air temperature data
                    aTem <- batch_daymet(as.data.frame(download_data()[2])) # all data available from daymet can be assessed here
                    #clean data and pull out avgdaily air temperature 
                  print("batch aTem2")
                    aTem <- clean_daymet(aTem)%>%
                      dplyr::select(site_id, date, tavg_air_C)
                    
                    #join air ad stream temp in a table to have attributed needed for therm_analysis
                    df <- left_join(as.data.frame(download_data()[1]), aTem, by = c("site_id", "date"))%>%
                      na.omit() %>%
                      dplyr::filter(tavg_air_C < 120 | tavg_wat_C < 60)
                
                    print(df)
                    ## for debugging
                    saveRDS(df, "df_nwis_output.RDS")
                    return(df)
                  })
                  
                  #create location dataframe for maps
                loc_df <- reactive({
                    df <- as.data.frame(download_data()[2])
                    return(df)
                  })
                
                # Create output table 
                output$metric_table <-DT::renderDT({ #include site_no, name, lat and long (2,3,5,6)
                  datatable(full_join(NWIS_sites()[input$site_table_rows_selected,c(2,3,5,6)], left_join(therm_analysis(data()), data_gap_check(data()), by = "site_id"), by = c("site_no" = "site_id"))) %>% 
                    formatStyle(c('AmpRatio', "PhaseLag_d", "Ratio_Mean"),
                                backgroundColor = styleInterval(40, c('lightgray', 'red'))) %>% #above 40 indicates dam influenced 
                    formatStyle(c('TS__Slope', "AdjRsqr", "YInt"),
                                backgroundColor = 'lightblue') %>%
                    formatStyle(c("max_conseq_missing_days"),
                                backgroundColor = styleInterval(49, c('white', 'orange'))) 
                  
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
                
                ### conduct yearly thermal analysis
                TM_data_byyear <- eventReactive(input$gobutton,{
                  TMy_output(data())
                  
                })
                
                #output thermal metric table by year (seperate tab)
                output$user_yearlyTM <- DT::renderDataTable({
                  datatable(TM_data_byyear()) %>% 
                    formatStyle(c('AmpRatio', "PhaseLag_d", "Ratio_Mean"),
                                backgroundColor = styleInterval(40, c('lightgray', 'red'))) %>% #above 40 indicates dam influenced 
                    formatStyle(c('TS__Slope', "AdjRsqr", "YInt"),
                                backgroundColor = 'lightblue') %>%
                    formatStyle(c("max_conseq_missing_days"),
                                backgroundColor = styleInterval(49, c('white', 'orange'))) 
                })
                
                ##Download Thermal Metric Output Table 
                output$download_TMyearly <- downloadHandler(
                  filename = function() {
                    paste("ThermalMetricsyearly_NWIS.csv")
                  },
                  content = function(file) {
                    write.csv(TM_data_byyear(), file, row.names = FALSE)
                  }
                )
                
                #creating datatable for plotting
                p_df <- reactive({
                  create_TMplot_df(data())
                })
                
                #output for data tab
                output$plot_tempdata <- renderPlotly({
                  
                  rows <- length(unique(data()$site_id))*300 #~600px before you scroll
                  print(rows)
                  ggplotly(p_dataTS(p_df()), height = rows)%>%
                    plotly::layout(legend=list(x=0, 
                                               xanchor='left',
                                               yanchor='top',
                                               orientation='h'))
                  
                })
                
                #Plot Linear Fit 
                output$plot_TS <- renderPlotly({
                  rows <- length(unique(data()$site_id))*300 
                  
                  ggplotly(plot_TMlm(p_df()), height =rows)%>% #px )%>%
                    plotly::layout(legend=list(x=0, 
                                               xanchor='left',
                                               yanchor='top',
                                               orientation='h'))
                })
                
                #Plot Annual Signal Results
                output$plot_TAS <- renderPlotly({
                  
                  ggplotly(plot_TMas(TM_data_byyear()), height =600)%>% #px )%>%
                  plotly::layout(legend=list(x=0, 
                                             xanchor='left',
                                             yanchor='top',
                                             orientation='h'))
                })
                
                
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
                  df <- loc_df()%>%
                    dplyr::select(dec_lat_va, dec_lon_va, site_no)%>%#create simple dataframe with lat and long first
                    dplyr::rename("lat" = dec_lat_va, "lng" = dec_lon_va)
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
