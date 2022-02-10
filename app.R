##app

source("global.R")


#function for dynamic plotting
#from: https://stackoverflow.com/questions/50914398/increase-plot-size-in-shiny-when-using-ggplot-facets

# Use the module in an application
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  setBackgroundColor(color = "ghostwhite"),
  #use of shinydashboard items without a dashboard - I use for 'box' package
  useShinydashboard(),
  
  navbarPage(selected = "NWIS Stream Data",
             ###APP TITLE###
             "PASTA: Paired Air and Stream Temperature Analysis",
        
        ##-------Info-----------######
        
        tabPanel(
          "Information Page",
          fluidRow(
            column(width = 8, offset = 2,
                   h1("Analysis Infomation and Overview"),
                   
                   p("**********UNDER DEVELOPMENT****************"),
                   p("**********FOR TEST APPLICATION ONLY********"),
                   p("**********USE WITH DISCRETION**************"),
                   
                   # includeText("include.txt"),
                   # hr(),
                   #shiny::includeText("Citation_Text.txt")
                   
                   
            ))),
        
        ##-------NWIS-----------#####
            tabPanel("NWIS Stream Data",
               nwisUI("nwisModule")
             ),
             
        
        ##------envCan----------#####
            tabPanel("Environment Canada Data",
                  envCanUI("envCanModule")
                ),

        ##------User-defined Tab-------###########
             #To create a moduler from these data it would require much updating due to the mulitple "UpdateSelectInput" functionality
             #tabPanel("User Data",
               #userDefinedUI("user defined inputs")
        
        ##################################
        ## User Data Tab 
        ###################################
            tabPanel(
                 "User-defined Data",
                 add_busy_bar(color = "red", height = "8px"),
                 tabsetPanel(id = "user_calc", type = "tabs",
                             
                             
                        tabPanel("Original Data Files",
                                 #adjust styles
                            tags$head(
                              
                                tags$style(HTML("
                                  .shiny-split-layout > div { overflow: visible;}
                                  ")),# always have the full select options visable when using split-layout 
                                
                                  tags$style(HTML("hr {border-top: 1px solid #000000;}")) #make horizontal lines black
                            ),
                            
                            headerPanel("Data Wrangling for User-Data"),
                            
                            fluidRow(
                              
                              
                              #--------------------------------------------------#
                              # Box 1 for Stream Temperature
                              #--------------------------------------------
                              box(title = "Step 1: Stream Temperature Data", width = 4, status = "primary",
                                  ##Content of Box 1
                               strong("Follow these steps to conduct thermal metric analysis.
                                      Required datasets are stream temperature dataaset (multiple sites allowed),
                                      and site location dataset with matching site ids, csvs are required. Uploads 
                                      can take some time please be patient"),
                               hr(),
                                         h4("1a: Upload Stream Temperature Dataset CSV (required columns: site ID, stream temperature, date (or date time)),
                                            upload can take a moment"),
                                         splitLayout(cellWidths = c("25%", "75%"),
                                            numericInput("colnm_row", "Header Row", value = 1),
                                            fileInput("upload_water", "upload raw stream temperature")
                                            ),
                                         
                                         h4("1b: Choose the columns names that correspond to the correct variables:"),
                               
                                            splitLayout(selectInput("ID_colnm", "Site ID Column", choices = NULL),""),
                                         
                                            splitLayout(selectInput("date_colnm", "Date Column", choices = NULL),
                                                        textInput("date_format", "Input date format:", placeholder = "%m/%d/%Y", value = "%m/%d/%Y")
                                                        
                                            ),
                               
                                          a("Date Format Tips", href= "https://www.statmethods.net/input/dates.html"),
                               
                                            splitLayout(selectInput("T_colnm", "Stream Temperature (Daily or SubDaily)", choices = NULL),
                                                        radioButtons("temp_unit", "Temperature Units", choices = c("celsius" = "cel", 
                                                                                                                   "fahrenheit" = "fhr", 
                                                                                                                  "kelvin" = "kel"))
                                            ),
                                         
                                         
                                          
                                         actionButton("colselect", "Tidy Stream Temperature Table"),
                               hr()
                              ),#close box for step 1
                           

                            #--------------------------------------------------#
                            # Box 2 for Stream Location 
                            #--------------------------------------------
                            box(title = "Step 2: Site Location Data ", width = 4, status = "primary",
                                          splitLayout(
                                           ##Location Dataset - can read in same Temperature file if that where it exists? 
                                                h4("2a. Enter Stream Location Data (csv)"),
                                                fileInput("upload_loc", NULL),
                                           ),
                                          p("Can be the same datatable as Stream Temperature data input, location datum must be WGS84 lat/long"),
                               
                                         h4("2b: Choose the columns names that correspond to the correct variables:"),
                                         splitLayout(
                                           selectInput("IDloc_colnm", "Site ID", choices = NULL), #needs to be distinct from ID id above
                                           selectInput("lat_colnm", "Latitude column", choices = NULL),
                                           selectInput("long_colnm", "Longitude column", choices = NULL)
                                           
                                         ),
                                
                                        actionButton("locselect", "Update Location DataTable"),
                                        p("Sites shown in box are data that has both stream temperature data and location data"),
                                        DT::dataTableOutput("user_dataavail"),
                            
                                hr()
                            ),
                            
                            #--------------------------------------------------#
                            # Box 3 for Air and Final Run
                            #--------------------------------------------
                            box(title = "Step 3: Air Temperature Data", width = 4, status = "primary",
                            
                                      
                                      ##Air Temperature Inputs
                                      h4("3: Add Air Temperature Data (user air option coming soon)"),
                                      p("Daymet data available from Jan 1980- Dec 2020"),
                                      actionButton("daymet_select", "Use Daymet Air Temperature Data"),
                                      hr(),
                                      p("Wait for table to appear before proceeding to step 6: running analysis.", style = "color:red"),
                                      hr(),
                                      DT::dataTableOutput("user_dataair"),
                                      
                                      h4("Optional: Output combined stream temperature and air temperature dataframe"),
                                      downloadButton("downloadInputdata", "Download Input DataTable"),
                                      
                                      hr(),
                                      
                                
                                      h4("6: Run the analysis!"),
                                      actionButton("calc_metric_u", "Calculate Thermal Metrics",
                                                   style="padding:20px; font-size: 22px; color: #fff; background-color: #FF0000; border-color: #2e6da4"),
                                      hr()
                            )
                            ),
                               
                            #----------------------------------#
                            #Output Data Tables for visualization#
                            #----------------------------------
                            fluidRow(
                              #in reverse order so they are replaced as data is added to
                              #datatable stream T data 
                              #datatable location data 
                              DT::dataTableOutput("user_dataloc"),
                              DT::dataTableOutput("user_datainput")
  
                            )
                                
                               
                    
                               
                                          #### To add option for user air. 
                                                       # "Air Temperature Dataset (optional)",
                                                       # checkboxInput("air_included", "Calculate using user input Air Temperature"),
                                                       # fileInput("upload_air", NULL)
                                                       
                   

                             
            ), #end messy user input - tabpanel 

                              
        tabPanel("Thermal Metric Results",
                                        #themal metrics dataframe for each year of analysis
                                        DT::dataTableOutput("user_dataTM"),
                                        downloadButton("download_user_dataTM", "Download Thermal Metric DataTable"),
                                        DT::dataTableOutput("user_yearlyTM"),
                                        downloadButton("download_user_yearlyTM", "Download Yearly Thermal Metric DataTable")
                                        
                               ),# end results panel
        
        
        tabPanel("Data Plots",
                 "Annual Temperature Signal Data Fit",
                 #fileInput("upload_water", "Upload Clean Dataframe as csv"),
                 #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                 plotlyOutput("plot_tempdata")
                 #DT::dataTableOutput("user_dataair")
                ),#end plots panel
                 
        tabPanel("Results Plots",
                 "",
                 #fileInput("upload_water", "Upload Clean Dataframe as csv"),
                 #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                 fluidRow(
                   splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot_TAS"),#Temperature ANnual Signals
                                                              plotlyOutput("plot_TS")))
                         #DT::dataTableOutput("user_dataair")
        )#end plots panel
            )#end tabset panel
        ##########################################
        )#End User Tab
    )#NavPage
  )#end fluidpage



########################################################################
########## SERVER ######################################################
########################################################################


server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)#increase max upload size to 30MB
  ####
  ####
  #External Server
  envCanServer("envCanModule")
  nwisServer("nwisModule")
  
  ### User Defined with the app file as lots of update input functions
  #userDefinedServer("user defined inputs")
  #STEP 1 - User stream temperature 
  user_data <-  eventReactive(input$upload_water, {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$upload_water
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, skip = input$colnm_row -1)
  }
  )
  
  #create initial dataframe for column selection 
  output$user_datainput <-DT::renderDataTable(
    user_data(), server = TRUE, selection = list(target = 'column'),
    caption = 'Original Input Data Table'
  )
  
  #update column name selection based on user column select
  colnm_input <- c("T_colnm", "ID_colnm", "date_colnm")
  
  #update all the colnames choices for user to select correct match
  observe(lapply(colnm_input, function(x){
    updateSelectInput(session, x,
                      #label = paste("Select input label", length(x)),
                      choices = names(user_data())#,#[,input$user_datainput_columns_selected]),#therefore columns dont need to be selected
                      #selected = tail(x, 1)
    )
  }
  )#end lapply
  )#end observe 
  
  #Update stream temperature (sTem) dataframe to clean consistent format
  sTem_df <- eventReactive(input$colselect, {
    df <- user_data()%>% #[,input$user_datainput_columns_selected] 
      rename("site_id" = input$ID_colnm , "date_raw" = input$date_colnm, "T_stream"  = input$T_colnm)%>%
      mutate(date = as.Date(date_raw, format = input$date_format),
             site_id = as.factor(site_id)) %>%
      dplyr::select(site_id, date, T_stream, date_raw)%>%
      group_by(site_id) %>%
      timetk::summarise_by_time( #for daily time steps from hourly.
        .date_var = date,
        .by       = "day", # Setup for monthly aggregation
        # Summarization
        tavg_wat_C = mean(T_stream)
      )
    #### Making sure input in in Celsius or converts it
    #using radio buttons
    sTem_units <- switch(input$temp_unit,
                         cel = function(x){ #this seems silly, but want it as a option
                           x * 1 
                         },
                         fhr = function(x){
                           ((x-32) * (5/9))
                         },
                         kel = function(x){
                           x - 273.15 
                         })
    
    #convert the input temperature values to celisus 
    df$tavg_wat_C <- sTem_units(df$tavg_wat_C)
    
    df
  })
  
  # show updated table based on column selected
  observeEvent(input$colselect, { #stream temperature df = sT_df
    #output new table with colums
    output$user_datainput <-DT::renderDataTable(
      sTem_df(), server = TRUE, selection = list(target = 'column'),
      caption = 'Updated Input Data Table')
  })
  
  ######################################################
  #### Recover Geo data for Daymet and Plotting Purposes
  #read datatable
  user_loc <-  eventReactive(input$upload_loc, {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$upload_loc
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath)
  }
  )
  
  #create initial dataframe for column selection 
  output$user_dataloc <- DT::renderDataTable(
    user_loc(), server = TRUE, selection = list(target = 'column'),
    caption = 'Original Input Data Table'
  )
  
  #update column name selection based on user column select
  colnm_locinput <- c("lat_colnm", "IDloc_colnm", "long_colnm")
  
  #update all the colnames choices for user to select correct match
  observe(lapply(colnm_locinput, function(x){
    updateSelectInput(session, x,
                      #label = paste("Select input label", length(x)),
                      choices = names(user_loc())#all column name (so dont need to column select)
    )
  }
  )#end lapply
  )#end reactive
  
  ###Create Reactive clean consistent Datatable for location inputs
  sLoc_df <- eventReactive(input$locselect, {
    user_loc() %>%
      rename("site_id" = input$IDloc_colnm , "lat" = input$lat_colnm, "long" = input$long_colnm)%>%
      dplyr::select("site_id", "lat", "long")%>% #to get column order correct and drop unused columns
      mutate(site_id = as.factor(site_id))%>%#to eal with numeric site ids
      dplyr::group_by(site_id)%>%
      dplyr::summarise_all(funs(mean))%>% # all lat and long should be the same. 
      na.omit()
      })
  
  # show updated table based on column selected
  observeEvent(input$locselect, { #stream temperature df = sT_df
    #output new table with colums
    output$user_dataloc <-DT::renderDataTable(
      sLoc_df(), server = TRUE, selection = list(target = 'column'),
      caption = 'Updated Input Data Table')
    
  }) #end ObserveEvent
  
  
  #####################################################
  ###       Join data for air combination 
  ######################################3
  
  ### add start and end date for each station id
  loc_df <- eventReactive(input$locselect, {
    sTem_df() %>%
      group_by(site_id)%>%
      summarise(start_date = min(date),
                end_date = max(date))%>%
      inner_join(sLoc_df(), .)
  }) #end date_group reactive
  
  
  #output location table
  output$user_dataavail <-DT::renderDataTable(
    loc_df(), server = TRUE, selection = list(target = 'column'),
    caption = 'Joined Input Data Table'
  )
  
  
  ####Daymet Air collection 
  aTem_df <- eventReactive(input$daymet_select, { 
    #run batch collection from daymet
    aTem <- batch_daymet_u(loc_df())
    #clean data and pull out avgdaily air temperature 
    aTem <- clean_daymet(aTem)%>%
      dplyr::select(site_id, date, tavg_air_C)
    
    aTem
  })
  
  ####Join Air and Stream ### 
  ##dataframe temperature = Tem_df##
  
  #
  Tem_df <- reactive({
    left_join(sTem_df(), aTem_df(), by = c("site_id", "date"))%>%
    na.omit()
  })
  
#output table
  output$user_dataair <-DT::renderDataTable( #https://rstudio.github.io/DT/server.html for largr data
    Tem_df(), server = TRUE, selection = list(target = 'column'),
    caption = 'Daymet Raw Data Table'
  )
  
  
  
  ##Download Cleaned Data Output Table wth Air
  output$downloadInputdata <- downloadHandler(
    filename = function() {
      paste("DataInput_UserData.csv")
    },
    content = function(file) {
      write.csv(Tem_df(), file, row.names = FALSE)
    }
  )
  
  ##change view to resutls panel
  observeEvent(input$calc_metric_u, {
    updateTabsetPanel(session, "user_calc", #id of tabset in ui, 
                      selected = "Thermal Metric Results")
  })
  
  ### conduct thermal analysis
  TM_data <- eventReactive(input$calc_metric_u,{
    #conduct therm analysis 
    left_join(therm_analysis(Tem_df()), data_gap_check(Tem_df()), by = "site_id")  
  })
  
  ### conduct yearly thermal analysis
  TM_data_byyear <- eventReactive(input$calc_metric_u,{
    T.y <- add_waterYear(Tem_df())
    T.yl <- lapply(levels(T.y$year_water), function(x){
            df.y <- T.y %>%
            filter(year_water == x)#%>%
    
            df.j <- left_join(therm_analysis(df.y), data_gap_check(df.y), by = "site_id")
      
    
            df.j$year <- x # add water year as a valuBe in table
      
            df.j#return dataframe
            })
    #convert list to dataframe
    df <- do.call(rbind.data.frame, T.yl)#return single dataframen
    
  })
  
  #output thermal metric table
  output$user_dataTM <-DT::renderDataTable({
    datatable(TM_data()) %>% 
      formatStyle(c('AmpRatio', "PhaseLag_d", "Ratio_Mean"),
        backgroundColor = styleInterval(40, c('lightgray', 'red'))) %>% #above 40 indicates dam influenced 
      formatStyle(c('TS__Slope', "AdjRsqr", "YInt"),
        backgroundColor = 'lightblue') %>%
    formatStyle(c("max_conseq_missing_days"),
                backgroundColor = styleInterval(49, c('white', 'orange')))# greater than 49 indicates poor data (large data gaps) 
  })
  
  ##Download Thermal Metric Output Table 
  output$download_user_dataTM <- downloadHandler(
    filename = function() {
      paste("DataOutput_UserData.csv")
    },
    content = function(file) {
      write.csv(TM_data(), file, row.names = FALSE)
    }
  )
  
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
  
  
  ##Download Yearly Metric Output Table 
  output$download_user_yearlyTM <- downloadHandler(
    filename = function() {
      paste("DataOutput_UserData_yearly.csv")
    },
    content = function(file) {
      write.csv(TM_data_byyear(), file, row.names = FALSE)
    }
  )
  #######
  #------Plots
  
p_df <- reactive({
    df_temp_l <- Tem_df()%>% #Tem_df() %>%
      split(f = as.factor(.$site_id))
      # group_by(site_id, .add = TRUE) %>%
      # group_split(.) 
    
    saveRDS(df_temp_l, "df_temp_l.RDS")
    
    sin_wfit_coef <- lapply(names(df_temp_l), function(x){
      y <- fit_TAS(df_temp_l[[x]][,"date"], df_temp_l[[x]][,"tavg_wat_C"])
      z <- mutate(y, site_id = x)#add column with site_id as it is droped in the lapply process
      })%>% 
      do.call("rbind", .)#make dataframe for sin coefficients
       
    saveRDS(sin_wfit_coef, "sin_wfit_coef.RDS")
    saveRDS(Tem_df(), "Tem_df_r.RDS")
    saveRDS(TM_data(),  "TM_data.RDS")
    saveRDS(TM_data_byyear(),  "TM_data_byyear.RDS")
    #data
      p_df <- Tem_df() %>%
        left_join(., sin_wfit_coef, by = "site_id") %>%
        mutate(sin_fit_w = (sinSlope * sin(rad_day(date))) + (cosSlope * cos(rad_day(date))) + YInt)
    
    #print(p_df)
    saveRDS(p_df, "p_df.RDS")
    p_df
})
    
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
      filter(tavg_wat_C > 1)%>%
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
      geom_point(data = TM_data_byyear(), aes(x = PhaseLag_d, y = AmpRatio, colour = factor(site_id)))+
      #ggtitle("test")+
      #scale_shape_manual(values=seq(0,15))+
      scale_color_viridis(discrete=TRUE, option = "turbo")+
      labs(x = "Phase Lag (days)", y= "Amplitude Ratio", colour="Mean Ratio")+
      ylim(0,1.2)+
      theme_bw()
    
    ggplotly(p, height = 600)
    
  })


}

shinyApp(ui, server)