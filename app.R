##app
source("global.R")
# Use the module in an application
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(selected = "NWIS Stream Data",
             ###APP TITLE###
             "PASTA: Paired Stream and Air Temperature Analysis",
             
        ##-------NWIS-----------#####
            tabPanel("NWIS Stream Data",
               nwisUI("nwisModule")
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
              
                 tabsetPanel(id = "user_calc", type = "tabs",
                             
                             
                        tabPanel("Original Data Files",
                            tags$head(
                            tags$style(HTML("
                              .shiny-split-layout > div {
                                overflow: visible;
                              }
                              "))),# always have the full select options visable when using split-layout 
                            tags$head(
                              tags$style(HTML("hr {border-top: 1px solid #000000;}")) #make horizontal lines black
                            ),
                            
                            headerPanel("This tab is to be used for setting up stream temperature data for analysis"),
                            
                            column(width = 8,
                               strong("Follow these steps to conduct thermal metric analysis.
                                      Required datasets are stream temperature dataaset (multiple sites allowed),
                                      and site location dataset with matching site ids"),
                               hr(),
                                         h4("1a: Upload Stream Temperature Dataset CSV (required columns: site ID, stream temperature, date (or date time)),
                                            upload can take a moment"),
                                         splitLayout(
                                            numericInput("colnm_row", "Column Header Row Value", value = 1),
                                            fileInput("upload_water", "upload raw stream temperature")
                                            ),
                                         
                                         h4("1b: Choose the columns names that correspond to the correct variables:"),
                               
                                            splitLayout(selectInput("ID_colnm", "Site ID Column", choices = NULL),""),
                                         
                                            splitLayout(selectInput("date_colnm", "Date Column", choices = NULL),
                                                        textInput("date_format", "Input date format:", placeholder = "%m/%d/%Y"),
                                                        a("Date Format Tips", href= "https://www.statmethods.net/input/dates.html")
                                            ),
                                         
                                            splitLayout(selectInput("T_colnm", "Stream Temperature (Daily or SubDaily)", choices = NULL),
                                                        radioButtons("temp_unit", "Temperature Units", choices = c("celsius" = "cel", 
                                                                                                                   "fahrenheit" = "fhr", 
                                                                                                                  "kelvin" = "kel"))
                                            ),
                                         
                                         
                                          
                                         actionButton("colselect", "Tidy Stream Temperature Table"),
                               
                               hr(),

                               DT::dataTableOutput("user_datainput"),
                               
                               hr(),
                               
                                          splitLayout(
                                           ##Location Dataset - can read in same Temperature file if that where it exists? 
                                                h4("2a. Enter Stream Location Data"),
                                                fileInput("upload_loc", NULL),
                                           ),
                                          p("Can be the same datatable as Stream Temperature data input, location datum must be WGS84 lat/long"),
                               
                                         h4("2b: Choose the columns names that correspond to the correct variables:"),
                                         splitLayout(
                                           selectInput("IDloc_colnm", "Site ID", choices = NULL), #needs to be distinct from ID id above
                                           selectInput("lat_colnm", "Latitude column", choices = NULL),
                                           selectInput("long_colnm", "Longitude column", choices = NULL),
                                           actionButton("locselect", "Update Location DataTable")
                                         ),
                                hr(),
                               
                               #datatable stream T data 
                               DT::dataTableOutput("user_dataavail"),
                               #datatable location data 
                               DT::dataTableOutput("user_dataloc"),
                                
                               hr(),
                                           ##Air Temperature Inputs
                                         h4("3: Add Air Temperature Data (currently only daymet data option)"),
                                         actionButton("daymet_select", "Use Daymet Air Temperature Data"),
                               
                               hr(),
                               DT::dataTableOutput("user_dataair"),
            
                                         h4("Optional: Output combined stream temperature and air temperature dataframe"),
                                           downloadButton("downloadInputdata", "Download Input DataTable"),
                                          
                                        hr(),
                                          
                                         h4("6: Calculate Thermal Metrics"),
                                           actionButton("calc_metric_u", "Calculate Thermal Metrics",
                                                        style="color: #fff; background-color: #FF0000; border-color: #2e6da4"),
                                           br()
                    
                               
                                          #### To add option for user air. 
                                                       # "Air Temperature Dataset (optional)",
                                                       # checkboxInput("air_included", "Calculate using user input Air Temperature"),
                                                       # fileInput("upload_air", NULL)
                                                       
                                  )#end column
   

                             
            ), #end messy user input - tabpanel 
                              
        tabPanel("Cleaned Data Input Files",
                 fileInput("upload_water", "Upload Clean Dataframe as csv"),
                 checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                 #DT::dataTableOutput("user_dataair")
        ),
        
        tabPanel("Thermal Metric Results",
                                        #themal metrics dataframe for each year of analysis
                                        DT::dataTableOutput("user_dataTM"),
                                        downloadButton("download_user_dataTM", "Download Thermal Metric DataTable"),
                                        DT::dataTableOutput("user_yearlyTM"),
                                        downloadButton("download_user_yearlyTM", "Download Yearly Thermal Metric DataTable")
                                        
                               )
                 

            )#end user defined tab panel
        ##########################################
        
        
            )
        )#NavPage
  )#end fluidpage



########################################################################
########## SERVER ######################################################
########################################################################


server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)#increase max upload size to 30MB
  
  nwisServer("nwisModule")
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
    user_data(), server = FALSE, selection = list(target = 'column'),
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
      mutate(date = as.Date(date_raw, format = input$date_format, tryFormats = c("%m/%d/%Y")),
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
      sTem_df(), server = FALSE, selection = list(target = 'column'),
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
    user_loc(), server = FALSE, selection = list(target = 'column'),
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
      sLoc_df(), server = FALSE, selection = list(target = 'column'),
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
    loc_df(), server = FALSE, selection = list(target = 'column'),
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
    left_join(sTem_df(), aTem_df(), by = c("site_id", "date"))
  })
  
#output table
  output$user_dataair <-DT::renderDataTable( #https://rstudio.github.io/DT/server.html for largr data
    Tem_df(), server = FALSE, selection = list(target = 'column'),
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
    therm_analysis(Tem_df())#, loc_df())    
  })
  
  ### conduct yearly thermal analysis
  TM_data_byyear <- eventReactive(input$calc_metric_u,{
    T.y <- add_waterYear(Tem_df())
    T.yl <- lapply(levels(T.y$year_water), function(x){
      
      df.y <- T.y %>%
        filter(year_water == x)%>%
        (therm_analysis(.))#, data_gap_check(.), by = "site_id")
      
      df.y$year <- x # add water year as a value in table
      
      df.y#return dataframe
    })
    
    #names(T.yl) <- levels(T.y$year_water)
    #conduct therm analysis 
    df <- do.call(rbind.data.frame, T.yl)#return single dataframe
    
  })
  
  #output lthermal metric table
  output$user_dataTM <-DT::renderDataTable(
    TM_data(), server = FALSE, selection = list(target = 'column'),
    caption = 'Thermal Metrics DataTable'
  )
  
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
  output$user_yearlyTM <- DT::renderDataTable(
    TM_data_byyear(), server = FALSE, selection = list(target = 'column')
  )
  
  
  ##Download Yearly Metric Output Table 
  output$download_user_yearlyTM <- downloadHandler(
    filename = function() {
      paste("DataOutput_UserData_yearly.csv")
    },
    content = function(file) {
      write.csv(TM_data_byyear(), file, row.names = FALSE)
    }
  )
  
  
}


shinyApp(ui, server)