##app

# Use the module in an application
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(selected = "NWIS Stream Data",
             ###APP TITLE###
             "Automated Paired Stream and Air Temperature Analysis",
             tabPanel("NWIS Stream Data",
               nwisUI("nwisModule")
             ),
             
             #To create a moduler from these data it would require much updating due to the mulitple "UpdateSelectInput" functionality
             #tabPanel("User Data",
               #userDefinedUI("user defined inputs")
               tabPanel(
                 "User-defined Data",
                 sidebarPanel(
                   
                   p("Under Development"),
                   
                   "Stream Temperature Dataset (required)",
                   numericInput("colnm_row", "Row Number of Column Names", value = 1),
                   fileInput("upload_water", "C:/Users/hared/Dropbox/UConn/Projects/300_Network_GW_Temp/200_Data_Input/SWT/LTER/HarvardForest/Big_Low.csv"),
                   
                   selectInput("date_colnm", "User Date Column Name", choices = NULL),
                   textInput("date_format", "Correct Date Format", placeholder = "%m/%d/%Y"),
                   a("Date Format Tips     ", "https://www.statmethods.net/input/dates.html"),#
                   selectInput("ID_colnm", "User ID Column Name", choices = NULL),
                   selectInput("T_colnm", "User Stream Temperature Column Name", choices = NULL),
                   radioButtons("temp_unit", "Temperature Units", choices = c("celsius" = "cel", 
                                                                                  "fahrenheit" = "fhr", 
                                                                                  "kelvin" = "kel"), 
                                selected =  "celsius"),
                   
                   actionButton("colselect", "Update Stream Temperature DataTable"),
                   
                   ##Location Dataset - can read in same Temperature file if that where it exists? 
                   "Stream Location Data (required for Daymet data and plotting functionality)",
                   "Can be the same as Stream Temperature data input",
                   fileInput("upload_loc", NULL),
                   "select lat and long columns NOTE*(have to select columns body not column header)",
                   selectInput("IDloc_colnm", "User ID Column Name", choices = NULL), #needs to be distinct from ID id above
                   selectInput("lat_colnm", "User latitude column name", choices = NULL),
                   selectInput("long_colnm", "User longitude column name", choices = NULL),
                   actionButton("locselect", "Update Location DataTable"),
                   
                   
                   ##Air Temperature Inputs
                   checkboxInput("air_included", "Calculate using user input Air Temperature"),
                   actionButton("daymet_select", "Use Daymet Air Temperature Data"),
                   downloadButton("downloadInputdata", "Download Input DataTable"),
                   actionButton("calc_metric_u", "Calculate Thermal Metrics"),
                   br(),
                   "Air Temperature Dataset (optional)",
                   fileInput("upload_air", NULL)
                   
                 ),
                 ##main panel with tabs for different output 
                 mainPanel(
                   h2("User Input Data"),
                   #themal metrics dataframe
                   DT::dataTableOutput("user_dataTM"),
                   #Datatable with air 
                   DT::dataTableOutput("user_dataair"),
                   #datatable stream T data 
                   DT::dataTableOutput("user_dataavail"),
                   #datatable location data 
                   DT::dataTableOutput("user_dataloc"),
                   #leafletOutput("user_dataavailmap"),
                   DT::dataTableOutput("user_datainput"),
                   
                   p(),
                   
                 ),
                 
               ) #end userdefined UI
)
)

########################################################################
########## SERVER ######################################################
########################################################################


server <- function(input, output, session) {
  
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
      mutate(date = as.Date(date_raw, format = input$date_format, tryFormats = c("%m/%d/%Y"))) %>%
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
      dplyr::select("site_id", "lat", "long") #to get column order correct and drop unused columns
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
  Tem_df <- reactive({
    left_join(sTem_df(), aTem_df(), by = c("site_id", "date"))
  })
  
  #output table
  output$user_dataair <-DT::renderDataTable( #https://rstudio.github.io/DT/server.html for largr data
    head(Tem_df()), server = FALSE, selection = list(target = 'column'),
    caption = 'Daymet Raw Data Table'
  )
  
  
  
  ##Download Metric Output Table 
  output$downloadInputdata <- downloadHandler(
    filename = function() {
      paste("DataInput_UserData.csv")
    },
    content = function(file) {
      write.csv(Tem_df(), file, row.names = FALSE)
    }
  )
  
  
  ### conduct thermal analysis
  TM_data <- eventReactive(input$calc_metric_u,{
    #conduct therm analysis 
    therm_analysis(Tem_df(), loc_df())    
  })
  
  #output location table
  output$user_dataTM <-DT::renderDataTable(
    TM_data(), server = FALSE, selection = list(target = 'column'),
    caption = 'Thermal Metrics DataTable'
  )
  
}


shinyApp(ui, server)