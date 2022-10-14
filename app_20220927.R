##app

source("global.R")
user_base <- tibble::tibble(
  user = c("user1", "user2"),
  password = sapply(c("pass1", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

#function for dynamic plotting
#from: https://stackoverflow.com/questions/50914398/increase-plot-size-in-shiny-when-using-ggplot-facets

# Use the module in an application
ui <- fluidPage(

  #theme = shinytheme("yeti"),
  #setBackgroundColor(color = "ghostwhite"),
  #use of shinydashboard items without a dashboard - I use for 'box' package
  useShinydashboard(),
  
  navbarPage(#selected = "NWIS Stream Data",
             ###APP TITLE###
             "PASTA: Paired Air and Stream Temperature Analysis",
             theme = "paper.css",
             #footer = includeHTML("./www/include_footer.html"),
             
        
        ##-------Info-----------######
        
        tabPanel(
          "Information Page",
          

          fluidRow(
            HTML("
                                     
                                     <section class='banner'>
                                     <h2 class='parallax'>PASTA</h2>
                                     <p class='parallax_description'>A tool for the calculation of 
                                     paired air & water temperature metrics</p>
                                     </section>
                                     ")
          ),
          
          fluidRow(
            column(3),
            column(6,
                   shiny::HTML("<br><br><center> <h1>Analysis Infomation and Overview</h1> </center><br>"),
                   shiny::HTML("<h4>Utlize the tabs on the top to select the source of stream temperature data: current options are NWIS, 
                                select Environmental Canada Data, and user uploads from direct upload or HydroShare resources.
                               The analyses conducted use the correlation between air and stream temperature 
                               to support interferences about hydrologic processes. Two analyses are performed: 
                               (1) Annual Signal Analysis, where the daily stream and air mean temperatureare fit to a sinusoid, and the 
                               compartive metrics are extracted - Amplitude Ratio, Phase lag (days), and Mean Ratio (e.g. Johnson et al. 2020, Hare et al. 2021)
                               (2) Daily (or weekly) mean temperature are compared through a linear regression (Kelleher et al. 2012, Letcher et al. 2016)
                               <br><br>CURRENTLY UNDER DEVELOPMENT AND FOR TEST APPLICATION ONLY </h4>")
            ),
            column(3)
          ),
          fluidRow(
            
            style = "height:50px;"),
          
          # PAGE BREAK
          tags$hr(),
          
          # WHERE
          fluidRow(
            column(1),
            column(10,
                   shiny::HTML("<br><br><center> <h1>Citations</h1> </center><br>"),
                   shiny::HTML("<h4> Hare, DK, Helton AM, Johnson ZC, Lane JW, and Briggs MA (2021) Continental-scale analysis of shallow and deep groundwater contributions to streams. Nature Communications 12, 1450. https://doi.org/10.1038/s41467-021-21651-0
                   <br>
                   <br>
                                      Johnson, ZC, BG Johnson, MA Briggs, WD Devine, CD Snyder, NP Hitt, DK Hare, TV Minkova (2020). Paired air-water annual temperature Patterns reveal hydrogeological controls on stream thermal regimes at watershed to continental scales. Journal of Hydrology. https://doi.org/10.1016/J.JHYDROL.2020.124929
                                      <br>
                                      <br>
                                     Kelleher, C., Wagener, T., Gooseff, M., McGlynn, B., McGuire, K., Marshall, L., 2012. Investigating controls on the thermal sensitivity of Pennsylvania streams. Hydrol. Process. 26, 771 https://doi.org/10.1002/hyp.8186
                                     <br>
                                     <br>
                                     Letcher, B.H., Hocking, D.J., Neil, K.O., Whiteley, A.R., Nislow, K.H., Donnell, M.J.O., 2016. A hierarchical model of daily stream temperature using air-water temperature synchronization , autocorrelation , and time lags. PeerJ https://doi.org/10.7717/Peerj.1727
                                     <br>
                                     <br>
                                     Thornton, M.M., R. Shrestha, Y. Wei, P.E. Thornton, S. Kao, and B.E. Wilson. 2020. Daymet: Daily Surface Weather Data on a 1-km Grid for North America, Version 4. ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/1840
                               .</h4>")
            ),
            column(1)
          ),
          
          # PAGE BREAK
          tags$hr(),
          
          fluidRow(
            
            style = "height:50px;"),
          
          fluidRow(
            column(1),
            column(10,
                   shiny::HTML("<br><br><center> <h1></h1> </center><br>"),
                   shiny::HTML("<h5>Developed with Funding provided by CUAHSI's Hydroinformatics Innovation Fellowship<br>
		Created by: Danielle Hare<br>
		Contact Info: danielle.hare@uconn.edu<br>.</h5>
        
                    <a href=https://www.cuahsi.org/>
                        <img src = logo_cuahsi.png width = 400px height = auto> 
                     .</a>
     			<a href=https://www.hydroshare.org/>
      				<img src = logo-hs.png width = 400px height = auto>.</a>

            <p>Disclaimer: The authors assume no responsibility or liability for any errors or omissions in the content of this site. We recommend consulting with subject-matter experts for the interpretation of output data in regard to a user's particular data and context. 
                <br>2022 PASTA: Paired Air and Stream Temperature Analysis
                               </p>")
            ),
            column(1)
          ),
          fluidRow(
            
            style = "height:50px;"),
          
          # PAGE BREAK
          tags$hr(),

            ),#end info tab panel
        
        ##-------NWIS-----------#####
            tabPanel("NWIS Stream Data",
                nwisUI("nwisModule")
             ),
             
        
        ##------envCan----------#####
            tabPanel("Environment Canada Data",
                  envCanUI("envCanModule")
                ),
        
        ##------NorWeST---------#####
        tabPanel("NorWeST Stream Data",
                 norwestUI("norwestModule")
        ),

        ##------User-defined Tab-------###########
             #To create a moduler from these data it would require much updating due to the mulitple "UpdateSelectInput" functionality
             #tabPanel("User Data",
               #userDefinedUI("user defined inputs")
        
        ##################################
        ## User Data Tab 
        ###################################
            tabPanel(
                 "Upload Data: HydroShare or Table",
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
                              strong("Follow these steps in order to conduct thermal metric analysis.
                                      Required datasets are stream temperature dataaset (multiple sites allowed),
                                      and site location dataset with matching site ids, csvs are required.  
                                      Stream datasets require columns: site ID, stream temperature, date (or date time, but only put date attributed in date input format).
                                      Uploads can take some time please be patient."),
                              hr(),
                              
                              #div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                              useShinyjs(),
                              #--------------------------------------------------#
                              # Box 1 for Stream Temperature
                              #--------------------------------------------
                              column(width = 4,
                              tabBox(title = "Step 1: Upload Stream Temperature Data",
                                     id = "upload_tab",
                                     #status = "primary",
                                  ##Content of Box 1
                               tabPanel("Table",
                                         splitLayout(cellWidths = c("50%", "50%"),
                                            numericInput("colnm_row", "Header Row", value = 1),
                                            selectInput(inputId = 'upload_deml', label = 'delimiter', 
                                                         choices = c(Comma=',' ,Semicolon=';'
                                                                     ,Tab='\t', Space= " "), selected = ',')),
                                        
                                        fileInput("upload_water", "Upload raw stream temperature")
                               ),
                                            #textInput("upload_deml", "delimiter", placeholder = ",", value = ","),
                                            
                              
                                tabPanel("HydroShare",
                               #title = "Upload from HydroShare", collapsible = TRUE, collapsed = TRUE,width = 4,
                               
                                            textInput("resource_id", "CUAHSI Resource ID"),
                                            #shinyauthr::loginUI(id = "login"),
                                            textInput("user_id", "HydroShare user ID"),
                                            passwordInput("user_pw", "HydroShare password"),
                                            actionButton("select_water_HS", "Explore HydroShare Data"),
                                            #checkboxInput("activate_HS", "Use HydroShare Data"),
                                            actionButton("upload_water_HS", "Data File Selected"),
                               
                                            DT::dataTableOutput("filetable_HS")

                                          ),
                              ),
                              box(title ="1b: Choose the columns names that correspond to the correct variables:", status = "primary",

                                            splitLayout(selectInput("ID_colnm", "Site ID Column", choices = NULL),""),

                                            splitLayout(selectInput("date_colnm", "Date Column", choices = NULL),
                                                        textInput("date_format", "Input date format:", placeholder = "%m/%d/%Y", value = "%m/%d/%Y")

                                            ),

                                          a("Date Format Tips", href="https://www.statmethods.net/input/dates.html"),

                                            splitLayout(selectInput("T_colnm", "Stream Temperature", choices = NULL),
                                                        radioButtons("temp_unit", "Temperature Units", choices = c("celsius" = "cel",
                                                                                                                   "fahrenheit" = "fhr",
                                                                                                                  "kelvin" = "kel"))
                                            ),

                                         actionButton("colselect", "Tidy Stream Temperature Table"),
                               hr(),
                               
                               h5("Choose Air Input Data: Daymet or User-defined"),
                               radioButtons("air_choice", "Air Temperature Source", choices = c("Daymet download" = "daymet",
                                                                                                "user upload" = "user_upload")), 
                              ),#close box for step 1
                              ),#close column 1

                            #--------------------------------------------------#
                            # Box 2 for Stream Location 
                            #--------------------------------------------#
                            box(title = "Step 2: Site Location Data - Required for Daymet", id = "upload_loctab", width = 4,
                                 ##Location Dataset
                                           splitLayout(cellWidths = c("25%", "25%", "50%"),
                                                       numericInput("colnm_row_loc", "Header Row", value = 1),
                                                       selectInput(inputId = 'upload_deml_loc', label = 'delimiter', 
                                                                   choices = c(Comma=',' ,Semicolon=';'
                                                                               ,Tab='\t', Space= " "), selected = ','),
                                                       fileInput("upload_loc", "2a. Enter Stream Location Data (csv)")),
                                         
                                        p("Can be the same datatable as Stream Temperature data input, location datum must be WGS84 lat/long"),
                                        hr(),

                                        strong("2b: Choose the columns names that correspond to the correct variables:"),
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
                            
                                  box(
                                      ##Air Temperature Inputs
                                      h5("Add Air Temperature Data"),
                                      # radioButtons("air_choice", "Air Temperature Source", choices = c("Daymet download" = "daymet",
                                      #                                                                  "user upload" = "user_upload")),                                                                                   
                                      p("Daymet data available from Jan 1980- Dec 2021")
                                  ),
                                
                                box(title = "User Air", id = "upload_userairtab",
                                    
                                    splitLayout(cellWidths = c("25%", "25%", "50%"),
                                                numericInput("colnm_row_air", "Header Row", value = 1),
                                                selectInput(inputId = 'upload_deml_air', label = 'delimiter', 
                                                            choices = c(Comma=',' ,Semicolon=';'
                                                                        ,Tab='\t', Space= " "), selected = ','),
                                                fileInput("upload_air", "2a. Enter Stream Location Data (csv)")),
                                    
                                    
                                    splitLayout(selectInput("ID_colnm_air", "Site ID Column", choices = NULL),""),
                                    
                                    splitLayout(selectInput("date_colnm_air", "Date Column", choices = NULL),
                                                textInput("date_format_air", "Input date format:", placeholder = "%m/%d/%Y", value = "%m/%d/%Y")
                                                
                                    ),
                                    
                                    a("Date Format Tips", href="https://www.statmethods.net/input/dates.html"),
                                    
                                    splitLayout(selectInput("T_colnm_air", "Stream Temperature", choices = NULL),
                                                radioButtons("temp_unit_air", "Temperature Units", choices = c("celsius" = "cel",
                                                                                                           "fahrenheit" = "fhr",
                                                                                                           "kelvin" = "kel"))
                                    ),
                                    
                                    actionButton("colselect_air", "Tidy User Air Data"),
                                    hr()
                                ),#close box for step 1
                              
                                      actionButton("add_air", "Join Air Temperature"),
                                      hr(),
                                      p("Wait for table to appear before proceeding to step 4: running analysis.", style = "color:red"),
                                      hr(),
                                      DT::dataTableOutput("user_dataair"),
                                      
                                      h4("Optional: Output combined stream temperature and air temperature dataframe"),
                                      downloadButton("downloadInputdata", "Download Input DataTable"),
                                      
                                      hr(),
                                      
                                
                                      h4("4: Run the analysis!"),
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
                                                       # "Air Temperature Dataset (optional)",
                                                       # checkboxInput("air_included", "Calculate using user input Air Temperature"),
                                                       # fileInput("upload_air", NULL)
                                                       
                   

                             
            ), #end messy user input - tabpanel 

                              
        tabPanel("Thermal Metric Results",
                 h4("Metric Data Table"),
                 h5("Grey Columns are assoicated with paired air and stream annual signals calculations"),
                 p("Amp_Ratio is Amplitude Ratio, unitless"),
                 p("PhaseLag_d is Phase Lag, days"),
                 p("Mean_ratio is ratio of average water temperature divided average air temperature"),
                 h5("Blue columns are associated with air and stream temperature linear regression"),
                 p("TS__Slope, is the slope of the linear relationship between air and water temperature"),
                 p("AdjRsqr, is the r2 of the linear fit"),
                 p("YInt, is the y intercept of the linear relationship"),
                 h5("Please review the literature citations from the information tab to explore how to interpret these data"),
                 
                 
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
  norwestServer("norwestModule")
  
  # ### User Defined with the app file as lots of update input functions
  # #userDefinedServer("user defined inputs")
  # observe({
  #   toggle(id = "login", condition = input$upload_water_HS)
  #   toggle(credentials(), condition = input$upload_water_HS)
  # })
  # 
  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = TRUE,
  #   log_out = reactive(logout_init())
  # )
  
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth)
  # )
  #STEP 1 - User stream temperature 
  ###NOTE USER DATA IS USED TWICE - only one should have data. but also should be able to over write due to the "action" required. 

  ##############
  #retrieve hydroshare data
  inFile_ls <-eventReactive(input$select_water_HS,{ 
          
    filelist_retrieval(resource_id,input$user_id, input$user_pw)
                                    #credentials()$info$user, credentials()$info$password)
    }, ignoreInit = TRUE)
  
  observeEvent(input$select_water_HS,{ 
    output$filetable_HS <- DT::renderDataTable(
    inFile_ls(), server = TRUE, selection = 'single', #only one right now to not have to deal with potenital different structures
    caption = 'Files Available from specified HydroShare resource id')}
    , ignoreInit = TRUE)
  
# if(input$upload_tab)
  # v <- reactiveValues(data2=NA)
  # 
  # output$doesitexist <- renderText({
  #   if (is.na(v$data2)) "No, it doesn't exist" else "Yes, it does exist"
  # })
  # 
  # observeEvent(input$simulate, { v$data2 <- data.frame(x=1)}
  #)
# if(isolate(input$activate_HS)){
  #userdata if hydroshare
  
  
  user_data <-  eventReactive(c(
    input$upload_water_HS,
    input$upload_water),
    {
    #req(is.null(df()))
    if (input$upload_tab == 'HydroShare'){ 
 
        row_val = input$filetable_HS_rows_selected
        print(row_val)
        files <- inFile_ls()[row_val,]

    #retrieve selected file
    file_retrieval <- lapply(files$url, function(file_url) #only one table allowed right now
    {
      file_retrieve_response = GET(file_url, authenticate(username, password, type = "basic"))
      file_ls <- content(file_retrieve_response)
    })

    file_df<- as.data.frame(file_retrieval)}
    
    else{
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.

      inFile <- input$upload_water

      if (is.null(inFile))
        return(NULL)

      file_df <-read.csv(inFile$datapath, skip = input$colnm_row -1, sep = input$upload_deml)
    }
    }
    )

##################################################  
  #create initial dataframe for column selection 
  output$user_datainput <-DT::renderDataTable(
    user_data(), server = TRUE, selection = list(target = 'column'),
    caption = 'Original Input Data Table'
  )
  
  #update column name selection based on user column select
  colnm_input <- c("T_colnm", "ID_colnm", "date_colnm")
  
  #update all the colnames choices for user to select correct match
  observe(
    lapply(colnm_input, function(x){
    updateSelectInput(session, x,
                      #label = paste("Select input label", length(x)),
                      choices = names(user_data())#,#[,input$user_datainput_columns_selected]),#therefore columns dont need to be selected
                      #selected = tail(x, 1)
    )
      }
  )#end lapply
  #, suspended = TRUE #start in suspensed state
  )#end observe 
  
  #Update stream temperature (sTem) dataframe to clean consistent format
  sTem_df <- eventReactive(input$colselect, {
    df <- user_data()%>% #[,input$user_datainput_columns_selected] 
      dplyr::rename("site_id" = input$ID_colnm , "date_raw" = input$date_colnm, "T_stream"  = input$T_colnm)%>%
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
    print(df)
    df
    })
  
  # show updated table based on column selected
  observeEvent(input$colselect, { #stream temperature df = sT_df
    #output new table with colums
    output$user_datainput <-DT::renderDataTable(
      sTem_df(), server = TRUE, selection = list(target = 'column'),
      caption = 'Updated Input Data Table', )
  },ignoreInit = TRUE)
  
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
    
    read.csv(inFile$datapath, skip = input$colnm_row_loc -1, sep = input$upload_deml_loc)
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
      dplyr::rename("site_id" = input$IDloc_colnm, "lat" = input$lat_colnm, "long" = input$long_colnm)%>%
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
      dplyr::mutate(site_id = as.factor(site_id))%>%
      dplyr::group_by(site_id)%>%
      dplyr::summarise(start_date = min(date),
                end_date = max(date),
                site_id = first(site_id))%>%
      dplyr::inner_join(sLoc_df(), .)
  }) #end date_group reactive
  
  
  #output location table
  output$user_dataavail <-DT::renderDataTable(
    loc_df(), server = TRUE, selection = list(target = 'column'),
    caption = 'Joined Input Data Table'
  )
  
  #####################
  ####Air Data 
  user_air <-  eventReactive(input$upload_air,
    {
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        
        inFile <- input$upload_air
        
        if (is.null(inFile))
          return(NULL)
        
        file_df <-read.csv(inFile$datapath, skip = input$colnm_row_air -1, sep = input$upload_deml_air)
      }
    )
  
  ##################################################  
  #update column name selection based on user column select
  colnm_input_air <- c("T_colnm_air", "ID_colnm_air", "date_colnm_air")
  
  #update all the colnames choices for user to select correct match
  observe(
    lapply(colnm_input_air, function(x){
      updateSelectInput(session, x,
                        #label = paste("Select input label", length(x)),
                        choices = names(user_air())#,#[,input$user_datainput_columns_selected]),#therefore columns dont need to be selected
                        #selected = tail(x, 1)
      )
    }
    )#end lapply
    #, suspended = TRUE #start in suspensed state
  )#end observe 
  
  #Update stream temperature (sTem) dataframe to clean consistent format

  
  aTem_df <- eventReactive(input$add_air, { 
    if(input$air_choice == "user_upload"){
      df <- user_air()%>% 
          dplyr::rename("site_id" = input$ID_colnm_air , "date_raw" = input$date_colnm_air, "T_air"  = input$T_colnm_air)%>%
          mutate(date = as.Date(date_raw, format = input$date_format_air),
                 site_id = as.factor(site_id)) %>%
          dplyr::select(site_id, date, T_air, date_raw)%>%
          group_by(site_id) %>%
          timetk::summarise_by_time( #for daily time steps from hourly.
            .date_var = date,
            .by       = "day", # Setup for monthly aggregation
            # Summarization
            tavg_air_C = mean(T_air)
          )
      
        #### Making sure input in in Celsius or converts it
        #using radio buttons
        aTem_units <- switch(input$temp_unit_air,
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
        df$tavg_air_C <- aTem_units(df$tavg_air_C)
        df
      
      
    }else{ # == daymet
      
    #run batch collection from daymet
    aTem <- batch_daymet(loc_df())
    #clean data and pull out avgdaily air temperature 
    aTem <- clean_daymet(aTem)%>%
      dplyr::select(site_id, date, tavg_air_C)
    
    aTem}
  })
  
  ####Join Air and Stream ### 
  ##dataframe temperature = Tem_df##
  
  #
  Tem_df <- reactive({
    Tem_df <- left_join(sTem_df(), aTem_df(), by = c("site_id", "date"))%>%
    na.omit()
    
    saveRDS(Tem_df, "Tem_df_20220927.RDS")
    
    Tem_df
    
  })
  
#output table
  output$user_dataair <-DT::renderDataTable( #https://rstudio.github.io/DT/server.html for largr data
    Tem_df(), server = TRUE, selection = list(target = 'column'),
    caption = 'Raw Data Table'
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
      TMy_output(Tem_df())
    
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