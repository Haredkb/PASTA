###UI functions
########################################
##### NWIS UI ##############################
############################################
############################################

nwisUI <- function(id, label = "Automated NWIS") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    add_busy_spinner(spin = "radar", position = "full-page", margins = c(10, 20)),
    ##TABS##
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(id = ns("nwis_calc"), #type = "tabs",
                tabPanel("Input: Available Stream Sites",
                         sidebarPanel(
                           #add_busy_spinner(spin = "cube-grid"),
                           
                           h6("STEP 1"),
                           p("Choose A Single State or Territory"),
                           # "state.name will be replaced with values from NWIS
                           selectInput(ns("state"), "State", state.abb, #want to add canada and mexico #https://tengl.net/blog/2020/1/7/drawing-canada-maps-in-r
                                       multiple = FALSE), #
                           
                           p("OR Use Map Extent Coordinates (will override state input)"),
                           checkboxInput(ns("mapextent"), "Use Map Extent instead of State?"), #input$myMap_bounds #https://stackoverflow.com/questions/44179257/getting-bounding-box-from-leaflet-in-r
                           p("**Area cannot be larger than [13.3x3.6 degrees]"),
                           
                           verbatimTextOutput(ns("AOI")), #area of interest
                           
                            #area of interest
                           
                           # #slider # add in water year (from DVStats) then use this function 
                           # sliderInput("year.range", "Analysis Years", value = c(year(Sys.Date()- years(8)), year(Sys.Date()- years(4))),
                           #                                     min = year(as.Date("1979-10-01")), max = year(Sys.Date()- years(2)), sep = ""),
                           #consider dateRangeInput in future iterations
                           p("Select date range of interest (min 1 year):"),
                           
                           dateRangeInput(ns("date.range"), "Analysis Years",
                                          start = as.Date("2010-10-01"),
                                          end   = as.Date("2015-09-30"),
                                          min    = as.Date("1980-01-01"),
                                          max    = as.Date("2022-12-30")),
                           #useShinyalert(),
                           actionButton(
                             inputId = ns("searchsites"),
                             label = "Search for Available Sites"),
                           
                           hr(),
                           
                           ###set parameter choices
                           
                           
                           
                           hr(),
                           
                           h6("STEP 2"),
                           
                           p('Select the sites of interest from the adjacent table, then press calculate metrics, the results will be shown on the next tab'),
                           
                           # selectInput(ns("yr_type"), "Conduct Analysis via Water Year (October Start) or Calendar Year (January Start)?", c("calendar", "water"), #want to add canada and mexico #https://tengl.net/blog/2020/1/7/drawing-canada-maps-in-r
                           #             multiple = FALSE), #
                           # 
                           radioButtons(ns("yr_type"), "Conduct Annual Signal Analysis via Water Year (October Start - recommended) or Calendar Year (January Start)?", 
                                        choices = c("Calendar" = "calendar", 
                                                    "Water Year" = "water"), 
                                        selected = "water"),
                           
                           #checkboxInput(ns("bfi"), "Download Flow Data, if available?", value = FALSE),# as.factor(parameter$parameter_nm)),
                            p("BFI calculation Temporarily Unavailable"),
                           ##add action button so thermal parameter run only happens after user is ready
                           actionButton(inputId = ns("gobutton"),label = "Calculate Thermal Metrics",
                                        style="padding:20px; font-size: 22px; color: #fff; background-color: #FF0000; border-color: #2e6da4"),
                           
                           
                           ##move the progress bar
                           tags$head(tags$style(
                             HTML(".shiny-notification {position:fixed;top: 75% ;left: 50%; }"))),
                         ),
                         
                         
                         
                         ##main panel with tabs for different output 
                         mainPanel(
                           h2("Sites with Available Temperature Data"),
                           leafletOutput(ns("dataavailmap")),
                           dataTableOutput(ns("site_table")),
                           p()#,
                           #actionButton("explore", "Update Points")
                         ),
                ),#end mainpanel
                
                tabPanel(title = "Results: Metric Tables", 
                         value = ns('results_tbl'), 
                        fluidRow(
                          
                         h5("Input Air and Stream Temperature Data, with flow and daily BFI if selected:"),
                         downloadButton(ns("download_rawdata"), "Download Air and Stream Data"),
                         p("Annual Temperature Signal Fit Parameters:"),
                         downloadButton(ns("download_fitdata"), "Download Annual Signal Fit Data"),
                         
                         br(),
                         h3("Scroll down for results"),
                         column(6,
                         h6("Grey Columns are assoicated with paired air and stream annual signals calculations"),
                         p("Amp_Ratio is Amplitude Ratio, unitless"),
                         p("PhaseLag_d is Phase Lag, days"),
                         p("Mean_ratio is ratio of average water temperature divided average air temperature")),
                         
                         column(6,
                         h6("Blue columns are associated with air and stream temperature linear regression"),
                         p("TS__Slope, is the slope of the linear relationship between air and water temperature"),
                         p("AdjRsqr, is the r2 of the linear fit"),
                         p("YInt, is the y intercept of the linear relationship")
                         ),
                         h5("Please review the literature citations from the information tab to explore how to interpret these data"),
                         img(src='InterpretationTable.jpeg', align = "right"),
                         h4("Summary Metric Data Table"),
                         downloadButton(ns("downloadData"), "Download Paired Stream-Air Metric DataTable"),
                         dataTableOutput(ns("metric_table")),
                         
                         textOutput(ns("datafail")),
                         
                         h4("Yearly Summary Metric Data Table"),
                         downloadButton(ns("download_TMyearly"), "Download DataTable by Year"),
                         dataTableOutput(ns("user_yearlyTM")),
                         
                         leafletOutput(ns("metricmap"))
                         
                #p(),
                #actionButton("recalc", "Update Points")),
                
                #tabPanel("Summary", verbatimTextOutput("summary"))
    
                )#end fluid row 
                ),
                
                tabPanel("Data Plots",
                         "Annual Temperature Signal Data Fit",
                         #fileInput("upload_water", "Upload Clean Dataframe as csv"),
                         #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                         plotlyOutput(ns("plot_tempdata"))
                         #DT::dataTableOutput("user_dataair")
                ),#end plots panel
                
                tabPanel("Results Plots",
                         "",
                         #fileInput("upload_water", "Upload Clean Dataframe as csv"),
                         #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                         fluidRow(
                           splitLayout(cellWidths = c("50%", "50%"), plotlyOutput(ns("plot_TAS")),#Temperature ANnual Signals
                                       plotlyOutput(ns("plot_TS"))))
                         #DT::dataTableOutput("user_dataair")
                )
  ),#end page1
  )
}

##############################################
##############################################
##    NorWest UI             ###################
##############################################

norwestUI <- function(id, label = "norwest") {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  
  tagList(
    #add_busy_spinner(spin = "radar", position = "full-page", margins = c(10, 20)),
    ##TABS##
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(id = ns("norW_calc"), type = "tabs",
                tabPanel("Input: Available Stream Sites",
                         sidebarPanel(
                           #add_busy_spinner(spin = "cube-grid"),
                           #h2("*UNDER DEVELOPMENT*", style = "color:red"),
                           strong("STEP 1"),
                           strong("Choose Basin/Processing Units, note selecting more than 3 can take a while"),

                           #most have been checked but some may need to be changed if url cannot be found
                           selectInput(ns("NorPU"), "NorWeST Processing Units", multiple = TRUE, 
                                       choices = c("Arizona" = "Arizona",
                                                   "Black Hills"  = "BlackHills",
                                                   "Central California" = "CentralCalifornia",
                                                   "Colorado" = "Colorado",
                                                   "Clearwater River Basin" = "Clearwater",
                                                   "Coastal California" = "CoastalCalifornia",
                                                   "Eastern Montana" = "eastern-montana",
                                                   "Lahontan Basin"= "Lahontan", 
                                                   "MidColumbia" = "MidColumbia",
                                                   "MidSnake"= "MidSnake",
                                                   "New Mexico" = "NewMexico",
                                                   "Northern California Coastal Klamath" = "NorthCaliforniaCoastalKlamath",
                                                   "Oregon Coast" = "OregonCoast",
                                                   "Salmon River Basin" = "Salmon",
                                                   "Missouri Headwaters" = "MissouriHW",
                                                   "Snake-Bear" = "SnakeBear",
                                                   "South-Central Oregon" = "SouthCentralOregon",
                                                   "SpoKoot" = "Spokoot",
                                                   "Upper Columbia-Yakima" = "UpperColumbiaYakima",
                                                   "Upper Green-North Platte",
                                                   "Upper Missouri - Marias" = "MariasMissouri",
                                                   "Upper Yellowstone-Bighorn" = "UpperYellowstoneBighorn",
                                                   "Utah" = "Utah",
                                                   "Washington Coast" = "WashingtonCoast")
                           ),
     

                           actionButton(
                             inputId = ns("getData"),
                             label = "Get Data"),
                           p("Can take a few minutes, especially for multiple processing units"),
                           hr(),
                           
                           ###set parameter choices
                                                      hr(),
                           
                           h3("STEP 2"),
                           h4('Select the sites of interest from the adjacent table, then press calculate metrics, the results will be shown on the next tab'),
                           
                           selectInput(ns("yr_type"), "Conduct Analysis via Water Year (October Start-recommended) or Calendar Year (January Start)?", 
                                       c("water", "calendar"), #want to add canada and mexico #https://tengl.net/blog/2020/1/7/drawing-canada-maps-in-r
                                       multiple = FALSE), #
                           
                           ##add action button so thermal parameter run only happens after user is ready
                           actionButton(inputId = ns("gobutton"),label = "Calculate Thermal Metrics",
                                        style="padding:20px; font-size: 22px; color: #fff; background-color: #FF0000; border-color: #2e6da4"),
                           
                           
                           ##move the progress bar
                           tags$head(tags$style(
                             HTML(".shiny-notification {position:fixed;top: 75% ;left: 50%; }"))),
                         ),#end sidebar panel
                         #                      
                         #                      
                         #                      
                         ##main panel with tabs for different output
                         mainPanel(
                           h2("Sites with Available Temperature Data"),
                           textOutput("datafail"),
                           leafletOutput(ns("dataavailmap")),
                           
                           dataTableOutput(ns("site_table")),
                           p()#,
                           #actionButton("explore", "Update Points")
                         ),
                ),#end mainpanel
                #             
                tabPanel(title = "Results: Metric Tables", 
                         value = ns('results_tbl'),
                         
                        fluidRow(
                          img(src='InterpretationTable.jpeg', align = "right"),
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
                         
                         dataTableOutput(ns("metric_table")),
                         downloadButton(ns("download_rawdata"), "Download Raw Air and Stream Data"),
                         downloadButton(ns("downloadData"), "Download Paired Stream-Air Metric DataTable"),
                         #plotOutput("AS_plot"),#annual signal plot
                         #plotOutput("TS_plot"),
                         leafletOutput(ns("metricmap")),
                         #p(),
                         #actionButton("recalc", "Update Points")),
                        )
                         #tabPanel("Summary", verbatimTextOutput("summary"))
                ),
                tabPanel("Data Plots",
                         "Annual Temperature Signal Data Fit",
                         #fileInput("upload_water", "Upload Clean Dataframe as csv"),
                         #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                         plotlyOutput(ns("plot_tempdata")),
                         downloadButton(ns("downloadSinData"))
                         #DT::dataTableOutput("user_dataair")
                ),#end plots panel
                
                tabPanel("Results Plots",
                         "",
                         #fileInput("upload_water", "Upload Clean Dataframe as csv"),
                         #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
                         fluidRow(
                           splitLayout(cellWidths = c("50%", "50%"), plotlyOutput(ns("plot_TAS")),#Temperature ANnual Signals
                                       plotlyOutput(ns("plot_TS"))))
                         #DT::dataTableOutput("user_dataair")
                )
    ),#end page1
    
  )
  
}

#### Need Updates ### 
##############################################
##############################################
##    envCan UI             ###################
##############################################

# envCanUI <- function(id, label = "envCanada automated") {
#   # `NS(id)` returns a namespace function, which was save as `ns` and will
#   # invoke later.
#   ns <- NS(id)
#   
#   
#   tagList(
#     #add_busy_spinner(spin = "radar", position = "full-page", margins = c(10, 20)),
#     ##TABS##
#     # Output: Tabset w/ plot, summary, and table ----
#     tabsetPanel(id = "envC_calc", type = "tabs",
#                 tabPanel("Input: Available Stream Sites",
#                          sidebarPanel(
#                            #add_busy_spinner(spin = "cube-grid"),
#                            h2("*UNDER DEVELOPMENT*", style = "color:red"),
#                            h4("STEP 1"),
#                            h4("Choose Stations"),
#                            # "state.name will be replaced with values from NWIS
#                            #selectInput(ns("station"), "Stations", choices = NULL)
#                            selectInput(ns("station"), "Stations", envCan_stations$STATION_NO, #want to add canada and mexico #https://tengl.net/blog/2020/1/7/drawing-canada-maps-in-r
#                                        multiple = TRUE), #
#                                           
#                            h4("OR Use Map Extent Coordinates (will override station list input)"),
#                            checkboxInput(ns("mapextent"), "Use Map Extent instead of station list?"), #input$myMap_bounds #https://stackoverflow.com/questions/44179257/getting-bounding-box-from-leaflet-in-r
#                            p("**Area cannot be larger than [13.3x3.6 degrees]"),
# 
#                            verbatimTextOutput(ns("AOI")), #area of interest
#                            
#                          
# 
#                            # #slider # add in water year (from DVStats) then use this function
#                            # sliderInput("year.range", "Analysis Years", value = c(year(Sys.Date()- years(8)), year(Sys.Date()- years(4))),
#                            #                                     min = year(as.Date("1979-10-01")), max = year(Sys.Date()- years(2)), sep = ""),
#                            #consider dateRangeInput in future iterations
#                             
#                             actionButton(
#                               inputId = ns("getData"),
#                               label = "Get Data"),
#                             p("Can take a few minutes, especially for multiple sites"),
#                             hr(),
# 
#                            ###set parameter choices
# 
# 
#                            hr(),
# 
#                            h2("STEP 2"),
# 
#                            h4('Select the sites of interest from the adjacent table, then press calculate metrics, the results will be shown on the next tab'),
# 
#                                                      ##add action button so thermal parameter run only happens after user is ready
#                            actionButton(inputId = ns("gobutton"),label = "Calculate Thermal Metrics",
#                                         style="padding:20px; font-size: 22px; color: #fff; background-color: #FF0000; border-color: #2e6da4"),
# 
# 
#                            ##move the progress bar
#                            tags$head(tags$style(
#                              HTML(".shiny-notification {position:fixed;top: 75% ;left: 50%; }"))),
#                          ),#end sidebar panel
#     #                      
#     #                      
#     #                      
#                          ##main panel with tabs for different output
#                          mainPanel(
#                            h2("Sites with Available Temperature Data"),
#                            leafletOutput(ns("dataavailmap")),
#                            downloadButton(ns("download_rawdata"), "Download Air and Stream Data"),
#                            dataTableOutput(ns("site_table")),
#                            p()#,
#                            #actionButton("explore", "Update Points")
#                          ),
#                 ),#end mainpanel
#     #             
#               tabPanel("Results: Metric Table and Plots",
#                        
#                        h4("Metric Data Table"),
#                        h5("Grey Columns are assoicated with paired air and stream annual signals calculations"),
#                        p("Amp_Ratio is Amplitude Ratio, unitless"),
#                        p("PhaseLag_d is Phase Lag, days"),
#                        p("Mean_ratio is ratio of average water temperature divided average air temperature"),
#                        h5("Blue columns are associated with air and stream temperature linear regression"),
#                        p("TS__Slope, is the slope of the linear relationship between air and water temperature"),
#                        p("AdjRsqr, is the r2 of the linear fit"),
#                        p("YInt, is the y intercept of the linear relationship"),
#                        h5("Please review the literature citations from the information tab to explore how to interpret these data"),
#                        
#                        dataTableOutput(ns("metric_table")),
#                        downloadButton(ns("downloadData"), "Download Paired Stream-Air Metric DataTable"),
#                        #plotOutput("AS_plot"),#annual signal plot
#                        #plotOutput("TS_plot"),
#                        leafletOutput(ns("metricmap")),
#                        #p(),
#                        #actionButton("recalc", "Update Points")),
# 
#                        #tabPanel("Summary", verbatimTextOutput("summary"))
#               ),
#     tabPanel("Data Plots",
#              "Annual Temperature Signal Data Fit",
#              #fileInput("upload_water", "Upload Clean Dataframe as csv"),
#              #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
#              plotlyOutput(ns("plot_tempdata")),
#              downloadButton(ns("downloadSinData"))
#              #DT::dataTableOutput("user_dataair")
#     ),#end plots panel
#     
#     tabPanel("Results Plots",
#              "",
#              #fileInput("upload_water", "Upload Clean Dataframe as csv"),
#              #checkboxInput("choose_clean_input", "Data File Meets Input Criteria (see requirements below)"),
#              fluidRow(
#                splitLayout(cellWidths = c("50%", "50%"), plotlyOutput(ns("plot_TAS")),#Temperature ANnual Signals
#                            plotlyOutput(ns("plot_TS"))))
#              #DT::dataTableOutput("user_dataair")
#     )
#      ),#end page1
#   
#   )
#   
# }
















# userDefinedUI <- function(id, label = "User Defined Input") {
#   # `NS(id)` returns a namespace function, which was save as `ns` and will
#   # invoke later.
#   ns <- NS(id)
#   
#   tagList(
#     add_busy_spinner(spin = "radar", position = "full-page", margins = c(10, 20)),
# 
# tabPanel(
#   "User-defined Data",
#   sidebarPanel(
#     
#     p("Under Development"),
#     
#     "Stream Temperature Dataset (required)",
#     numericInput(ns("colnm_row"), "Row Number of Column Names", value = 1),
#     fileInput(ns("upload_water"), "C:/Users/hared/Dropbox/UConn/Projects/300_Network_GW_Temp/200_Data_Input/SWT/LTER/HarvardForest/Big_Low.csv"),
#     
#     selectInput(ns("date_colnm"), "User Date Column Name", choices = NULL),
#     textInput(ns("date_format"), "Correct Date Format", placeholder = "%m/%d/%Y"),
#     a("Date Format Tips     ", "https://www.statmethods.net/input/dates.html"),#
#     selectInput(ns("ID_colnm"), "User ID Column Name", choices = NULL),
#     selectInput(ns("T_colnm"), "User Stream Temperature Column Name", choices = NULL),
#     radioButtons(ns("temp_unit"), "Temperature Units", choices = c("celsius" = "cel", 
#                                                                "fahrenheit" = "fhr", 
#                                                                "kelvin" = "kel"), 
#                  selected =  "celsius"),
#     
#     actionButton(ns("colselect"), "Update Stream Temperature DataTable"),
#     
#     ##Location Dataset - can read in same Temperature file if that where it exists? 
#     "Stream Location Data (required for Daymet data and plotting functionality)",
#     "Can be the same as Stream Temperature data input",
#     fileInput(ns("upload_loc"), NULL),
#     "select lat and long columns NOTE*(have to select columns body not column header)",
#     selectInput(ns("IDloc_colnm"), "User ID Column Name", choices = NULL), #needs to be distinct from ID id above
#     selectInput(ns("lat_colnm"), "User latitude column name", choices = NULL),
#     selectInput(ns("long_colnm"), "User longitude column name", choices = NULL),
#     actionButton(ns("locselect"), "Update Location DataTable"),
#     
#     
#     ##Air Temperature Inputs
#     checkboxInput(ns("air_included"), "Calculate using user input Air Temperature"),
#     actionButton(ns("daymet_select"), "Use Daymet Air Temperature Data"),
#     downloadButton(ns("downloadInputdata"), "Download Input DataTable"),
#     actionButton(ns("calc_metric_u"), "Calculate Thermal Metrics"),
#     br(),
#     "Air Temperature Dataset (optional)",
#     fileInput(ns("upload_air"), NULL)
#     
#   ),
#   ##main panel with tabs for different output 
#   mainPanel(
#     h2("User Input Data"),
#     #themal metrics dataframe
#     DT::dataTableOutput(ns("user_dataTM")),
#     #Datatable with air 
#     DT::dataTableOutput(ns("user_dataair")),
#     #datatable stream T data 
#     DT::dataTableOutput(ns("user_dataavail")),
#     #datatable location data 
#     DT::dataTableOutput(ns("user_dataloc")),
#     #leafletOutput("user_dataavailmap"),
#     DT::dataTableOutput(ns("user_datainput")),
#     
#     p(),
#     
#   ),
#   
# ) #end userdefined UI
# )
# }