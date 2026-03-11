#UI

#### Start UI ####
ui <- dashboardPage(
  #secure_app( #wrap in secure_app for authentication with shiny_manager. Undo hash to enable, see `setup` folder for credentials and contact Sam Struthers for usernames/passwords
    dashboardHeader(title = "Water Quality Monitoring Dashboard"),
    #### Define Sidebar ####
    dashboardSidebar(
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Live WQ Data", tabName = "sensor_data", icon = icon("chart-line")),
        #Placeholder for future functions
        #menuItem("WQ Forecast", tabName = "forecast", icon = icon("bolt"))
        menuItem("CLP Basin Conditions", tabName = "flow_data", icon = icon("droplet")),
        menuItem("Site Map", tabName = "map", icon = icon("map"))
#TODO: Create these tabs and migrate functions as necessary
        #Placeholder for future functions
        #menuItem("Real-Time TOC", tabName = "toc_realtime", icon = icon("arrow-up-right-dots")),
        #menuItem("Water Quality Forecasts", tabName = "forecasts", icon = icon("bolt)),
        #menuItem("Longitudinal Water Quality", tabName = "long_wq", icon = icon("water")),
      )
    ),
    #### Define Body Styling and start tabs ####
    dashboardBody(
      tags$head(
        tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
      ),
      # Adjusting size of progress bar
      tags$style(HTML("
  .shiny-notification-content-progress {
    font-size: 72px !important;
    padding: 60px !important;
  }
  .progress {
    height: 120px !important;
  }
  .progress-bar {
    font-size: 64px !important;
    line-height: 120px !important;
  }
")),

      tabItems(
        #### Home Tab ####
        home_ui("home"),
        
        #### Sensor Data Tab ####
        tabItem(tabName = "sensor_data",
                fluidRow(
                  box(
                    title = "Data Controls", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4,
                             dateRangeInput("date_range",
                                            label = "Select Date Range:",
                                            start = ymd("2025-11-10") - days(7), # replace with Sys.Date() - days(7) in production
                                            end = ymd("2025-11-10"), # replace with Sys.Date() in production
                                            max = ymd("2025-11-10") # replace with Sys.Date() in production
                             ),
                             pickerInput("sites_select",
                                         label = "Select Sites:",
                                         choices = c("South Fork CLP", "Chambers Lake Outflow", "CLP at Poudre Falls", "Canyon Mouth", "CLP at Indian Meadows", "CLP at Manners Bridge"),
                                         selected = c("South Fork CLP", "Chambers Lake Outflow", "CLP at Poudre Falls", "Canyon Mouth", "CLP at Indian Meadows", "CLP at Manners Bridge"),
                                         multiple = TRUE,
                                         options = pickerOptions(
                                           actionsBox = TRUE,
                                           selectAllText = "Select All",
                                           deselectAllText = "Deselect All"
                                         )
                             )
                      ),
                      column(4,
                             pickerInput("parameters_select",
                                         label = "Select Parameters:",
                                         choices = c("Temperature", "Turbidity", "pH", "DO",
                                                     "Specific Conductivity", "Chl-a Fluorescence", "FDOM Fluorescence", "Depth"),
                                         selected = c("Temperature", "Turbidity","Specific Conductivity", "Chl-a Fluorescence", "FDOM Fluorescence"),
                                         multiple = TRUE,
                                         options = pickerOptions(
                                           actionsBox = TRUE,
                                           selectAllText = "Select All",
                                           deselectAllText = "Deselect All"
                                         )
                             ),

                             selectInput("data_timestep", "Summarizing Time Step",
                                         choices = c("15 mins", "1 hour", "3 hours", "6 hours", "12 hours", "1 day"),
                                         selected = "1 hour"),

                             uiOutput("dynamic_load_button"),
                      ),
                      column(4,
                             h4("Log Scale Controls:"),
                             br(),
                             uiOutput("log_controls"),
                             br()#,
                            #Download currently disabled
                             #downloadButton("download_data", "Download Data",
                             #               class = "btn-success")
                      )
                    )
                  )
                ),

                fluidRow(
                  box(
                    title = "Time Series Plots", status = "primary", solidHeader = TRUE, width = 12,collapsible = TRUE,

                    # Add descriptive text below the title
                    tags$p(
                      "These are live data from sensors deployed across the Cache la Poudre watershed. QAQC filters attempt to remove erronous data but may not cover all scenarios. Functions to smooth data are also applied to remove noise from optical sensors (Turbidity, Chl-a) when QAQC is applied. All data is summarized to the `Summarizing Time Step` by calculating a rolling median. CLP at Poudre Falls and CLP at Manner's Bridge are known to have data transmission gaps due to their locations.",
                      style = "margin-bottom: 15px; font-weight: normal; font-style: italic;"
                    ),

                    uiOutput("dynamic_plots") %>% withSpinner()
                  )
                ),
                fluidRow(
                  box(
                    title = "Modelled Time Series Plots",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    collapsible = TRUE,

                    # Add descriptive text below the title
                    tags$p(
                      "These are preliminary model results. Model Ensemble line represents the mean of four separate models while the range represents the maximum and minimum estimates across models. Data gaps represent data removed due to QAQC process or due to data transmission errors.  Historical grab sampling values for comparison are available from 4/1/25-7/1/25, please change the date range above to view.",
                      style = "margin-bottom: 16px; font-weight: bold; font-style; normal;"
                    ),

                    # Plot output
                    fluidRow(
                      column(
                        12,
                        uiOutput("toc_plots_panel")
                      )
                    )
                  )
                )

        ),
        #### WQ Site Map Tab ####
        tabItem(tabName = "map",
                fluidRow(
                  box(
                    title = "Site Locations", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("site_map", height = "600px") %>% withSpinner()
                  )
                )
        ),
        #### Flow Data Tab ####
        tabItem(tabName = "flow_data",
                layout_columns(
                  col_widths = c(7, 5),
                  # 3-day Q plot card
                  card(
                    card_header("Current flows (7 days) for key CLP sites"),
                    card_body(
                      plotlyOutput("seven_day_q_plot")
                    )
                  ),
                  # Conditional SNOTEL card that will only show between October and July
                  uiOutput("snotel_card")
                ),
                # Main Map Card
                card(
                  card_header("Tracking Flow rates through the CLP network"),
                  card_body(
                    leafletOutput("map", height = 600)
                  ),
                  card_footer(
                    "Data retrieved from USGS/CDWR using the cdssr package. Colors indicate flow trend in last 24 hours: red (decreasing), green (increasing), grey (no data)."
                  )
                )
        )
        #### End of Tabs ####
      )
    )
  )
# ) #UNDO HASH TO ENABLE SHINY MANAGER AUTHENTICATION, SEE `setup` folder for credentials and contact Sam Struthers for usernames/passwords
#### End of UI ####
