# R/home_module.R

#### Home Module UI ####
home_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "home",
          shinyjs::useShinyjs(),
          tags$head(
            tags$style(HTML(paste0("
              #", ns("home_map_container"), " {
                position: relative;
                height: calc((100vh - 50px) * 0.55);
                margin: -15px -15px 0 -15px;
                border-bottom: 2px solid #ddd;
              }
              #", ns("home_plot_container"), " {
                height: calc((100vh - 50px) * 0.45);
                margin: 0 -15px -15px -15px;
                padding: 10px 15px;
                background-color: #f9f9f9;
                display: flex;
                flex-direction: column;
              }
              .map-overlay {
                position: absolute;
                top: 10px;
                left: 50px;
                z-index: 1000;
                background: rgba(255, 255, 255, 0.9);
                padding: 15px;
                border-radius: 8px;
                box-shadow: 0 0 15px rgba(0,0,0,0.2);
                width: 320px;
              }
              .stats-overlay {
                position: absolute;
                bottom: 30px;
                left: 10px;
                z-index: 1000;
                background: rgba(255, 255, 255, 0.9);
                padding: 10px;
                border-radius: 8px;
                box-shadow: 0 0 10px rgba(0,0,0,0.1);
              }
            ")))
          ),
          
          div(id = ns("home_map_container"),
              leafletOutput(ns("home_map"), height = "100%", width = "100%"),
              
              # Top Left Control Panel
              div(class = "map-overlay",
                  h3("System Controls", style = "margin-top: 0;"),
                  
                  # Parameter Selector
                  pickerInput(ns("map_param"), "Visualize Parameter:",
                              choices = c("FDOM Fluorescence", "Temperature", "Turbidity", "pH", "DO", 
                                          "Specific Conductivity", "Chl-a Fluorescence", "Depth", "Estimated TOC"),
                              selected = "Turbidity",
                              options = list(`style` = "btn-primary")),
                  
                  hr(),
                  
                  # Initialization Control
                  p(tags$small("The map shows the latest available snapshot. Initialize full data streams for historical trends.")),
                  actionBttn(ns("start_sync"), "Initialize Data", style = "jelly", color = "primary", icon = icon("download"), size = "sm"),
                  br(), br(),
                  uiOutput(ns("sync_checklist"))
              )
          ),
          
          div(id = ns("home_plot_container"),
              h4("Fort Collins Intake Total Organic Carbon (TOC) Forecast", style = "margin-top: 0; margin-bottom: 10px; text-align: center; flex: 0 0 auto;"),
              div(style = "flex: 1 1 auto; min-height: 0;",
                  plotlyOutput(ns("intake_toc_forecast_plot"), height = "100%")
              )
          )
  )
}

#### Home Module Server ####
home_server <- function(id, loaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 0. Load Snapshot immediately for the map
    snapshot_data <- reactiveVal(NULL)
    distributed_toc_data <- reactiveVal(NULL)
    intake_forecast_data <- reactiveVal(NULL)
    
    # Load initial snapshot (fastest path to map)
    observe({
      tryCatch({
        df <- arrow::read_parquet(snapshot_url, as_data_frame = TRUE)
        latest_snapshot <- df %>%
          group_by(site, parameter) %>%
          filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
          ungroup()
        snapshot_data(latest_snapshot)
      }, error = function(e) {
        showNotification("Failed to load initial map snapshot.", type = "error")
      })
    })

    # Sequence remaining data pulls AFTER map is visualized
    observeEvent(input$home_map_zoom, {
      req(input$home_map_zoom)
      
      # 1. Pull Distributed TOC for the map markers (the "Estimated TOC" overlay)
      later::later(function() {
        try({
          df <- arrow::read_parquet(distributed_forecast_url, as_data_frame = TRUE)
          distributed_toc_data(df)
        }, silent = TRUE)
      }, 0.5)

      # 2. Pull Intake Forecast for the bottom plot
      later::later(function() {
        try({
          df <- arrow::read_parquet(intake_forecast_url, as_data_frame = TRUE)
          intake_forecast_data(df)
        }, silent = TRUE)
      }, 1.5)
    }, once = TRUE)
    
    # 1. Track sync status
    sync_status <- reactiveValues(
      cached_data = "done",
      wet_api = "pending",
      hydrovu_api = "pending",
      contrail_api = "pending",
      cdwr_flow_api = "pending",
      snotel_api = "pending",
      all_done = FALSE
    )
    
    # Real-time TOC Estimate (Model-based) for the map
    realtime_toc_snapshot <- reactive({
      req(snapshot_data())
      
      # We need FDOM, Temp, SC, Turbidity, Chl-a for the model
      # Sites pman_fc and pbr_fc do not have FDOM
      sensor_snapshot <- snapshot_data() %>%
        filter(site %nin% c("pman_fc", "pbr_fc"))
      
      # Check if we have enough parameters to even try
      params <- unique(sensor_snapshot$parameter)
      required <- c("FDOM Fluorescence", "Temperature", "Specific Conductivity", "Turbidity")
      
      if (!all(required %in% params)) return(NULL)
      
      tryCatch({
        # Run the model on the snapshot
        # We need canyon_q which might not be loaded yet in the main server, 
        # but apply_toc_model has a fallback to pull it if NULL.
        res <- apply_toc_model(
          sensor_data = sensor_snapshot,
          toc_model_file_path = "data/models/ross_only_toc_xgboost_models_light_20260224.rds",
          scaling_params_file_path = "data/models/scaling_params_toc_20260224.parquet",
          summarize_interval = "15 mins", # Match the choices in the ui
          time_col = "DT_round",
          value_col = "mean"
        )
        
        if (nrow(res) > 0) {
          res %>%
            select(site, DT_round, mean = TOC_guess_ensemble) %>%
            mutate(
              parameter = "Estimated TOC",
              units = "mg/L",
              DT_round_MT = with_tz(DT_round, tzone = "America/Denver")
            )
        } else {
          NULL
        }
      }, error = function(e) {
        warning("Real-time TOC snapshot calculation failed: ", e$message)
        NULL
      })
    })
    
    output$sync_checklist <- renderUI({
      get_icon <- function(status) {
        if (status == "pending") return(icon("circle", class = "text-muted"))
        if (status == "loading") return(icon("spinner", class = "fa-spin text-primary"))
        if (status == "done") return(icon("check-circle", class = "text-success"))
      }
      
      tagList(
        tags$div(style = "font-size: 0.9em;",
          p(get_icon(sync_status$cached_data), " Snapshot Data Loaded"),
          p(get_icon(sync_status$wet_api), " Pulling WET API"),
          p(get_icon(sync_status$hydrovu_api), " Pulling HydroVu API"),
          p(get_icon(sync_status$contrail_api), " Pulling Contrail API"),
          p(get_icon(sync_status$cdwr_flow_api), " Pulling CDWR Flow API"),
          p(get_icon(sync_status$snotel_api), " Pulling SNOTEL API")
        )
      )
    })
    
    # Render Home Map (Static Base Only)
    output$home_map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 8, maxZoom = 15)) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Clean") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        # Set bounds to the CLP basin area
        setMaxBounds(lng1 = -106.1, lat1 = 40.3, lng2 = -104.9, lat2 = 41.0) %>%
        setView(lng = -105.3, lat = 40.6, zoom = 9) %>%
        addLayersControl(
          baseGroups = c("Clean", "Topographic", "Satellite"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })

    # Update Home Map Markers and Controls via Proxy
    observe({
      target_param <- input$map_param
      
      data_to_use <- if (!is.null(loaded_data()) && nrow(loaded_data()) > 0) {
        loaded_data()
      } else {
        snapshot_data()
      }
      
      req(data_to_use, nrow(data_to_use) > 0)
      
      latest_readings <- data_to_use %>%
        mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
        group_by(site, parameter) %>%
        filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()

      # Add Modeled TOC estimates if available
      rt_toc <- realtime_toc_snapshot()
      if (!is.null(rt_toc) && nrow(rt_toc) > 0) {
        latest_readings <- bind_rows(latest_readings, rt_toc)
      }

      snapshot_timestamp <- max(latest_readings$DT_round_MT, na.rm = TRUE)
      formatted_timestamp <- format(snapshot_timestamp, "%B %d, %Y %I:%M %p %Z")

      snapshot_wide <- latest_readings %>%
        select(site, parameter, mean, units) %>%
        mutate(display_val = paste0(round(mean, 2), " ", units)) %>%
        select(-mean, -units) %>%
        pivot_wider(names_from = parameter, values_from = display_val) 
      
      site_latest_times <- latest_readings %>%
        group_by(site) %>%
        summarise(DT_round_MT = max(DT_round_MT, na.rm = TRUE), .groups = "drop")
      
      target_numeric <- latest_readings %>%
        filter(parameter == target_param) %>%
        select(site, numeric_val = mean)
      
      # Use sonde_locations from global.R
      map_data <- sonde_locations %>%
        inner_join(snapshot_wide, by = "site") %>%
        inner_join(site_latest_times, by = "site") %>%
        left_join(target_numeric, by = "site") %>%
        filter(!is.na(lat), !is.na(lon))
      
      expected_params <- c("Temperature", "pH", "Specific Conductivity", "DO", 
                           "Turbidity", "FDOM Fluorescence", "Chl-a Fluorescence", 
                           "Depth", "Estimated TOC")
      
      popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
        row <- map_data[i, ]
        popup <- paste0(
          "<div style='min-width: 250px;'>",
          "<b>Site:</b> ", row$Site_Name, "<br>",
          "<b>Latest:</b> ", format(row$DT_round_MT, "%Y-%m-%d %H:%M"), "<br><hr style='margin: 8px 0;'>",
          "<table style='width: 100%; font-size: 0.95em;'>"
        )
        for (param in expected_params) {
          val <- if (param %in% names(row)) row[[param]] else NA
          is_missing <- is.na(val) || val == "NA NA" || is.null(val) || val == "NA"
          
          display_val <- if(is_missing) "<span style='color: #999; font-style: italic;'>No Data</span>" else val
          style <- if(param == target_param) "style='font-weight: bold; color: #E70870;'" else ""
          
          popup <- paste0(popup, "<tr><td ", style, ">", param, "</td><td ", style, " align='right'>", display_val, "</td></tr>")
        }
        # Add a Link to Trends
        popup <- paste0(popup, "</table>",
                        "<div style='text-align: center; margin-top: 10px;'>",
                        "<button class='btn btn-xs btn-primary' onclick='Shiny.setInputValue(\"", ns("view_trends"), "\", \"", row$site, "\", {priority: \"event\"})'>View Trends</button>",
                        "</div></div>")
        return(HTML(popup))
      })

      # Define Color Palettes (moved inside observe for proxy)
      if (target_param == "pH") {
        pal <- colorBin(palette = c("red", "orange", "green", "orange", "red"), bins = c(0, 6.8, 7.0, 8.3, 8.8, 14), domain = c(0, 14), na.color = "#a9a9a9")
      } else if (target_param == "Turbidity") {
        pal <- colorBin(palette = c("green", "orange", "red"), bins = c(0, 30, 50, Inf), domain = c(0, 1000), na.color = "#a9a9a9")
      } else if (target_param == "Specific Conductivity") {
        pal <- colorBin(palette = c("green", "orange", "red"), bins = c(0, 90, 100, Inf), domain = c(0, 500), na.color = "#a9a9a9")
      } else if (target_param == "DO") {
        pal <- colorBin(palette = c("red", "orange", "green"), bins = c(0, 6, 7, Inf), domain = c(0, 20), na.color = "#a9a9a9")
      } else if (target_param == "Estimated TOC") {
        pal <- colorBin(palette = c("green", "orange", "red"), bins = c(0, 4, 8, Inf), domain = c(0, 20), na.color = "#a9a9a9")
      } else {
        pal <- colorNumeric(palette = "viridis", domain = map_data$numeric_val, na.color = "#a9a9a9")
      }
      
      leafletProxy("home_map") %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          data = map_data,
          lng = ~lon, lat = ~lat,
          radius = 10, color = "#333333", weight = 1.5,
          fillColor = ~pal(numeric_val), fillOpacity = 0.9,
          popup = popup_content, label = ~Site_Name,
          labelOptions = labelOptions(textsize = "14px")
        ) %>%
        addLegend(
          position = "bottomright", pal = pal,
          values = map_data$numeric_val[!is.na(map_data$numeric_val)], 
          title = paste(target_param), opacity = 1
        ) %>%
        addControl(
          html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px 15px; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.2);'>",
                        "<h4 style='margin: 0; color: #333;'><b>Data Updated:</b><br>", formatted_timestamp, "</h4></div>"),
          position = "topright"
        )
    })

    # Fix disappearing map on tab return
    observeEvent(session$userData$parent_session$input$sidebar, {
      if (session$userData$parent_session$input$sidebar == "home") {
        shinyjs::delay(100, {
          shinyjs::runjs("window.dispatchEvent(new Event('resize'));")
        })
      }
    })
    
    # Handle "View Trends" from popup
    observeEvent(input$view_trends, {
      req(input$view_trends)
      
      # Switch to sensor data tab
      updateTabItems(session = session$userData$parent_session, "sidebar", "sensor_data")
      
      # Map the site code back to the display name for the picker
      site_display_name <- site_table %>%
        filter(site_code == input$view_trends) %>%
        pull(site_name)
      
      if (length(site_display_name) > 0) {
        updatePickerInput(session = session$userData$parent_session, "sites_select", selected = site_display_name)
      }
    })
    
    # Render Intake TOC Forecast Plot
    output$intake_toc_forecast_plot <- renderPlotly({
      req(intake_forecast_data())
      
      intake_cached_data <- intake_forecast_data() %>%
        filter(date == max(date, na.rm = TRUE)) %>% # Get the most recent forecast date
        mutate(across(contains("intake_q_swe_pred"), ~ round(.x, 2))) %>%
        filter(date_24h <= Sys.Date() + days(10)) #Limit to the next 10 days
        
      plot_toc_forecast(intake_cached_data)
    })
    
    return(list(
      start_sync = reactive({ input$start_sync }),
      set_status = function(step, status) { sync_status[[step]] <- status },
      full_sync_done = reactive({ sync_status$all_done }),
      snapshot_ready = reactive({ !is.null(snapshot_data()) }),
      cached_df = reactive({
        # Return the full dataset that was loaded for the snapshot
        # This will be available to the main server to avoid re-downloading
        arrow::read_parquet(snapshot_url, as_data_frame = TRUE)
      })
    ))
  })
}
