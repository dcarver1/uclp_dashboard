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
    
    observe({
      github_link <- "https://github.com/rossyndicate/uclp_dashboard/raw/main/data/data_backup.parquet"
      
      tryCatch({
        df <- arrow::read_parquet(github_link, as_data_frame = TRUE)
        
        latest_snapshot <- df %>%
          group_by(site, parameter) %>%
          filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
          ungroup()
        
        snapshot_data(latest_snapshot)
      }, error = function(e) {
        showNotification("Failed to load initial map snapshot.", type = "error")
      })
    })
    
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
    
    # Render Home Map
    output$home_map <- renderLeaflet({
      target_param <- input$map_param
      
      data_to_use <- if (!is.null(loaded_data()) && nrow(loaded_data()) > 0) {
        loaded_data()
      } else {
        snapshot_data()
      }
      
      if (is.null(data_to_use) || nrow(data_to_use) == 0) {
        return(leaflet() %>% addTiles() %>% setView(lng = -105.3, lat = 40.6, zoom = 10))
      }
      
      latest_readings <- data_to_use %>%
        mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
        group_by(site, parameter) %>%
        filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()

      # Fetch TOC forecasts as the "Estimated TOC" for the map to avoid XGBoost pointer corruption issues
      # and slow API calls during map rendering.
      try({
        toc_forecasts <- arrow::read_parquet("data/toc_forecast_distributed_backup.parquet", as_data_frame = TRUE)
        latest_toc <- toc_forecasts %>%
          filter(date_24h == Sys.Date() | date_24h == max(date_24h, na.rm = TRUE)) %>%
          group_by(site_code) %>%
          slice(1) %>%
          ungroup() %>%
          mutate(
            site = tolower(site_code),
            site = ifelse(site %in% c("pman", "pbr"), paste0(site, "_fc"), site)
          ) %>%
          select(site, mean = dist_mean_pred_toc) %>%
          mutate(
            parameter = "Estimated TOC",
            units = "mg/L",
            DT_round_MT = max(latest_readings$DT_round_MT, na.rm = TRUE)
          )
        
        latest_readings <- bind_rows(latest_readings, latest_toc)
      }, silent = TRUE)

      snapshot_timestamp <- max(latest_readings$DT_round_MT, na.rm = TRUE)
      formatted_timestamp <- format(snapshot_timestamp, "%B %d, %Y %I:%M %p %Z")

      snapshot_wide <- latest_readings %>%
        select(site, DT_round_MT, parameter, mean, units) %>%
        mutate(display_val = paste0(round(mean, 2), " ", units)) %>%
        select(-mean, -units) %>%
        pivot_wider(names_from = parameter, values_from = display_val) 
      
      target_numeric <- latest_readings %>%
        filter(parameter == target_param) %>%
        select(site, numeric_val = mean)
      
      locations <- read_csv("data/sonde_location_metadata.csv", show_col_types = FALSE) %>%
        separate(col = "lat_long", into = c("lat", "lon"), sep = ",", convert = TRUE) %>%
        mutate(
          site = tolower(Site),
          site = ifelse(site %in% c("pman", "pbr"), paste0(site, "_fc"), site)
        ) %>%
        select(site, Site_Name = Site, lat, lon, watershed)
      
      map_data <- locations %>%
        inner_join(snapshot_wide, by = "site") %>%
        left_join(target_numeric, by = "site") %>%
        filter(!is.na(lat), !is.na(lon)) %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      expected_params <- c("Temperature", "pH", "Specific Conductivity", "DO", 
                           "Turbidity", "FDOM Fluorescence", "Chl-a Fluorescence", 
                           "Depth", "Estimated TOC")
      
      map_data$popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
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
        popup <- paste0(popup, "</table></div>")
        return(HTML(popup))
      })
      
      # Define Color Palettes
      if (target_param == "pH") {
        # pH: <6.8 or >8.8 = red, 8.6-8.3 or 7-6.8 = orange, 7-8.3 = green
        pal <- colorBin(
          palette = c("red", "orange", "green", "orange", "red"),
          bins = c(0, 6.8, 7.0, 8.3, 8.8, 14),
          domain = c(0, 14),
          na.color = "#a9a9a9"
        )
      } else if (target_param == "Turbidity") {
        # Turbidity: >50 = red, 50-30 = orange, <30 = green
        pal <- colorBin(
          palette = c("green", "orange", "red"),
          bins = c(0, 30, 50, Inf),
          domain = c(0, 1000),
          na.color = "#a9a9a9"
        )
      } else if (target_param == "Specific Conductivity") {
        # SC: >100 = red, 100-90 = orange, <90 = green
        pal <- colorBin(
          palette = c("green", "orange", "red"),
          bins = c(0, 90, 100, Inf),
          domain = c(0, 500),
          na.color = "#a9a9a9"
        )
      } else if (target_param == "DO") {
        # DO: <6 = red, 6-7 = orange, >7 = green
        pal <- colorBin(
          palette = c("red", "orange", "green"),
          bins = c(0, 6, 7, Inf),
          domain = c(0, 20),
          na.color = "#a9a9a9"
        )
      } else if (target_param == "Estimated TOC") {
        # TOC: >8 = red, 8-4 = orange, <4 = green
        pal <- colorBin(
          palette = c("green", "orange", "red"),
          bins = c(0, 4, 8, Inf),
          domain = c(0, 20),
          na.color = "#a9a9a9"
        )
      } else {
        pal <- colorNumeric(palette = "viridis", domain = map_data$numeric_val, na.color = "#a9a9a9")
      }
      
      leaflet(map_data) %>%
        setView(lng = -105.3, lat = 40.6, zoom = 9) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Clean") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addControl(
          html = paste0("<div style='background: rgba(255,255,255,0.9); padding: 10px 15px; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.2);'>",
                        "<h4 style='margin: 0; color: #333;'><b>Data Updated:</b><br>", formatted_timestamp, "</h4></div>"),
          position = "topright"
        ) %>%
        addCircleMarkers(
          radius = 10, color = "#333333", weight = 1.5,
          fillColor = ~pal(numeric_val), fillOpacity = 0.9,
          popup = ~popup_content, label = ~Site_Name,
          labelOptions = labelOptions(textsize = "14px")
        ) %>%
        addLegend(
          position = "bottomright", pal = pal,
          values = ~numeric_val[!is.na(numeric_val)], 
          title = paste(target_param), opacity = 1
        ) %>%
        addLayersControl(
          baseGroups = c("Clean", "Topographic", "Satellite"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    # Handle "View Trends" from popup
    observeEvent(input$view_trends, {
      # This needs to interact with the parent session to change tabs
      # We use a custom message or just updateTabItems if we can access the parent
      updateTabItems(session = session$userData$parent_session, "sidebar", "sensor_data")
      # Also pre-select the site if possible - this requires more integration with the sensor_data tab
    })
    
    # Render Intake TOC Forecast Plot
    output$intake_toc_forecast_plot <- renderPlotly({
      req(input$home_map_zoom) # Defer plot processing until the leaflet map has initialized and rendered
      
      intake_forecast_github_link <- "https://github.com/rossyndicate/uclp_dashboard/raw/main/data/toc_forecast_intake_backup.parquet"
      
      intake_cached_data <- arrow::read_parquet(intake_forecast_github_link, as_data_frame = TRUE) %>%
        filter(date == max(date, na.rm = TRUE)) %>% # Get the most recent forecast date
        mutate(across(contains("intake_q_swe_pred"), ~ round(.x, 2))) %>%
        filter(date_24h <= Sys.Date() + days(10)) #Limit to the next 10 days
      
      # Extract the forecast creation date for the title
      forecast_date <- unique(intake_cached_data$date)[1]
      
      # Define RGBA colors
      col_red    <- 'rgba(255, 0, 0, 0.2)'
      col_orange <- 'rgba(255, 165, 0, 0.2)'
      col_green  <- 'rgba(0, 255, 0, 0.2)'
      col_blue   <- 'rgba(0, 0, 255, 0.2)'
      
      # Reference lines
      ref_lines <- c(2, 4, 8)
      hline_shapes <- lapply(ref_lines, function(y_val) {
        list(
          type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y_val, y1 = y_val, yref = "y",
          line = list(color = "rgba(0, 0, 0, 0.4)", width = 1.5, dash = "dash")
        )
      })
      
      # Create plot
      p <- plot_ly(intake_cached_data, x = ~date_24h) %>%
        add_ribbons(ymin = ~intake_q_swe_pred_q75, ymax = ~intake_q_swe_pred_max,
                    fillcolor = col_red, line = list(color = 'transparent'),
                    showlegend = FALSE, hoverinfo = "none") %>%
        add_ribbons(ymin = ~intake_q_swe_pred, ymax = ~intake_q_swe_pred_q75,
                    fillcolor = col_orange, line = list(color = 'transparent'),
                    showlegend = FALSE, hoverinfo = "none") %>%
        add_ribbons(ymin = ~intake_q_swe_pred_q25, ymax = ~intake_q_swe_pred,
                    fillcolor = col_green, line = list(color = 'transparent'),
                    showlegend = FALSE, hoverinfo = "none") %>%
        add_ribbons(ymin = ~intake_q_swe_pred_min, ymax = ~intake_q_swe_pred_q25,
                    fillcolor = col_blue, line = list(color = 'transparent'),
                    showlegend = FALSE, hoverinfo = "none") %>%
        add_lines(
          y = ~intake_q_swe_pred,
          line = list(color = "black", width = 2.5),
          name = "Median Prediction",
          text = ~paste0(
            "Max: ", intake_q_swe_pred_max, " mg/L<br>",
            "Q75: ", intake_q_swe_pred_q75, " mg/L<br>",
            "Median: ", intake_q_swe_pred, " mg/L<br>",
            "Q25: ", intake_q_swe_pred_q25, " mg/L<br>",
            "Min: ", intake_q_swe_pred_min, " mg/L"
          ),
          hovertemplate = "%{text}<extra></extra>"
        ) %>%
        layout(
          title = list(
            text = paste0("Forecast Created: ", forecast_date, " 3:00 AM MT"),
            x = 0.5,
            y = 0.95
          ),
          xaxis = list(title = "Date"),
          yaxis = list(
            title = "Predicted Intake TOC (mg/L)",
            range = c(min(intake_cached_data$intake_q_swe_pred_min) - 0.2, max(intake_cached_data$intake_q_swe_pred_max) + 0.2)
          ),
          shapes = hline_shapes,
          hovermode = "x unified",
          legend = list(orientation = 'h', y = -0.2),
          margin = list(t = 30, b = 30, l = 50, r = 20)
        )
      
      p %>% config(displayModeBar = FALSE)
    })
    
    return(list(
      start_sync = reactive({ input$start_sync }),
      set_status = function(step, status) { sync_status[[step]] <- status },
      full_sync_done = reactive({ sync_status$all_done }),
      snapshot_ready = reactive({ !is.null(snapshot_data()) })
    ))
  })
}
