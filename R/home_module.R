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
                height: calc(100vh - 80px);
                margin: -15px;
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
                                          "Specific Conductivity", "Chl-a Fluorescence", "Depth"),
                              selected = "FDOM Fluorescence",
                              options = list(`style` = "btn-primary")),
                  
                  hr(),
                  
                  # Initialization Control
                  p(tags$small("The map shows the latest available snapshot. Initialize full data streams for historical trends.")),
                  actionBttn(ns("start_sync"), "Initialize Data", style = "jelly", color = "primary", icon = icon("download"), size = "sm"),
                  br(), br(),
                  uiOutput(ns("sync_checklist"))
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
          p(get_icon(sync_status$contrail_api), " Pulling Contrail API")
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
        return(leaflet() %>% addTiles() %>% setView(lng = -105.3, lat = 40.6, zoom = 9))
      }
      
      latest_readings <- data_to_use %>%
        mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
        group_by(site, parameter) %>%
        filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()

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
      
      param_cols <- setdiff(names(snapshot_wide), c("site", "DT_round_MT", "Site_Name"))
      
      map_data$popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
        row <- map_data[i, ]
        popup <- paste0(
          "<div style='min-width: 200px;'>",
          "<b>Site:</b> ", row$Site_Name, "<br>",
          "<b>Latest:</b> ", format(row$DT_round_MT, "%Y-%m-%d %H:%M"), "<br><hr>",
          "<table style='width: 100%;'>"
        )
        for (param in param_cols) {
          val <- row[[param]]
          if (!is.na(val) && val != "NA NA") {
            style <- if(param == target_param) "style='font-weight: bold; color: #E70870;'" else ""
            popup <- paste0(popup, "<tr><td ", style, ">", param, ":</td><td ", style, " align='right'>", val, "</td></tr>")
          }
        }
        popup <- paste0(popup, "</table></div>")
        return(HTML(popup))
      })
      
      pal <- colorNumeric(palette = "viridis", domain = map_data$numeric_val, na.color = "#a9a9a9")
      
      leaflet(map_data) %>%
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
    
    return(list(
      start_sync = reactive({ input$start_sync }),
      set_status = function(step, status) { sync_status[[step]] <- status },
      full_sync_done = reactive({ sync_status$all_done }),
      snapshot_ready = reactive({ !is.null(snapshot_data()) })
    ))
  })
}