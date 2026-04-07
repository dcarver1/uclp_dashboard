# R/home_module.R

#### Home Module UI ####
home_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "home",
          shinyjs::useShinyjs(),
          fluidRow(
            box(
              title = "System Initialization", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 4,
              
              p("The map currently shows the latest available snapshot. Initialize full data streams to access historical trends, forecasts, and detailed analytics."),
              br(),
              
              # The trigger button (wrapped in ns() for the module)
              actionBttn(ns("start_sync"), "Initialize Full Data", style = "jelly", color = "primary", icon = icon("download")),
              br(), br(),
              
              # Where our dynamic checklist will render (wrapped in ns())
              uiOutput(ns("sync_checklist"))
            ),
            box(
              title = "Latest Water Quality Snapshot",
              status = "primary",
              solidHeader = TRUE,
              width = 8,
              leafletOutput(ns("home_map"), height = "600px") %>% withSpinner()
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
      # Load the cached data as our initial snapshot
      # In a production environment, this might be a pre-filtered 'latest_only.parquet'
      github_link <- "data/data_backup.parquet"
      if (file.exists(github_link)) {
        df <- arrow::read_parquet(github_link, as_data_frame = TRUE)
        # Filter for just the latest snapshot to keep the map responsive
        latest_snapshot <- df %>%
          group_by(site, parameter) %>%
          filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
          ungroup()
        snapshot_data(latest_snapshot)
      }
    })
    
    # 1. Track the current step (0 means hasn't started)
    sync_step <- reactiveVal(0)
    
    # 2. Track the status of each specific data source
    sync_status <- reactiveValues(
      cached_data = "done", # Already loaded for snapshot
      wet_api = "pending",
      hydrovu_api = "pending",
      contrail_api = "pending",
      all_done = FALSE
    )
    
    # Visual checks for seeing if the data sets have been downloaded
    output$sync_checklist <- renderUI({
      
      # Helper function to generate the right icon based on status
      get_icon <- function(status) {
        if (status == "pending") return(icon("circle", class = "text-muted"))
        if (status == "loading") return(icon("spinner", class = "fa-spin text-primary"))
        if (status == "done") return(icon("check-circle", class = "text-success"))
      }
      
      # Build the checklist UI
      tagList(
        h4(get_icon(sync_status$cached_data), " Snapshot Data Loaded"),
        h4(get_icon(sync_status$wet_api), " Pulling WET API (SFM, CHD, PFAL)"),
        h4(get_icon(sync_status$hydrovu_api), " Pulling HydroVu API (PBD)"),
        h4(get_icon(sync_status$contrail_api), " Pulling Contrail API (FC Sites)")
      )
    })
    
    # When the user clicks the button, start the API sync
    observeEvent(input$start_sync, {
      sync_status$wet_api <- "loading"
      
      # Yield to the UI for 0.1 seconds so it can draw the spinner, THEN start step 2
      later::later(function() {
        sync_step(2) # Skipping step 1 (cached data) as it's already used for snapshot
      }, delay = 0.1)
    })
    
    # The Daisy-Chain Observer
    observeEvent(sync_step(), {
      req(sync_step() > 0) # Don't run if it's 0
      
      if (sync_step() == 2) {
        # WET API
        Sys.sleep(2)

        sync_status$wet_api <- "done"
        sync_status$hydrovu_api <- "loading"
        later::later(function() { sync_step(3) }, delay = 0.1)

      } else if (sync_step() == 3) {
        # HydroVu API
        Sys.sleep(2)

        sync_status$hydrovu_api <- "done"
        sync_status$contrail_api <- "loading"
        later::later(function() { sync_step(4) }, delay = 0.1)

      } else if (sync_step() == 4) {
        # Contrail API
        Sys.sleep(2)

        # Final cleanup and combining data
        sync_status$contrail_api <- "done"
        sync_status$all_done <- TRUE

        # Update the global loaded_data with the full dataset if needed
        # For now, we'll just signal completion
        
        # Show a success message
        showNotification("All data streams initialized successfully!", type = "message")
      }
    })
    
    # Render Home Map with either Snapshot or Full Data
    output$home_map <- renderLeaflet({
      
      # Define a comfortable default bounding box around the watershed for the empty map
      default_lng1 <- -106.5
      default_lat1 <- 40.0
      default_lng2 <- -104.0
      default_lat2 <- 41.5
      
      # Determine which data to use: Full Data if available, otherwise Snapshot
      data_to_use <- if (!is.null(loaded_data()) && nrow(loaded_data()) > 0) {
        loaded_data()
      } else {
        snapshot_data()
      }
      
      # Initial empty map if no data at all
      if (is.null(data_to_use) || nrow(data_to_use) == 0) {
        # Default view of the watershed with restricted bounds and zoom
        m <- leaflet(options = leafletOptions(minZoom = 8)) %>%
          addTiles() %>%
          setView(lng = -105.3, lat = 40.6, zoom = 9) %>%
          setMaxBounds(lng1 = default_lng1, lat1 = default_lat1, lng2 = default_lng2, lat2 = default_lat2) %>%
          addControl("Loading snapshot...", position = "topright")
        return(m)
      }
      
      # Define the parameter you want to drive the map colors
      target_param <- "Temperature" 
      
      # Find the latest reading for each site/parameter
      latest_readings <- data_to_use %>%
        mutate(DT_round_MT = with_tz(DT_round, tzone = "America/Denver")) %>%
        group_by(site, parameter) %>%
        filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()
      
      # Pivot so we have one row per site with text strings for the popups
      snapshot_wide <- latest_readings %>%
        select(site, DT_round_MT, parameter, mean, units) %>%
        # Combine value and units for display
        mutate(display_val = paste0(round(mean, 2), " ", units)) %>%
        select(-mean, -units) %>%
        pivot_wider(
          names_from = parameter, 
          values_from = display_val
        ) 
      
      # Grab the raw numeric value for our target mapping parameter
      target_numeric <- latest_readings %>%
        filter(parameter == target_param) %>%
        select(site, numeric_val = mean)
      
      # Load spatial coordinates
      locations <- read_csv("data/sonde_location_metadata.csv", show_col_types = FALSE) %>%
        separate(col = "lat_long", into = c("lat", "lon"), sep = ",", convert = TRUE) %>%
        mutate(
          site = tolower(Site),
          site = ifelse(site %in% c("pman", "pbr"), paste0(site, "_fc"), site)
        ) %>%
        select(site, Site_Name = Site, lat, lon, watershed)
      
      # Join spatial data with our snapshot strings AND numeric target data
      map_data <- locations %>%
        inner_join(snapshot_wide, by = "site") %>%
        left_join(target_numeric, by = "site") %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      # Build HTML for popups dynamically
      exclude_cols <- c("site", "DT_round_MT", "Site_Name", "color", "lat", "lon", "watershed", "geometry", "Site")
      param_cols <- setdiff(names(snapshot_wide), exclude_cols)
      
      map_data$popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
        row <- map_data[i, ]
        popup <- paste0(
          "<b>Site:</b> ", row$Site_Name, "<br>",
          "<b>Latest Reading:</b> ", format(row$DT_round_MT, "%Y-%m-%d %H:%M"), "<br><hr>",
          "<b>Current Conditions:</b><br>"
        )
        for (param in param_cols) {
          val <- row[[param]]
          if (!is.na(val) && val != "NA NA") {
            popup <- paste0(popup, "<i>", param, ":</i> ", val, "<br>")
          }
        }
        return(HTML(popup))
      })
      
      pal <- colorNumeric(palette = "viridis", domain = map_data$numeric_val, na.color = "#a9a9a9")
      bbox <- st_bbox(map_data)
      pad <- 0.15
      
      leaflet(map_data, options = leafletOptions(minZoom = 8)) %>%
        addTiles() %>%
        setMaxBounds(
          lng1 = bbox[["xmin"]] - pad, lat1 = bbox[["ymin"]] - pad, 
          lng2 = bbox[["xmax"]] + pad, lat2 = bbox[["ymax"]] + pad
        ) %>%
        addCircleMarkers(
          radius = 10, color = "#333333", weight = 1.5,
          fillColor = ~pal(numeric_val), fillOpacity = 0.9,
          popup = ~popup_content, label = ~Site_Name,
          labelOptions = labelOptions(noHide = FALSE, textsize = "14px")
        ) %>%
        addLegend(
          position = "bottomright", pal = pal,
          values = ~numeric_val[!is.na(numeric_val)], 
          title = paste("Latest", target_param), opacity = 1
        )
    })
    
    # Return a list of reactives for the main server to use
    return(list(
      full_sync_done = reactive({ sync_status$all_done }),
      snapshot_ready = reactive({ !is.null(snapshot_data()) })
    ))
  })
}