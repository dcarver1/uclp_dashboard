# R/home_module.R

#### Home Module UI ####
home_ui <- function(id) {
  ns <- NS(id)
  
  tabItem(tabName = "home",
          fluidRow(
            box(
              title = "System Initialization", 
              status = "primary", 
              solidHeader = TRUE, 
              width = 4,
              
              p("The dashboard pools high-resolution data from multiple watershed APIs. Please initialize the data streams to begin."),
              br(),
              
              # The trigger button (wrapped in ns() for the module)
              actionBttn(ns("start_sync"), "Initialize Data Streams", style = "jelly", color = "primary", icon = icon("download")),
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
    
    # 1. Track the current step (0 means hasn't started)
    sync_step <- reactiveVal(0)
    
    # 2. Track the status of each specific data source
    sync_status <- reactiveValues(
      cached_data = "pending",
      wet_api = "pending",
      hydrovu_api = "pending",
      contrail_api = "pending"
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
        h4(get_icon(sync_status$cached_data), " Reading Cached Parquet Data"),
        h4(get_icon(sync_status$wet_api), " Pulling WET API (SFM, CHD, PFAL)"),
        h4(get_icon(sync_status$hydrovu_api), " Pulling HydroVu API (PBD)"),
        h4(get_icon(sync_status$contrail_api), " Pulling Contrail API (FC Sites)")
      )
    })
    
    # When the user clicks the button, start Step 1
    observeEvent(input$start_sync, {
      sync_status$cached_data <- "loading"
      
      # Yield to the UI for 0.1 seconds so it can draw the spinner, THEN start step 1
      later::later(function() {
        sync_step(1)
      }, delay = 0.1)
    })
    
    # The Daisy-Chain Observer
    observeEvent(sync_step(), {
      req(sync_step() > 0) # Don't run if it's 0
      
      if (sync_step() == 1) {
        #Read in cached data which has been passed through autoQAQC
        github_link <- "data/data_backup.parquet" #remove on live version and use ^ github link instead ^
        cached_data <- arrow::read_parquet(github_link, as_data_frame = TRUE)
        
        # Simulate time taken to read data
        Sys.sleep(2) 
        
        # Mark done, set next to loading, trigger Step 2
        sync_status$cached_data <- "done"
        sync_status$wet_api <- "loading"
        later::later(function() { sync_step(2) }, delay = 0.1) 
        
      } else if (sync_step() == 2) {
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
        
        # Show a success message
        showNotification("All data streams initialized successfully!", type = "message")
      }
    })

    # Render Home Map with latest snapshots
    output$home_map <- renderLeaflet({
      # Initial empty map before data loads
      if (is.null(loaded_data()) || nrow(loaded_data()) == 0) {
        # Default view of the watershed
        m <- leaflet() %>%
          addTiles() %>%
          setView(lng = -105.3, lat = 40.6, zoom = 9) %>%
          addControl("Please click 'Initialize Data Streams' to load site locations and data.", position = "topright")
        return(m)
      }
      
      # Process the snapshot data
      data <- loaded_data()
      
      # Find the latest reading for each site/parameter
      latest_readings <- data %>%
        group_by(site, parameter) %>%
        filter(DT_round == max(DT_round, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup()
        
      # Pivot so we have one row per site with columns for each parameter
      snapshot_wide <- latest_readings %>%
        select(site, DT_round_MT, parameter, mean, units) %>%
        # Combine value and units for display
        mutate(display_val = paste0(round(mean, 2), " ", units)) %>%
        select(-mean, -units) %>%
        pivot_wider(
          names_from = parameter, 
          values_from = display_val
        ) %>%
        left_join(site_table, by = c("site" = "site_code"))

      # Load spatial coordinates
      # (Using the same logic as the Site Map tab)
      locations <- read_csv("data/sonde_location_metadata.csv", show_col_types = FALSE) %>%
        separate(col = "lat_long", into = c("lat", "lon"), sep = ",", convert = TRUE) %>%
        mutate(
          site = tolower(Site),
          site = ifelse(site %in% c("pman", "pbr"), paste0(site, "_fc"), site)
        ) %>%
        select(site, lat, lon, watershed)
        
      # Join spatial data with our snapshot data
      map_data <- locations %>%
        inner_join(snapshot_wide, by = "site") %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326)

      # Build HTML for popups dynamically based on available columns
      # Exclude non-parameter columns from the stats list
      exclude_cols <- c("site", "DT_round_MT", "site_name", "color", "lat", "lon", "watershed", "geometry", "Site")
      param_cols <- setdiff(names(snapshot_wide), exclude_cols)

      map_data$popup_content <- lapply(seq_len(nrow(map_data)), function(i) {
        row <- map_data[i, ]
        
        # Build the HTML popup
        popup <- paste0(
          "<b>Site:</b> ", row$site_name, "<br>",
          "<b>Latest Reading:</b> ", format(row$DT_round_MT, "%Y-%m-%d %H:%M"), "<br><hr>",
          "<b>Current Conditions:</b><br>"
        )
        
        # Add available parameters
        for (param in param_cols) {
          val <- row[[param]]
          if (!is.na(val) && val != "NA NA") {
            popup <- paste0(popup, "<i>", param, ":</i> ", val, "<br>")
          }
        }
        
        return(HTML(popup))
      })

      # Render the populated map
      leaflet(map_data) %>%
        addTiles() %>%
        addCircleMarkers(
          radius = 8,
          color = ~color,
          fillOpacity = 0.8,
          popup = ~popup_content,
          label = ~site_name,
          labelOptions = labelOptions(noHide = FALSE, textsize = "14px")
        )
    })
    
    # Return a reactive value indicating if the sync is completely finished
    # This will be useful later when you want to pass the pooled data back to the main server
    return(reactive({ sync_status$contrail_api == "done" }))
  })
}
