#### Global Variables ####

#Prototype dashboard
# Fixed package loading for shinyapps.io deployment
# Remove the custom package_loader function and install.packages() calls

# Simply load required packages - shinyapps.io will automatically detect and install them
## commiting out libraries that are not used 
## remove here, scales, and glue as there were not called oftem and easily replaced. 
suppressMessages({
  #library(furrr)
  library(ross.wq.tools)
  # Date/time handling
  library(zoo)
  library(padr)
  # Data cleaning and utilities
  # library(janitor)
  # library(broom)
  # Stats/modeling
  library(stats)
  # library(RcppRoll)
  # library(trend)
  library(xgboost)
  # Spatial packages
  library(sf)
  library(leaflet)
  # Vis
  # library(ggpubr)
  # library(ggthemes)
  library(plotly)
  # library(ggpmisc)
  # Web scraping/data retrieval
  library(rvest)
  # library(httr)
  library(httr2)
  library(cdssr)
  library(yaml)
  # Development tools
  library(devtools) # this might be needed as ross.wq and cddsr are not on cran 
  # Shiny
  library(shiny)
  library(shinymanager) # maybe? 
  library(shinycssloaders)
  # library(shinyTime)
  library(bslib)
  library(shinyWidgets)
  library(shinydashboard)
  library(htmltools)
  library(readr)
  # Core data manipulation
  library(tidyverse)
  library(DT)
  library(purrr)
  library(data.table)
  library(arrow)
})

#### Set up ####

options(shiny.maxRequestSize = 10000 * 1024^2)

#negate %in% call for easier filtering
`%nin%` = Negate(`%in%`)

#set consistent site colors and names
site_table <- tibble(site_code = c("sfm", "chd", "pfal", "pbd", "pbr_fc", "pman_fc"),
                     site_name = c("South Fork CLP", "Chambers Lake Outflow", "CLP at Poudre Falls", "Canyon Mouth", "CLP at Indian Meadows", "CLP at Manners Bridge"),
                     color = c("#002EA3", "#E70870", "#256BF5", "#1E4D2B", "#56104E", "#FFCA3A"))

#CDWR sites we are interested in
cdwr_lookup_table <- tibble(
  site_abbrev = c(   "LAPLODCO", "JOEBELCO", "JWCCHACO", "CLANSECO", "CLANLICO", "NPRCANCO", #Upper CLP Basin sites
                     "MUNCANCO","CLANHACO", "NOCALACO", "CLASRKCO", "CLAFTCCO", #Upper CLP Basin sites
                     "HOROUTCO", "HSCCLPCO", #Lower CLP Diversions (Horsetooth)
                     "LAPTUNCO", "CAPDCPCO" #laramie river basin
  ),
  site_title = c("Long Draw Outflow", "Joe Wright Reservoir Outflow", "Chambers Lake Outflow", "NF below Seaman", "NF at Livermore", "North Poudre Canal", #Upper CLP Basin sites
                 "Munroe Canal","NF above Halligan", "NF below Halligan", "South Fork CLP", "Canyon Mouth", #Upper CLP Basin sites
                 "Horsetooth Outflow", "Hansen Supply Canal to Poudre", #Lower CLP Diversions (Horsetooth)
                 "Laramie River Tunnel", "Michigan Ditch" #laramie river basin
  ))

#Just Isolate the site abbrevs
cdwr_upper_clp_sites <- cdwr_lookup_table%>%pull(site_abbrev)


cdwr_api_key <- read_yaml("creds/CDWRCreds.yml")$api_key

water_chem <- read_parquet("data/chem/ROSS_FC_water_chemistry_20251114.parquet")

#Parameter plot bounds
plot_param_table <- tibble(
  parameter = c("Temperature", "Turbidity", "pH", "DO",
                 "Specific Conductivity", "Chl-a Fluorescence", "FDOM Fluorescence", "Depth",
                "TOC"),
  lower = c(10, 0.1, 6.5, 6, 20, 0.1, 0.1, 0.1, 2),
  upper = c(20, 40, 9, 10, 60, 1, 1, 2, 5),
  units = c("°C", "NTU", "", "mg/L", "µS/cm", "RFU", "RFU", "ft", "mg/L")
)
toc_model_bounds <- water_chem%>%
  summarise(TOC_lower  = min(TOC, na.rm = T),
            TOC_upper = max(TOC, na.rm = T))

toc_forecast_sites <- read_csv("data/toc_forecast_location_metadata.csv", show_col_types = F)%>%filter(model_version == "Distributed")

