# install.packages("pacman")

pacman::p_load(
  # ross.wq.tools,
  
  # Date/time handling
  zoo, padr,
  # not luburdate? 
  
  # Data cleaning and utilities
  # this is a lot of cleaning steps, why can't they be done before, specific here, seems of given the container nature for shiny 
  
  # Stats/modeling
  stats, RcppRoll, trend, scales, xgboost,
  # is there statistically modeling going on within the shiny app? 
  
  # Spatial packages
  sf, leaflet,
  
  # Vis
  ggpubr, ggthemes, plotly, ggpmisc,
  # ggplot and plotly? 
  
  # Web scraping/data retrieval
  rvest, httr, httr2, cdssr, yaml,
  
  # Development tools
  devtools,
  
  # Shiny
  shiny, shinymanager, shinycssloaders, shinyTime, bslib, shinyWidgets, shinydashboard, htmltools, readr,
  # what is shiny manager, shiny time, shinywidgets, shinydashboard, doing? 
  
  # Core data manipulation
  tidyverse, DT, purrr, data.table, arrow
  # why the whole tidyverse, 
  # why DT and data.table? 

)



devtools::install_github("anguswg-ucsb/cdssr")
devtools::install_github("rossyndicate/ross.wq.tools")
