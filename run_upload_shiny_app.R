#library(shiny)
# Run the application
shiny::shinyAppDir("dashboard/")




#Upload app to GeoSpatial Centroid Account

rsconnect::deployApp(appDir = "dashboard/", account = "geocentroid", appName = "UCLP_WQ_Dashboard")
