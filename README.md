# uclp_dashboard
Independant repository for development of the Upper CLP DSS dashboard and GHA for auto QAQC

## File/Folder Structure

### `ui.R`, `server.R` and `global.R`
These files are used to render and run the Shiny App at: https://geocentroid.shinyapps.io/UCLP_WQ_Alpha_Dashboard/ 
Contact Sam Struthers for username/password

### R
This folder is sourced by the shiny app and should only include scripts needed to run the shiny app. We will transition this folder to contain a file per section of the shiny app to reduce code in the `server.R` and `ui.R` files. 

### data
This folder contains all metadata, models, and cached data created by Github Actions Workflows. 

### gha_scripts

This folder contains three R scripts used in `.github/workflows` to run hourly and daily Github Actions Workflows. 

### .github

This folder contains two scripts used to run hourly and daily Github Actions Workflows.

### creds
This folder provides templates for each of the API creds needed to run this shiny app or workflows. Contact Sam Struthers or Juan De La Torre if you need access

