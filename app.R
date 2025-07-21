# ELAN Dashboard App
# This is the main app file that uses the modular dashboard structure

# Load required libraries
library(shiny)
library(leaflet)
# library(RSocrata)  # Removed - not used and may cause WebR compatibility issues
library(dplyr)
library(bslib)
library(bsicons)
library(sf)
library(magrittr)
# library(geojsonio)  # Removed - not needed, using sf::st_write for GeoJSON
library(htmltools)
library(htmlwidgets)
# library(stringi)  # Removed - not used and may cause WebR compatibility issues
library(RColorBrewer)
# library(reactlog)  # Removed - not used and may cause WebR compatibility issues
library(ggplot2)
library(ggiraph)
library(tidyverse)
library(foreach)
# library(RCurl)  # Removed - not used and may cause WebR compatibility issues
library(DT)
# library(shinyBS)  # Removed - not used and may cause WebR compatibility issues
library(shinyjs)
library(readxl)
library(rmarkdown)
library(knitr)
library(kableExtra)

# Source the UI and server files
source("ui.R")
source("server.R")

options(shiny.sanitize.errors = FALSE)

# Run the Shiny app
shinyApp(ui = ui, server = server) 