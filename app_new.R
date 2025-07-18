# ELAN Dashboard App
# This is the main app file that uses the modular dashboard structure

# Load required libraries
library(shiny)
library(leaflet)
library(RSocrata)
library(dplyr)
library(bslib)
library(bsicons)
library(sf)
library(magrittr)
library(geojsonio)
library(htmltools)
library(htmlwidgets)
library(stringi)
library(RColorBrewer)
library(reactlog)
library(ggplot2)
library(ggiraph)
library(tidyverse)
library(foreach)
library(RCurl)
library(DT)
library(shinyBS)
library(shinyjs)
library(readxl)

# Source the UI and server files
source("ui.R")
source("server_new.R")

options(shiny.sanitize.errors = FALSE)

# Run the Shiny app
shinyApp(ui = ui, server = server) 