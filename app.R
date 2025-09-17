# ELAN Dashboard App
# This is the main app file for the modular dashboard structure

# 1. Load Required Libraries (Keep this section)
library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(bsicons)
library(sf)
library(magrittr)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)
library(ggplot2)
library(ggiraph)
library(tidyverse)
library(foreach)
library(DT)
library(shinyjs)
library(readxl)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(tibble)

# Load helpers
source("helpers/utils.R")
source("helpers/data_loading.R")
source("helpers/language_system.R")

# Load dashboard modules
source("modules/dashboard/mod_ui_dashboard.R")
source("modules/dashboard/mod_server_dashboard.R")

# Load tab modules
source("modules/wijken/mod_server_wijken.R")
source("modules/gemeenten/mod_server_gemeente.R")

# Load UI panels
source("panels/load_panels.R")


# Run the app

shinyApp(ui = ui, server = server) 