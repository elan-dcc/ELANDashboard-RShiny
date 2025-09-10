# ELAN Dashboard App
# This is the main app file that uses the modular dashboard structure

# Load required libraries with error handling for shinylive compatibility
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

# Load dependencies that might be missing for shinylive with error handling
tryCatch({
  library(munsell)  # Required by ggiraph
  library(scales)   # Used in Rmd files and ggplot2
  library(viridisLite)  # Color scales dependency
  library(farver)   # Color handling dependency
  library(labeling) # ggplot2 dependency
  library(isoband)  # ggplot2 dependency
  library(gtable)   # ggplot2 dependency
  library(grid)     # ggplot2 dependency
}, error = function(e) {
  warning("Some dependencies could not be loaded: ", e$message)
})

tryCatch({
  library(ggiraph)
}, error = function(e) {
  warning("ggiraph could not be loaded: ", e$message)
})

library(tidyverse)
library(foreach)
library(DT)
library(shinyjs)

# Load packages that might not be available in WebAssembly
tryCatch({
  library(readxl)
}, error = function(e) {
  warning("readxl could not be loaded: ", e$message)
})

tryCatch({
  library(rmarkdown)
  library(knitr)
  library(kableExtra)
}, error = function(e) {
  warning("Some markdown packages could not be loaded: ", e$message)
})

# Additional tidyverse components that might be needed
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(readr)

# Source panel files at the top level
source("ui_panels/load_panels.R")

# Source dashboard components
source("dashboard/global_dashboard.R")
source("dashboard/ui_dashboard.R")
source("dashboard/language_system.R")

# Source the UI and server files
source("ui.R")
source("server.R")

options(shiny.sanitize.errors = FALSE)

# Run the Shiny app
shinyApp(ui = ui, server = server)
