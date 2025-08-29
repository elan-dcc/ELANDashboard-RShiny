# Main Dashboard UI
# This file combines the Wijken and Gemeente tabs into the main dashboard

# Source the UI modules
source("../modules/wijken/mod_ui_wijken.R")
source("../modules/gemeente/mod_ui_gemeente.R")

# Main dashboard UI function
dashboard_ui <- function() {
  nav_panel("ELAN Dashboard", icon = bsicons::bs_icon("graph-up"),
    navset_tab( id = "ELANSetTab",
      wijken_ui(),
      gemeente_ui()
    ), 
    selected = "Wijken"
  )
} 