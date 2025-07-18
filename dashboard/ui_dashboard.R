# Main Dashboard UI
# This file combines the Wijken and Gemeente tabs into the main dashboard

# Source the UI components
source("dashboard/components/wijken/ui_wijken.R")
source("dashboard/components/gemeente/ui_gemeente.R")

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