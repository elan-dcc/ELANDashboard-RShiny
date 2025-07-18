# Main Dashboard Server Logic
# This file combines the server logic for both Wijken and Gemeente tabs

# Source the server components
source("dashboard/components/wijken/server_wijken.R")
source("dashboard/components/gemeente/server_gemeente.R")

# Main dashboard server function
dashboard_server <- function(input, output, session, current_language) {
  
  # Call the individual server functions with language support
  wijken_server(input, output, session, current_language)
  gemeente_server(input, output, session, current_language)
} 