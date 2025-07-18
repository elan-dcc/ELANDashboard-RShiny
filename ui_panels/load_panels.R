# Load all UI panels
# This file sources all the separate panel files

# Source all panel files
source("ui_panels/variables_definition_panel.R")
source("ui_panels/data_dictionary_panel.R")
source("ui_panels/change_log_panel.R")
source("ui_panels/about_us_panel.R")

# Function to get all panels
get_all_panels <- function() {
  list(
    variables_definition_panel(),
    data_dictionary_panel(),
    change_log_panel(),
    about_us_panel()
  )
} 