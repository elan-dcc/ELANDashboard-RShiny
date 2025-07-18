# Variables Definition Panel
# This file contains the UI for the Variables Definition page

variables_definition_panel <- function() {
  nav_panel(uiOutput("variables_definition_panel_title"),
    div(
      class = "page-container",
      h1("Variables Definition", class = "page-heading"),
      div(
        class = "page-description",
        p("This page provides detailed definitions of all variables used in the ELAN Dashboard. Use the search and filter options to find specific variables.")
      ),
      div(
        class = "table-container",
        DT::dataTableOutput("variables_definition_table", width = "100%")
      )
    )
  )
} 