# Change Log Panel
# This file contains the UI for the Change Log page

change_log_panel <- function() {
  nav_panel(uiOutput("change_log_panel_title"),
    div(
      class = "page-container",
      
      h2("Version 1 - 09/09/2025", class = "version-heading"),
      div(
        class = "section-content",
        p("1. Initial Release - R Shiny Dashboard :"),
        tags$ul(
          tags$li("This is an R Shiny version of our previous dashboard: https://ggdh-dashboard.onrender.com/"),
          tags$li("Removed \"Other Projects\" pages (Hadoks and Hartfalen) - separate dashboards will be created later"),
          tags$li("Completely overhauled visualizations and features for improved user experience"),
          tags$li("Added Municipality (Gemeente) level visualization capabilities"),
          tags$li("Implemented simple report generator for chosen areas (accessible by clicking on map or bar chart)"),
          tags$li("Updated to the latest 2024 data, with some variables still available only until 2022"),
          tags$li("Added multi-language support (Dutch/English)"),
          tags$li("Fixed are related to Nieuwkoop and Rijswijk")
        ),
        p("2. Area Coverage :"),
        tags$ul(
          tags$li("ELAN covered area :"),
          tags$ul(
            tags$li("Leiden and other : Alphen aan den Rijn, Hillegom, Kaag en Braassem, Katwijk, Leiden, Leiderdorp, Lisse, Nieuwkoop, Noordwijk, Oegstgeest, Teylingen, Voorschoten, Zoeterwoude"),
            tags$li("Delft and other: Delft, Midden-Delfland, Pijnacker-Nootdorp, Westland"),
            tags$li("Zoetermeer")
          ),
          # tags$li("Additional : Waddinxveen, Bodegraven-Reeuwijk"),
          tags$li("Hadoks Area : 's-Gravenhage, Leidschendam-Voorburg, Rijswijk, Wassenaar")
        ),
        # p("3. Available Pages and Features :"),
        # tags$ul(
        #   tags$li("Neighbourhood (Wijk) Page: Contains comprehensive infographics of past variables per neighbourhood"),
        #   tags$li("Supply and Demand Analysis: Features clustering and projection of selected variables per neighbourhood in collaboration with Hadoks"),
        #   tags$li("Opioid Analysis Page:"),
        #   tags$ul(
        #     tags$li("Available variables: Alcohol abuse, Medication abuse, Loneliness, BMI, and Opioid medication usage"),
        #     tags$li("Interactive map visualization"),
        #     tags$li("Scatter plots and line charts for trend analysis")
        #   ),
        #   tags$li("Interactive data exploration tools"),
        #   tags$li("Multi-language support (Dutch/English)"),
        #   tags$li("Responsive design for various screen sizes"),
        #   tags$li("Note: Additional theme projects/pages are still \"Work in Progress\"")
        # )
      )
    )
  )
} 