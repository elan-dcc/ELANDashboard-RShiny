# Change Log Panel
# This file contains the UI for the Change Log page

change_log_panel <- function() {
  nav_panel(uiOutput("change_log_panel_title"),
    div(
      class = "page-container",
      h1("Changelog", class = "page-heading"),
      p("This page contains what changes relatively to the last updated version", class = "version-description"),
      
      h2("Version 2.3 - 11/12/2024", class = "version-heading"),
      div(
        class = "section-content",
        p("1. Functionality add :"),
        tags$ul(
          tags$li("Add a new region on the Neighbourhood page that links to control input on the Hadoks page"),
          tags$li("Add control panel session save in region input")
        )
      ),
      
      h2("Version 2.2 - 25/11/2024", class = "version-heading"),
      div(
        class = "section-content",
        p("1. Area coverage :"),
        tags$ul(
          tags$li("Fix Zoetermeer, Lisse and Hillegom neighbourhood (Wijk)")
        ),
        p("2. Add variables related to referral info"),
        p("3. Fix JSON file read on Hadoks page (request JSON file posing some issues)")
      ),
      
      h2("Version 2.1 - 24/10/2024", class = "version-heading"),
      div(
        class = "section-content",
        p("1. Area coverage :"),
        tags$ul(
          tags$li("Change dashboard name to ELAN Dashboard"),
          tags$li("Neighbourhood / Wijk page changes :"),
          tags$ul(
            tags$li("Fix bug-related duplicated Wijk name in the drop-down list"),
            tags$li("Remove several variables to choose from to plot")
          ),
          tags$li("Add several links and contacts to \"About Us\" page")
        )
      ),
      
      h2("Version 2 - 11/08/2024", class = "version-heading"),
      div(
        class = "section-content",
        p("1. Area coverage :"),
        tags$ul(
          tags$li("ELAN covered area :"),
          tags$ul(
            tags$li("Leiden and other : Alphen aan den Rijn, Hillegom, Kaag en Braassem, Katwijk, Leiden, Leiderdorp, Lisse, Nieuwkoop, Noordwijk, Oegstgeest, Teylingen, Voorschoten, Zoeterwoude"),
            tags$li("Delft and other: Delft, Midden-Delfland, Pijnacker-Nootdorp, Westland"),
            tags$li("Zoetermeer")
          ),
          tags$li("Additional : Waddinxveen, Bodegraven-Reeuwijk"),
          tags$li("Hadoks Area : 's-Gravenhage, Leidschendam-Voorburg, Rijswijk, Wassenaar")
        ),
        p("2. Working pages are :"),
        tags$ul(
          tags$li("Neighbourhood: contains infographics of past variables per Neighbourhood"),
          tags$ul(
            tags$li("Add variable themes, to reduce the number of variables shown in the dropdown variable input"),
            tags$li("Fix the translation"),
            tags$li("Merge the variables in the Opioid page to the Nighbourhood page")
          ),
          tags$li("Supply and Demand: contains a clustering and projection of selected variables per Neighbourhood in Collaboration with Hadoks"),
          tags$ul(
            tags$li("Fix the bivariate cluster size (in the call back function)")
          ),
          tags$li("Heart failure: contains infographics of heart failure patients"),
          tags$ul(
            tags$li("Add brief infomation cards"),
            tags$li("Add basic demographics bar chart"),
            tags$li("Add multi level sunburst chart")
          ),
          tags$li("Remove \"Work in progress\" theme pages")
        )
      ),
      
      h2("Version 1 - 17/01/2024", class = "version-heading"),
      div(
        class = "section-content",
        p("1. Area coverage :"),
        tags$ul(
          tags$li("Den Haag and other : s-Gravenhage, Leidschendam-Voorburg, Rijswijk, Wassenaar"),
          tags$li("Hadoks Area : 's-Gravenhage, Leidschendam-Voorburg, Rijswijk, Wassenaar")
        ),
        p("2. Working pages are :"),
        tags$ul(
          tags$li("Neighbourhood: contains infographics of past variables per Neighbourhood"),
          tags$li("Supply and Demand: contains a clustering and projection of selected variables per Neighbourhood in Collaboration with Hadoks"),
          tags$li("Opioid page"),
          tags$ul(
            tags$li("Variables available are Alcohol abuse, Medication abuse, Loneliness, BMI, and Opioid medication user"),
            tags$li("Add a basic map chart"),
            tags$li("Add scatter and line chart")
          ),
          tags$li("Other theme projects/pages are still \"Work in Progress\"")
        )
      )
    )
  )
} 