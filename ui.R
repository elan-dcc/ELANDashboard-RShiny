## app.R ##
# install.packages("bsicons")
library(shiny)    # for shiny apps
library(leaflet)  # renderLeaflet function
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
library(shinyBS)
library(ggiraph)
library(shinyjs)


# Source panel files at the top level
source("ui_panels/load_panels.R")

# Source dashboard components
source("dashboard/global_dashboard.R")
source("dashboard/ui_dashboard.R")
source("dashboard/language_system.R")

ui <- page_fluid(
  useShinyjs(),
  includeCSS("www/style.css"),
  tags$head(
    tags$script(src = "script.js"),
    tags$style(HTML("
      .main-content-container {
        max-width: 1400px !important;
        margin: 0 auto !important;
        padding: 0 20px !important;
      }
      body {
        min-width: 1200px !important;
      }
    "))
  ),

  page_navbar(
  title = div(
    tags$a(href = "https://healthcampusdenhaag.nl/nl/", target = "_blank",
      img(src="img/hc-dh-logo.svg")
    ),
  ),
  window_title = "ELAN Dashboard",
  id = "navbar",
  selected = "ELAN Dashboard",
  fluid = T,

  dashboard_ui(),
  # Add all panels from external files
  variables_definition_panel(),
  data_dictionary_panel(),
  change_log_panel(),
  about_us_panel(),
  # Language toggle button - positioned at the end of navbar
  nav_item(
    div(
      style = "margin-left: 10px;",
      actionButton(
        inputId = "language_selector",
        label = textOutput("language_button_text"),
        style = "color: white !important; background-color: #007CC2 !important; border: none !important;"
      )
    )
  )
  
),
# Main content container
div(
  class = "main-content-container",
  # Floating scroll navigation button - moved outside page_navbar
  div(
    class = "scroll-nav-button",
    div(
      class = "scroll-up-area",
      onclick = "window.scrollTo({top: 0, behavior: 'smooth'});",
      "UP"
    ),
    div(
      class = "scroll-down-area", 
      onclick = "window.scrollTo({top: document.body.scrollHeight, behavior: 'smooth'});",
      "DOWN"
  )
)
),
# Footer
div(
  class = "footer-container",
  div(
    class = "footer",
    # First row: three columns (logo+address, main sponsors, links)
    div(
      class = "footer-row-1",
      # Column 1: Logo + Address
      div(
        class = "footer-logo",
        tags$a(href = "https://healthcampusdenhaag.nl/nl/", target = "_blank",
          img(src="img/hc-dh-logo.svg")
        )
      ),
      div(
        class = "footer-col-1",
        
        div(
          class = "footer-address",
          p(""),
          p("Health Campus The Hague"),
          p("Turfmarkt 99, 3rd Floor"),
          p("2511 DP Den Haag")
        ),
        div(
          class = "footer-copyright",
          p(uiOutput("last_updated_text")),
          p(uiOutput("copyright_text"))
        )
      ),
      # Column 2: Main Sponsors (LUMC and Universiteit Leiden)
      div(
        class = "footer-col-2",
        # div(
        #   class = "main-sponsors-title",
        #   p("Powered by           ")
        # ),
        div(
          class = "main-sponsors",
          div(
            class = "main-sponsor-logo",
            tags$a(href = "https://healthcampusdenhaag.nl/nl/", target = "_blank",
            img(src="img/lumc.svg", alt="LUMC")
            )
          ),
          div(
            class = "main-sponsor-logo",
            tags$a(href = "https://healthcampusdenhaag.nl/nl/", target = "_blank",
            img(src="img/universiteit_leiden.svg", alt="Universiteit Leiden")
            )
          )
        )
      ),
      # Column 3: Footer Links
      div(
        class = "footer-col-3",
      div(
        class = "footer-links",
          tags$a(href = "#", onclick = "document.querySelector('[data-value=\"About Us\"]').click();", uiOutput("about_us_link")),
          tags$a(href = "#", onclick = "document.querySelector('[data-value=\"Change Log\"]').click();", uiOutput("change_log_link")),
        tags$a(href = "#", onclick = "Shiny.setInputValue('show_variables', Math.random());", uiOutput("variables_explanation_link")),
          tags$a(href = "#", onclick = "document.querySelector('[data-value=\"Variables Definition\"]').click();", uiOutput("variables_definition_link")),
          tags$a(href = "#", onclick = "document.querySelector('[data-value=\"Data Dictionary\"]').click();", uiOutput("data_sources_link"))
        )
      )
    ),
    # Second row: Partner logos
    div(
      class = "footer-row-2",
      div(
        class = "partners-title",
        p(uiOutput("partners_title_text"))
      ),
      div(
        class = "partners-logos",
        div(
          class = "partner-logo",
          img(src="img/Partner/logo1-rgb.svg", alt="Partner 1")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/hhs_nl_groen_fc-2018.svg", alt="Partner 2")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/logo lumc_PMS_NL.svg", alt="Partner 3")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/hadoks_logo.svg", alt="Partner 4")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/PAR_Groep+po_line_01_CMYK_FC.svg", alt="Partner 5")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/UL - Algemeen - RGB-Kleur.svg", alt="Partner 6")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/Haga_logo.svg", alt="Partner 7")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/Compact_Logo_gemeente_Den_Haag.svg", alt="Partner 8")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/GGD_logo.svg", alt="Partner 9")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/HMC_logo.svg", alt="Partner 10")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/hc-dh-logo.svg", alt="Partner 11")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/logo1-rgb.svg", alt="Partner 1")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/hhs_nl_groen_fc-2018.svg", alt="Partner 2")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/logo lumc_PMS_NL.svg", alt="Partner 3")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/hadoks_logo.svg", alt="Partner 4")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/PAR_Groep+po_line_01_CMYK_FC.svg", alt="Partner 5")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/UL - Algemeen - RGB-Kleur.svg", alt="Partner 6")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/Haga_logo.svg", alt="Partner 7")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/Compact_Logo_gemeente_Den_Haag.svg", alt="Partner 8")
        ),
        div(
          class = "partner-logo",
          img(src="img/Partner/GGD_logo.svg", alt="Partner 9")
      ),
      div(
          class = "partner-logo",
          img(src="img/Partner/HMC_logo.svg", alt="Partner 10")
      ),
      div(
          class = "partner-logo",
          img(src="img/Partner/hc-dh-logo.svg", alt="Partner 11")
        )
      )
    )
  )
)
)
  
  
