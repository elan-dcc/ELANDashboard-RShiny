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


# Sourcing the modules

source("modules/mod_dashboard_server.R")
source("modules/mod_wijken_server.R")
source("modules/mod_gemeente_server.R")

# PLACEHOLDER - Defining the UI object 

ui <- page_fluid(
  useShinyjs(),
  includeCSS("www/css/style.css"),
  tags$head(
    tags$script(src = "www/scripts/script.js"),
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

        
#END OF UI
##########


# PLACEHOLDER - Defining the server function

server = function(input, output, session) {
  
  # Reactive value for current language
  current_language <- reactiveVal("nl")
  
  # Observe language changes - toggle between NL and EN
  observeEvent(input$language_selector, {
    if (current_language() == "nl") {
      current_language("en")
    } else {
      current_language("nl")
    }
  })
  
  # Language button text
  output$language_button_text <- renderText({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0 || lang == "nl") {
      "NL"
    } else {
      "EN"
    }
  })
  
  
  
  # Footer language outputs
  output$last_updated_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("last_updated", lang)
  })
  
  output$copyright_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("copyright", lang)
  })
  
  output$about_us_link <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("about_us", lang)
  })
  
  output$change_log_link <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("change_log", lang)
  })
  
  output$variables_explanation_link <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("variables_explanation", lang)
  })
  
  output$variables_definition_link <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("variables_definition", lang)
  })
  
  output$data_sources_link <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("data_sources", lang)
  })
  
  output$partners_title_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("partners_title", lang)
  })
  
  # Remove item info text
  output$remove_item_info_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("remove_item_info", lang)
  })
  
  # Navbar panel name outputs
  output$variables_definition_panel_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("variables_definition_panel", lang)
  })
  
  output$data_dictionary_panel_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("data_dictionary_panel", lang)
  })
  
  output$change_log_panel_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("change_log_panel", lang)
  })
  
  output$about_us_panel_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("about_us_panel", lang)
  })
  
  # Call the main dashboard server function with language support
  dashboard_server(input, output, session, current_language)
  
  # Modal triggers for footer links
  observeEvent(input$show_variables, {
    showModal(modalDialog(
      title = "Variables Explanation",
      size = "lg",
      div(
        class = "modal-content",
        p("This section provides detailed explanations of all variables used in the ELAN Dashboard."),
        p("Select a variable category to view detailed descriptions and definitions."),
        h3("Variable Categories:"),
        tags$ul(
          tags$li("Person Variables - Age, gender, ethnicity data"),
          tags$li("Household Variables - Family structure and mobility"),
          tags$li("Socioeconomic Variables - Income, employment, benefits"),
          tags$li("Healthcare Costs - Medical expenses and insurance"),
          tags$li("Medication Variables - Prescription drug usage"),
          tags$li("Primary Care Variables - GP and first-line healthcare"),
          tags$li("Secondary Care Variables - Hospital and specialist care")
        )
      ),
      footer = modalButton("Close")
    ))
  })
  
  # Data Sources page tables
  output$table_h <- DT::renderDataTable({
    DT::datatable(
      df_h,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 25, 50),
        searching = TRUE,
        ordering = TRUE
      ),
      filter = "top",
      style = "bootstrap"
    )
  })
  
  output$table_cbs <- DT::renderDataTable({
    DT::datatable(
      df_cbs,
      options = list(
        pageLength = 25,
        lengthMenu = c(5, 10, 25, 50),
        searching = TRUE,
        ordering = TRUE
      ),
      filter = "top",
      style = "bootstrap"
    )
  })
  
  # Variables Definition table
  output$variables_definition_table <- DT::renderDataTable({
    DT::datatable(
      df_label_def_nl,
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        searching = TRUE,
        ordering = TRUE
      ),
      filter = "top",
      style = "bootstrap",
      rownames = FALSE
    )
  })
} 

### END OF SERVER PORTION

# Run the app

shinyApp(ui = ui, server = server) 