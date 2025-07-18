## app.R ##
# install.packages("RCurl")
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
library(reactlog)
library(ggplot2)
library(ggiraph)
library(tidyverse)
library(foreach)
library(RCurl)
library(DT)

# Source dashboard components
source("dashboard/global_dashboard.R")
source("dashboard/server_dashboard.R")
source("dashboard/language_system.R")

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