# modules/dashboard/mod_server_dashboard.R

dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Language toggle
    current_language <- reactiveVal("nl")
    
    observeEvent(input$language_selector, {
      current_language(ifelse(current_language() == "nl", "en", "nl"))
    })
    
    output$language_button_text <- renderText({
      if (current_language() == "nl") "NL" else "EN"
    })
    
    # Helper to render language-dependent UI text
    lang_output <- function(key) {
      renderUI({
        lang <- current_language()
        if (is.null(lang) || length(lang) == 0) lang <- "en"
        get_text(key, lang)
      })
    }
    
    # Footer and navbar text
    output$last_updated_text <- lang_output("last_updated")
    output$copyright_text <- lang_output("copyright")
    output$about_us_link <- lang_output("about_us")
    output$change_log_link <- lang_output("change_log")
    output$variables_explanation_link <- lang_output("variables_explanation")
    output$variables_definition_link <- lang_output("variables_definition")
    output$data_sources_link <- lang_output("data_sources")
    output$partners_title_text <- lang_output("partners_title")
    output$remove_item_info_text <- lang_output("remove_item_info")
    output$variables_definition_panel_title <- lang_output("variables_definition_panel")
    output$data_dictionary_panel_title <- lang_output("data_dictionary_panel")
    output$change_log_panel_title <- lang_output("change_log_panel")
    output$about_us_panel_title <- lang_output("about_us_panel")
    
    # Call tab modules
    wijken_server("wijken", current_language)
    gemeente_server("gemeente", current_language)
    
    # Modal for variables explanation
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
    
    # Data Sources tables
    output$table_h <- DT::renderDataTable({
      DT::datatable(df_h, options = list(pageLength = 10, lengthMenu = c(5, 10, 25, 50), searching = TRUE, ordering = TRUE), filter = "top", style = "bootstrap")
    })
    
    output$table_cbs <- DT::renderDataTable({
      DT::datatable(df_cbs, options = list(pageLength = 25, lengthMenu = c(5, 10, 25, 50), searching = TRUE, ordering = TRUE), filter = "top", style = "bootstrap")
    })
    
    output$variables_definition_table <- DT::renderDataTable({
      DT::datatable(df_label_def_nl, options = list(pageLength = 25, lengthMenu = c(10, 25, 50, 100), searching = TRUE, ordering = TRUE), filter = "top", style = "bootstrap", rownames = FALSE)
    })
  })
}
