# Gemeente Tab UI Component
# This file contains the UI for the Gemeente (Municipalities) tab

gemeente_ui <- function() {
  nav_panel(id = "ELANSetGemTab", uiOutput("gemeente_tab_title"), accordion(  
    accordion_panel(
      value = "control_panel",
      title = "Control Panel",
      icon = bsicons::bs_icon("sliders"),
      fluidRow( 
        column(10, 
               selectInput(
                 inputId = 'gem_drop_var_id',
                 label = uiOutput("gem_select_variable_label"),
                 choices = var_dict_nl,
                 selected = 'Total_ICPCPat_Pop',
                 width = '100%'
                 
               )
        ),
        column(2, 
               selectInput(inputId = "gem_drop_select_year",
                           label = uiOutput("gem_select_year_label"), c(""),#, selected = "2020",
                           width = '100%'
               )
        )
      ),
      fluidRow(
        column(4, 
               selectInput(
                 inputId = 'gem_drop_municipality',
                 label = uiOutput("gem_select_region_label"),
                 choices = area_dict,
                 selected = 'ELAN area',
                 width = '100%'
                 
               )
        ),
        column(8, 
               div(
                 class = "scrollable-dropdown-container",
                 uiOutput("gem_select_gemeente_label"),
                 div(
                   class = "scrollable-dropdown",
                   selectInput(inputId = "gem_drop_municipality_spec_id",
                               label = NULL,
                               multiple=TRUE, selectize=TRUE,
                               c(""),#, selected = "2020"
                               width = '100%'
                               
                   )
                 )
               ),
               div(
                 style = "margin-top: 10px; display: flex; justify-content: space-between; align-items: center;",
                 div(
                   class = "info-text-container",
                   bsicons::bs_icon("info-circle", size = 0.8),
                   textOutput("remove_item_info_text")
                 ),
                 actionButton("clear_button_gemeente", uiOutput("gem_clear_all_button_text"))
               )
        )
      )
      
    ),  
    id = "acc_gemeente",  
    open = "Control Panel"  
  ),
  # Variable explanation section for Gemeente
  accordion_panel(
    value = "variable_info",
    title = "Variabele Informatie",
    icon = bsicons::bs_icon("info-circle"),
    fluidRow(
      column(12,
        div(
          class = "variable-info-container",
          h5(uiOutput("gem_variable_definition_title"), class = "page-heading"),
          uiOutput('variable_definition_gemeente')
        )
      )
    ),
    fluidRow(
      column(12,
        div(
          class = "variable-info-container",
          h5(uiOutput("gem_data_source_title"), class = "page-heading"),
          uiOutput('data_source_gemeente')
        )
      )
    )
  ),
  fluidRow(
    column(8,
           fluidRow(
             card(
             uiOutput('gem_heading_map'),
             uiOutput('gem_subtext_map'),
             leafletOutput("gem_map",  height = 300)
             )
           ),
           fluidRow(
             card(
             uiOutput('gem_heading_line'),
             uiOutput('gem_subtext_line'),
             girafeOutput("gem_line_chart", width = "100%", height= "300px"),
             uiOutput('gem_line_legend')
             )
           ),
           fluidRow(
             card(
               selectInput(inputId = "gem_drop_selected_gemeente_id",
                           label = uiOutput("gem_selected_wijk_label"),
                           multiple=TRUE, selectize=TRUE,
                           c(""),
                           width = '100%'
                           
               ),
               downloadButton("gemeente_report", uiOutput("gem_generate_report_button_text"))
             )
           )
           
    ),
    column(4,
           card(
           uiOutput('gem_heading_bar'),
           uiOutput('gem_subtext_bar'),
           div(
             class = "bar-chart-wrapper",
           girafeOutput("gem_bar_chart", width = "100%", height= "auto") 
           ) 
           )
    )
  ))
} 