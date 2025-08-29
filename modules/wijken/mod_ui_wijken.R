# Wijken Tab UI Component
# This file contains the UI for the Wijken (Neighborhoods) tab

wijken_ui <- function() {
  nav_panel(id = "ELANSetWijkTab", uiOutput("wijken_tab_title"), accordion(  
    accordion_panel(
      value = "control_panel",
      title = "Control Panel",
      icon = bsicons::bs_icon("sliders"),
      fluidRow( 
        column(10, 
               selectInput(
                 inputId = 'drop_var_id',
                 label = uiOutput("select_variable_label"),
                 choices = var_dict_nl,
                 selected = 'Total_ICPCPat_Pop',
                 width = '100%'
                 
               )
        ),
        column(2, 
               selectInput(inputId = "drop_select_year",
                           label = uiOutput("select_year_label"), c(""),#, selected = "2020",
                           width = '100%'
               )
        )
      ),
      fluidRow(
        column(4, 
               selectInput(
                 inputId = 'drop_municipality',
                 label = uiOutput("select_region_label"),
                 choices = area_dict,
                 selected = 'ELAN area',
                 width = '100%'
                 
               )
        ),
        column(8, 
               div(
                 class = "scrollable-dropdown-container",
                 uiOutput("select_gemeente_label"),
                 div(
                   class = "scrollable-dropdown",
                   selectInput(inputId = "drop_municipality_spec_id",
                               label = NULL,
                               multiple=TRUE, selectize=TRUE,
                               c(""),
                               width = '100%'
                               
                   )
                 )
               ),
               div(
                 style = "margin-top: 10px; text-align: right;",
                 actionButton("clear_button_wijken", uiOutput("clear_all_button_text"))
               ),
               div(
                 style = "margin-top: 5px; font-size: 12px; color: #333; display: flex; align-items: center; gap: 5px;",
                 bsicons::bs_icon("info-circle", size = 1),
                 uiOutput("wijken_remove_info_text")
               )
            
        )
      )
      
    ),  
    id = "acc_wijken",  
    open = "Control Panel"  
  ),
  # Variable explanation section for Wijken
  accordion_panel(
    value = "variable_info",
    title = "Variabele Informatie",
    icon = bsicons::bs_icon("info-circle"),
    fluidRow(
      column(12,
        div(
          class = "variable-info-container",
          h5(uiOutput("variable_definition_title"), class = "page-heading"),
          uiOutput('variable_definition_wijken')
        )
      )
    ),
    fluidRow(
      column(12,
        div(
          class = "variable-info-container",
          h5(uiOutput("data_source_title"), class = "page-heading"),
          uiOutput('data_source_wijken')
        )
      )
    )
  ),
  fluidRow(
  column(8,
         fluidRow(
           card(
             uiOutput('heading_map'),
             uiOutput('subtext_map'),
             leafletOutput("map",  height = 300)
           )
           
         ),
         fluidRow(
           card(
             uiOutput('heading_line'),
             uiOutput('subtext_line'),
             girafeOutput("line_chart", width = "100%", height = "300px"),
             uiOutput('line_legend')
           )
           
         ),
         fluidRow(
           card(
             selectInput(inputId = "drop_selected_wijk_id",
                         label = uiOutput("selected_wijk_label"),
                         multiple=TRUE, selectize=TRUE,
                         c(""),
                         width = '100%'
                         
             ),
             downloadButton("wijk_report", uiOutput("generate_report_button_text"))
             
           )
           
         )
         
  ),
  column(4,
         card(
           uiOutput('heading_bar'),
           uiOutput('subtext_bar'),
           div(
             class = "bar-chart-wrapper",
           girafeOutput("bar_chart", width = "100%", height= "auto") 
           ) 
         )
         
  )
  )
  )
} 