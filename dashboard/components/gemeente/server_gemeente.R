# Gemeente Tab Server Logic
# This file contains the server logic for the Gemeente (Municipalities) tab

gemeente_server <- function(input, output, session, current_language = reactive("en")) {
  
  # Language-specific UI outputs for Gemeente
  output$gemeente_tab_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("gemeente_tab", lang)
  })
  
  output$gem_select_variable_label <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("select_variable", lang)
  })
  
  output$gem_select_year_label <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("select_year", lang)
  })
  
  output$gem_select_region_label <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("select_region", lang)
  })
  
  output$gem_select_gemeente_label <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("select_gemeente", lang)
  })
  
  output$gem_remove_neighborhood_tooltip_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("remove_neighborhood_tooltip", lang)
  })
  
  output$remove_item_info_text <- renderText({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("remove_item_info", lang)
  })
  
  output$gem_clear_all_button_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("clear_all", lang)
  })
  
  output$gem_variable_definition_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("variable_definition", lang)
  })
  
  output$gem_data_source_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("data_source", lang)
  })
  
  output$gem_selected_wijk_label <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("selected_wijk", lang)
  })
  
  output$gem_generate_report_button_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("generate_report", lang)
  })
  
  # Reactive expressions for Gemeente
  gem_var_to_map <- reactive({
    input$gem_drop_var_id
  })
  
  gem_year_to_map <- reactive({
    input$gem_drop_select_year
  })
  
  gem_municipality_to_map <- reactive({
    input$gem_drop_municipality
  })
  
  gem_spec_to_map <- reactive({
    input$gem_drop_municipality_spec_id
  })
  
  # UI outputs for Gemeente
  output$gem_heading_map <- renderUI({
    # Use appropriate variable label based on language
    if (current_language() == "nl") {
      var_label <- ifelse(is.null(var_def_label_NL_dict[[input$gem_drop_var_id]]), input$gem_drop_var_id, var_def_label_NL_dict[[input$gem_drop_var_id]])
    } else {
      var_label <- ifelse(is.null(var_def_label_dict[[input$gem_drop_var_id]]), input$gem_drop_var_id, var_def_label_dict[[input$gem_drop_var_id]])
    }
    card_header(paste(var_label, get_text("in_text", current_language()), input$gem_drop_municipality, get_text("at_text", current_language()), input$gem_drop_select_year))
  })
  
  output$gem_subtext_map <- renderUI({
    p(htmltools::HTML(get_text("map_subtext", current_language())))
  })
  
  output$gem_heading_line <- renderUI({
    # Use appropriate variable label based on language
    if (current_language() == "nl") {
      var_label <- ifelse(is.null(var_def_label_NL_dict[[input$gem_drop_var_id]]), input$gem_drop_var_id, var_def_label_NL_dict[[input$gem_drop_var_id]])
    } else {
      var_label <- ifelse(is.null(var_def_label_dict[[input$gem_drop_var_id]]), input$gem_drop_var_id, var_def_label_dict[[input$gem_drop_var_id]])
    }
    card_header(paste(var_label, get_text("in_text", current_language()), input$gem_drop_municipality, get_text("overtime_text", current_language())))
  })
  
  output$gem_subtext_line <- renderUI({
    p(htmltools::HTML(get_text("click_map_bar_trendline", current_language())))
  })
  
  output$gem_heading_bar <- renderUI({
    # Use appropriate variable label based on language
    if (current_language() == "nl") {
      var_label <- ifelse(is.null(var_def_label_NL_dict[[input$gem_drop_var_id]]), input$gem_drop_var_id, var_def_label_NL_dict[[input$gem_drop_var_id]])
    } else {
      var_label <- ifelse(is.null(var_def_label_dict[[input$gem_drop_var_id]]), input$gem_drop_var_id, var_def_label_dict[[input$gem_drop_var_id]])
    }
    card_header(paste(get_text("ranking_text", current_language()), var_label, get_text("at_text", current_language()), input$gem_drop_select_year))
  })
  
  output$gem_subtext_bar <- renderUI({
    p(htmltools::HTML(get_text("click_gemeente_trendline", current_language())))
  })
  
  # Variable explanation outputs for Gemeente tab
  output$variable_definition_gemeente <- renderUI({
    if (!is.null(input$gem_drop_var_id) && input$gem_drop_var_id != "") {
      # Use Dutch labels and definitions based on current language
      if (current_language() == "nl") {
        var_label <- var_def_label_NL_dict[[input$gem_drop_var_id]]
        var_def <- var_def_NL_dict[[input$gem_drop_var_id]]
      } else {
        var_label <- var_def_label_dict[[input$gem_drop_var_id]]
        var_def <- var_def_dict[[input$gem_drop_var_id]]
      }
      
      if (!is.null(var_label)) {
        if (!is.null(var_def)) {
          p(HTML(paste0("<strong>", var_label, ":</strong> ", var_def)))
        } else {
          p(HTML(paste0("<strong>", var_label, ":</strong> ", get_text("no_definition_available", current_language()))))
        }
      } else {
        if (!is.null(var_def)) {
          p(HTML(paste0("<strong>", input$gem_drop_var_id, ":</strong> ", var_def)))
        } else {
          p(HTML(paste0("<strong>", input$gem_drop_var_id, ":</strong> ", get_text("no_definition_available", current_language()))))
        }
      }
    } else {
      p(get_text("select_variable_for_definition", current_language()))
    }
  })
  
  output$data_source_gemeente <- renderUI({
    if (!is.null(input$gem_drop_var_id) && input$gem_drop_var_id != "") {
      # Use Dutch labels and data sources based on current language
      if (current_language() == "nl") {
        var_label <- var_def_label_NL_dict[[input$gem_drop_var_id]]
        data_source <- var_def_data_NL_dict[[input$gem_drop_var_id]]
      } else {
        var_label <- var_def_label_dict[[input$gem_drop_var_id]]
        data_source <- var_def_data_dict[[input$gem_drop_var_id]]
      }
      
      if (!is.null(var_label)) {
        if (!is.null(data_source)) {
          p(HTML(paste0("<strong>", var_label, " - ", get_text("data_source", current_language()), ":</strong> ", data_source)))
        } else {
          p(HTML(paste0("<strong>", var_label, ":</strong> ", get_text("data_source_not_available", current_language()))))
        }
      } else {
        if (!is.null(data_source)) {
          p(HTML(paste0("<strong>", input$gem_drop_var_id, " - ", get_text("data_source", current_language()), ":</strong> ", data_source)))
        } else {
          p(HTML(paste0("<strong>", input$gem_drop_var_id, ":</strong> ", get_text("data_source_not_available", current_language()))))
        }
      }
    } else {
      p(get_text("select_variable_for_data_source", current_language()))
    }
  })
  
  # Reactive values for Gemeente
  gem_dataset_subset <- reactiveVal()
  gem_GMN_selected <- reactiveVal()
  gem_WKN_selected <- reactiveVal()
  gem_selected_map <- reactiveValues(groups = vector())
  gem_selected_bar <- reactiveValues(name = vector())
  
  # Observe events for Gemeente
  observeEvent(input$gem_drop_var_id, ignoreInit = FALSE, {
    freezeReactiveValue(input, "gem_drop_select_year")
    
    gem_min_year <- min(as.numeric(unique(states_gmc[!is.na(states_gmc[[gem_var_to_map()]]), ][["YEAR"]])), na.rm = TRUE)
    gem_max_year <- max(as.numeric(unique(states_gmc[!is.na(states_gmc[[gem_var_to_map()]]), ][["YEAR"]])), na.rm = TRUE)
    
    gem_marks <- as.character(gem_min_year:gem_max_year)
    
    updateSelectInput(session, "gem_drop_select_year",
                      choices =  gem_marks, 
                      selected = (as.character(gem_max_year))  
    )
    
    gem_selected_map$groups  <- c()
    gem_selected_bar$name <- c()
  })
  

  
  # Update variable choices based on language for Gemeente
  observeEvent(current_language(), {
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    if (lang == "nl") {
      # Use Dutch variable dictionary
      updateSelectInput(session, "gem_drop_var_id",
        choices = var_dict_nl,
        selected = input$gem_drop_var_id
      )
    } else {
      # Use English variable dictionary
      updateSelectInput(session, "gem_drop_var_id",
        choices = var_dict_en,
        selected = input$gem_drop_var_id
      )
    }
  })
  



  
  observeEvent(input$gem_drop_municipality, ignoreInit = FALSE, {
    freezeReactiveValue(input, "gem_drop_municipality_spec_id")
    
    if (gem_municipality_to_map() %in% names(special_regions)) {
      gem_dff <- states_gmc[states_gmc$GMN %in% special_regions[[gem_municipality_to_map()]], ]
    } else {
      gem_dff <- states_gmc[states_gmc$GMN == gem_municipality_to_map(), ]
    }
    
    gem_value_labels <- unique(gem_dff$GMN)
    gem_selected_value <- unlist(unique(gem_dff$GMN))
    
    updateSelectInput(session, "gem_drop_municipality_spec_id",
                      choices =  (gem_value_labels),
                      selected = (gem_selected_value)
    )
  })
  
  # Clear All button for Gemeente tab
  observeEvent(input$clear_button_gemeente, {
    updateSelectInput(session, "gem_drop_municipality_spec_id", selected = character(0))
    gem_selected_map$groups <- vector()
    gem_selected_bar$name <- vector()
    updateSelectizeInput(session, "gem_drop_selected_gemeente_id", selected = character(0))
    
    leafletProxy("gem_map") %>%
      clearGroup(group = "gem_regions") %>%
      clearGroup(group = unique(states_gmc$GMC))
  })

  # Map output for Gemeente
  output$gem_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = 4.490150, lat = 52.143211, zoom = 9)
  })
  

  


  # Map observer for Gemeente
  observeEvent(list(input$gem_drop_var_id, input$gem_drop_select_year, input$gem_drop_municipality), ignoreInit = FALSE, {
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    # # Debug: Print input values
    # cat("Map observer triggered with inputs:\n")
    # cat("Variable:", input$gem_drop_var_id, "\n")
    # cat("Year:", input$gem_drop_select_year, "\n")
    # cat("Municipality:", input$gem_drop_municipality, "\n")
    
    # Ensure we have valid inputs before proceeding
    req(input$gem_drop_var_id, input$gem_drop_select_year)
    
    # Get municipalities based on the selected region
    if (input$gem_drop_municipality %in% names(special_regions)) {
      municipalities_to_use <- special_regions[[input$gem_drop_municipality]]
    } else {
      municipalities_to_use <- input$gem_drop_municipality
    }
    
    # cat("Municipalities to use:", municipalities_to_use, "\n")
    
    # # Check if we have any municipalities to display
    # if (length(municipalities_to_use) == 0) {
    #   cat("No municipalities to display\n")
    #   return()
    # }
    # cat("Filtering data for year:", gem_year_to_map(), "and municipalities:", municipalities_to_use, "\n")
    
    gem_states <- states_gmc %>%
      filter(YEAR == gem_year_to_map()) %>%
      filter(GMN %in% municipalities_to_use) %>%
      arrange(gem_var_to_map())

    # cat("Filtered data rows:", nrow(gem_states), "\n")
    # cat("Filtered municipalities:", unique(gem_states$GMN), "\n")

    if (nrow(gem_states) == 0) {
      cat("No data found, clearing map\n")
      leafletProxy("gem_map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }
    
    gem_dataset_subset(gem_states)
    
    gem_var_values <- gem_states[[gem_var_to_map()]]
    if (length(gem_var_values) == 0 || all(is.na(gem_var_values))) {
      leafletProxy("gem_map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }
    
    gem_paletteNum <- colorNumeric('Blues', domain = gem_var_values)

    gem_stateLabels <- sprintf('<b>%s : %s</b><br/>%s : %g <br/> %s : %s',
                           get_text("gemeente_label", lang), gem_states$GMN, 
                           gem_var_to_map(), gem_states[[gem_var_to_map()]], 
                           get_text("total_population_label", lang), prettyNum(gem_states$Total_Population,big.mark=",")) %>%
      lapply(function(x) HTML(x))
                   
    leafletProxy("gem_map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = gem_states,
                  layerId = ~GMC,
                  weight = 1,
                  fillOpacity = .75,
                  fillColor = ~gem_paletteNum(gem_states[[gem_var_to_map()]]),
                  label = ~gem_stateLabels,
                  labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '10px'),
                  group = "gem_regions",
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = 'yellow',
                    bringToFront = TRUE
                  )
      ) %>%
      addPolygons(data = gem_states,
                  fillColor = "yellow",
                  fillOpacity = 0.25,
                  weight = 1,
                  color = "black",
                  label = ~gem_stateLabels,
                  layerId = ~GM_CODE,
                  group = ~GMC) %>%
      hideGroup(group = gem_states$GMC)  %>%
      addLegend(pal = gem_paletteNum, values = gem_states[[gem_var_to_map()]],
                title = paste( "<small>", ifelse(is.null(var_def_label_dict[[gem_var_to_map()]]), gem_var_to_map(), var_def_label_dict[[gem_var_to_map()]]), "<br>", get_text("per_gemeente_label", lang), "</small>"),
                position = 'topleft') %>%
      onRender(paste("<script>", "map.doubleClickZoom.disable();", "</script>", sep = ""))
  })

  # Map click handler for Gemeente
  observeEvent(input$gem_map_shape_click, ignoreInit = FALSE, {
    if(input$gem_map_shape_click$group == "gem_regions" ){
      gem_selected_map$groups <- c(gem_selected_map$groups, input$gem_map_shape_click$id)
      leafletProxy("gem_map") %>% showGroup(group = (unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == unname(Code_dict_gmc_to_gmn[input$gem_map_shape_click$id])]))))
    } else {
      gem_selected_map$groups <- setdiff(gem_selected_map$groups, (unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == unname(Code_dict_gmc_to_gmn[input$gem_map_shape_click$group])]))) )
      leafletProxy("gem_map") %>% hideGroup(group = (unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == unname(Code_dict_gmc_to_gmn[input$gem_map_shape_click$group])]))) )
    }

    gem_selected_bar$name <- unname(Code_dict_gmc_to_gmn[gem_selected_map$groups])
    session$sendCustomMessage(type = 'gem_bar_chart_set', message = unname(Code_dict_gmc_to_gmn[gem_selected_map$groups]))
  })

  # Bar chart selected handler for Gemeente
  observeEvent(input$gem_bar_chart_selected, ignoreInit = FALSE, {
    if(!!length(setdiff(input$gem_bar_chart_selected, gem_selected_bar$name))){
      gem_change_value = setdiff(input$gem_bar_chart_selected, gem_selected_bar$name)

      if (length(gem_change_value) > 0) {
        gem_added_gmc <- foreach(i = gem_change_value) %do% {(unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == i])))}
        gem_added_gmc <- unlist(gem_added_gmc)
      } else {
        gem_added_gmc <- character(0)
      }

      gem_selected_map$groups <- c(gem_selected_map$groups, gem_added_gmc)
      gem_selected_bar$name <- unname(Code_dict_gmc_to_gmn[gem_selected_map$groups])
      leafletProxy("gem_map") %>% showGroup(group = gem_added_gmc)

      updateSelectizeInput(session,
                           inputId = "gem_drop_selected_gemeente_id",
                           choices = gem_dataset_subset()$GMN,
                           selected = gem_selected_bar$name)
    }
    else if(!!length(setdiff(gem_selected_bar$name, input$gem_bar_chart_selected)) ) {
      gem_change_value = setdiff(gem_selected_bar$name, input$gem_bar_chart_selected )

      if (length(gem_change_value) > 0) {
        gem_reduced_gmc <- foreach(i = gem_change_value) %do% {(unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == i])))}
        gem_reduced_gmc <- unlist(gem_reduced_gmc)
      } else {
        gem_reduced_gmc <- character(0)
      }

      gem_selected_map$groups <- setdiff(gem_selected_map$groups, gem_reduced_gmc )
      gem_selected_bar$name <- unname(Code_dict_gmc_to_gmn[gem_selected_map$groups])
      leafletProxy("gem_map") %>% hideGroup(group = gem_reduced_gmc )

      updateSelectizeInput(session,
                           inputId = "gem_drop_selected_gemeente_id",
                           choices = gem_dataset_subset()$GMN,
                           selected = gem_selected_bar$name)
    }
  })

  # Dropdown selection handler for Gemeente
  observeEvent(input$gem_drop_selected_gemeente_id, ignoreNULL = FALSE, {
    if(!!length(setdiff(input$gem_drop_selected_gemeente_id, gem_selected_bar$name))){
      gem_change_value = setdiff(input$gem_drop_selected_gemeente_id, gem_selected_bar$name)
      
      if (length(gem_change_value) > 0) {
        gem_added_gmc <- foreach(i = gem_change_value) %do% {(unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == i])))}
        gem_added_gmc <- unlist(gem_added_gmc)
      } else {
        gem_added_gmc <- character(0)
      }
      
      gem_selected_map$groups <- c(gem_selected_map$groups, gem_added_gmc)
      gem_selected_bar$name <- unname(Code_dict_gmc_to_gmn[gem_selected_map$groups])
      leafletProxy("gem_map") %>% showGroup(group = gem_added_gmc)
      
    }
    else if(!!length(setdiff(gem_selected_bar$name, input$gem_drop_selected_gemeente_id)) ) {
      gem_change_value = setdiff(gem_selected_bar$name, input$gem_drop_selected_gemeente_id )
      
      if (length(gem_change_value) > 0) {
        gem_reduced_gmc <- foreach(i = gem_change_value) %do% {(unique(unname(Code_dict_gmn_to_gmc[names(Code_dict_gmn_to_gmc) == i])))}
        gem_reduced_gmc <- unlist(gem_reduced_gmc)
      } else {
        gem_reduced_gmc <- character(0)
      }
      
      gem_selected_map$groups <- setdiff(gem_selected_map$groups, gem_reduced_gmc )
      gem_selected_bar$name <- unname(Code_dict_gmc_to_gmn[gem_selected_map$groups])
      leafletProxy("gem_map") %>% hideGroup(group = gem_reduced_gmc )
      
    }
    
    session$sendCustomMessage(type = 'gem_bar_chart_set', message = unname(Code_dict_gmc_to_gmn[gem_selected_map$groups]))
  })

  # Bar chart output for Gemeente
  output$gem_bar_chart <- renderGirafe({
    gem_bar_data <- gem_dataset_subset()

    if (is.null(gem_bar_data) || nrow(gem_bar_data) == 0) {
      df = data.frame(x = c(0, 10), y = c(0, 0))
      gem_bar <- ggplot(df, aes(x = x, y = y)) +
        geom_col_interactive() +
        theme_void() +
        labs(x = get_text("gemeente_label", current_language()), y = get_text("value_label", current_language())) + 
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA))
      return(girafe(ggobj = gem_bar))
    }

    len_gmn <- length(unique(gem_bar_data$GMN))
    height_adj <- ifelse(len_gmn < 15, 6, 25)

    if (!is.null(dim(gem_bar_data))) {
      gem_bar_data <- gem_bar_data %>% drop_na(gem_var_to_map())
      
      if (nrow(gem_bar_data) == 0) {
        df = data.frame(x = c(0, 10), y = c(0, 0))
        gem_bar <- ggplot(df, aes(x = x, y = y)) +
          geom_col_interactive() +
          theme_void() +
          labs(x = get_text("gemeente_label", current_language()), y = get_text("value_label", current_language())) + 
          theme(plot.background = element_rect(fill = "transparent", color = NA),
                panel.background = element_rect(fill = "transparent", color = NA))
        return(girafe(ggobj = gem_bar))
      }
      
      gem_bar <- gem_bar_data[!duplicated(gem_bar_data$GMN, fromLast = TRUE), ] |>
        ggplot() +
        geom_col_interactive(aes(
          x = reorder(GMN, eval(parse(text=gem_var_to_map()))),
          y = eval(parse(text=gem_var_to_map())),
          fill = reorder(GMN, eval(parse(text=gem_var_to_map()))),
          data_id = GMN,
          tooltip = c(paste0(get_text("gemeente_label", current_language()), ": ", GMN, "\n ", get_text("value_label", current_language()), ": ", eval(parse(text=gem_var_to_map())) )),
        )) +
        geom_text_interactive(aes(
          x = reorder(GMN, eval(parse(text=gem_var_to_map()))),
          y = 0.02,
          label = GMN), colour="#0e1d6b", check_overlap = TRUE,  hjust = 0, size = 10) +
        coord_flip() +
        xlab("GMN") +
        ylab(var_def_label_dict[gem_var_to_map()]) +
        custom_theme() +
        theme(axis.text.y=element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1), 
              legend.position="none")+
        scale_fill_manual_interactive(values = grDevices::colorRampPalette(c("#f2fff6", "#3C50BF"))(22) )

      girafe(ggobj = gem_bar, options = list(opts_selection(type = "multiple", css = "fill:yellow;stroke:white;r:5pt;"),
                                         opts_hover(css = "fill:wheat;stroke:white;r:5pt;") ), height_svg = height_adj * 1.5, width_svg = 8)
    }
  })

  # Line chart output for Gemeente
  output$gem_line_chart <- renderGirafe({
    gem_states <- states_gmc %>%
      filter(GMN %in% gem_spec_to_map()) %>%
      filter(GMN %in% gem_selected_bar$name)
    
    if ( nrow(gem_states) == 0) {
      df = data.frame(x = c(0, 10), y = c(0, 0))
      line <- ggplot(df, aes(x = x, y = y)) +
        geom_line_interactive() +
        geom_point_interactive() +
        theme_void() +
        labs(x = get_text("year_label", current_language()), y = get_text("value_label", current_language())) +
        theme(legend.position = 'none',
              panel.grid.minor = element_blank(),
              plot.title.position = 'plot')
      
      return(girafe(ggobj = line))
    }
    
    line <- gem_states[!is.na(gem_states[[gem_var_to_map()]]), ][!duplicated(gem_states[c('GMN','YEAR')], fromLast = TRUE), ] |>
      ggplot(aes(
        x = YEAR,
        y = eval(parse(text=gem_var_to_map())),
        colour = GMN,
        data_id = GMN,
      )) +
      scale_x_continuous(n.breaks = length(unique(gem_states[!is.na(gem_states[[gem_var_to_map()]]), ][["YEAR"]])) ) +
      scale_y_continuous(n.breaks = 8) +
      scale_color_manual(values = gmn_color_dict) +
      geom_line_interactive(linewidth = 0.5) +
      geom_point_interactive(aes(tooltip = c(paste0(get_text("gemeente_label", current_language()), ": ", GMN, "\n ", get_text("year_label", current_language()), ": ", YEAR, "\n ", get_text("value_label", current_language()), ": ", eval(parse(text=gem_var_to_map())) )) ), size = 1.5) +
      labs(x = get_text("year_label", current_language()), y = get_text("value_label", current_language())) +
      line_chart_theme() +
      theme(legend.position = 'none',
            panel.grid.minor = element_blank(),
            plot.title.position = 'plot')
    
    line_girraffe <- girafe(ggobj = line, height_svg = 3, width_svg = 12)
    line_girraffe <- girafe_options(
      line_girraffe,
      opts_selection(type = "none", css = "opacity:0.5;stroke:yellow;r:0.5pt;", only_shiny = TRUE),
      opts_hover(css = ''), 
      opts_hover_inv(css = "opacity:0.1;"), 
      opts_sizing(rescale = TRUE)
    )
    
    line_girraffe
  })
  
  # Interactive legend for gemeente line chart
  output$gem_line_legend <- renderUI({
    gem_states <- states_gmc %>%
      filter(GMN %in% gem_spec_to_map()) %>%
      filter(GMN %in% gem_selected_bar$name)
    
    if (nrow(gem_states) == 0) {
      return(NULL)
    }
    
    unique_gmn <- unique(gem_states$GMN)
    legend_items <- lapply(unique_gmn, function(gmn) {
      color <- gmn_color_dict[gmn]
      div(
        class = "legend-item",
        div(
          class = "legend-color",
          style = paste0("background-color: ", color, ";")
        ),
        span(
          class = "legend-text",
          gmn
        )
      )
    })
    
    div(
      class = "legend-container",
      h4(get_text("legend_title", current_language()), class = "legend-title"),
      div(
        class = "legend-items",
        legend_items
      )
    )
  })
  
  # Report generation for Gemeente
  output$gemeente_report <- downloadHandler(
    filename = "gemeente report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "gemeente_report_basic.Rmd")
      file.copy("gemeente_report_basic.Rmd", tempReport, overwrite = TRUE)
      
      selected_gemeentes <- if(length(input$gem_drop_selected_gemeente_id) > 0) {
        input$gem_drop_selected_gemeente_id
      } else if(length(gem_selected_bar$name) > 0) {
        gem_selected_bar$name
      } else {
        character(0)
      }
      
      # Get the selected variable from the dashboard control panel
      selected_variable <- if(!is.null(input$gem_drop_var_id) && input$gem_drop_var_id != "") {
        input$gem_drop_var_id
      } else {
        "Total_ICPCPat_Pop"  # Default fallback
      }
      
      params <- list(
        n = selected_gemeentes,
        selected_variable = selected_variable
      )
      report_env <- new.env(parent = .GlobalEnv)
      
      # Pass all necessary data to the report environment
      report_env$states_gmc <- states_gmc
      report_env$states_wkc <- states_wkc
      report_env$var_def_label_dict <- var_def_label_dict
      report_env$gmn_color_dict <- gmn_color_dict
      report_env$df <- df
      report_env$df_gem <- df_gem
      report_env$geo_df <- geo_df
      report_env$geo_df_gem <- geo_df_gem
      report_env$GMN_WKN_list <- GMN_WKN_list
      report_env$Code_dict_wkn_to_wkc <- Code_dict_wkn_to_wkc
      report_env$Code_dict_wkc_to_wkn <- Code_dict_wkc_to_wkn
      report_env$Code_dict_gmn_to_gmc <- Code_dict_gmn_to_gmc
      report_env$Code_dict_gmc_to_gmn <- Code_dict_gmc_to_gmn
      report_env$var_dict <- var_dict
      report_env$area_dict <- area_dict
      report_env$special_regions <- special_regions
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = report_env,
                        quiet = FALSE
      )
    }
  )
} 