# Wijken Tab Server Logic
# This file contains the server logic for the Wijken (Neighborhoods) tab

wijken_server <- function(input, output, session, current_language = reactive("en")) {
  
  # Language-specific UI outputs
  output$wijken_tab_title <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    get_text("wijken_tab", lang)
  })
  
  output$select_variable_label <- renderUI({
    get_text("select_variable", current_language())
  })
  
  output$select_year_label <- renderUI({
    get_text("select_year", current_language())
  })
  
  output$select_region_label <- renderUI({
    get_text("select_region", current_language())
  })
  
  output$select_gemeente_label <- renderUI({
    get_text("select_gemeente", current_language())
  })
  
  output$remove_neighborhood_tooltip_text <- renderUI({
    get_text("remove_neighborhood_tooltip", current_language())
  })
  
  output$wijken_remove_info_text <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    if (lang == "nl") {
      "Klik op het item - Wijk - en druk op delete of backspace toets om het item te verwijderen"
    } else {
      "Click the item - Wijk - and press delete or backspace button to remove the item"
    }
  })
  
  output$clear_all_button_text <- renderUI({
    get_text("clear_all", current_language())
  })
  
  output$variable_definition_title <- renderUI({
    get_text("variable_definition", current_language())
  })
  
  output$data_source_title <- renderUI({
    get_text("data_source", current_language())
  })
  
  output$selected_wijk_label <- renderUI({
    get_text("selected_wijk", current_language())
  })
  
  output$generate_report_button_text <- renderUI({
    get_text("generate_report", current_language())
  })
  
  # Reactive expressions for Wijken
  var_to_map <- reactive({
    input$drop_var_id
  })
  
  year_to_map <- reactive({
    input$drop_select_year
  })
  
  municipality_to_map <- reactive({
    input$drop_municipality
  }) 
  
  neighbourhood_to_map <- reactive({
    input$drop_municipality_spec_id 
  })
  
  # UI outputs for Wijken
  output$heading_map <- renderUI({
    # Use appropriate variable label based on language
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    if (lang == "nl") {
      var_label <- ifelse(is.null(var_def_label_NL_dict[[input$drop_var_id]]), input$drop_var_id, var_def_label_NL_dict[[input$drop_var_id]])
    } else {
      var_label <- ifelse(is.null(var_def_label_dict[[input$drop_var_id]]), input$drop_var_id, var_def_label_dict[[input$drop_var_id]])
    }
    card_header(paste(var_label, get_text("in_text", lang), input$drop_municipality, get_text("at_text", lang), input$drop_select_year))
  })
  
  output$subtext_map <- renderUI({
    p(htmltools::HTML(get_text("map_subtext", current_language())))
  })
  
  output$heading_line <- renderUI({
    # Use appropriate variable label based on language
    if (current_language() == "nl") {
      var_label <- ifelse(is.null(var_def_label_NL_dict[[input$drop_var_id]]), input$drop_var_id, var_def_label_NL_dict[[input$drop_var_id]])
    } else {
      var_label <- ifelse(is.null(var_def_label_dict[[input$drop_var_id]]), input$drop_var_id, var_def_label_dict[[input$drop_var_id]])
    }
    card_header(paste(var_label, get_text("in_text", current_language()), input$drop_municipality, get_text("overtime_text", current_language())))
  })
  
  output$subtext_line <- renderUI({
    p(htmltools::HTML(get_text("click_map_bar_trendline", current_language())))
  })
  
  output$heading_bar <- renderUI({
    # Use appropriate variable label based on language
    if (current_language() == "nl") {
      var_label <- ifelse(is.null(var_def_label_NL_dict[[input$drop_var_id]]), input$drop_var_id, var_def_label_NL_dict[[input$drop_var_id]])
    } else {
      var_label <- ifelse(is.null(var_def_label_dict[[input$drop_var_id]]), input$drop_var_id, var_def_label_dict[[input$drop_var_id]])
    }
    card_header(paste(get_text("ranking_text", current_language()), var_label, get_text("at_text", current_language()), input$drop_select_year))
  })
  
  output$subtext_bar <- renderUI({
    p(htmltools::HTML(get_text("click_wijk_trendline", current_language())))
  })
  
  # Variable explanation outputs for Wijken tab
  output$variable_definition_wijken <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    if (!is.null(input$drop_var_id) && input$drop_var_id != "") {
      # Use Dutch labels and definitions based on current language
      if (lang == "nl") {
        var_label <- var_def_label_NL_dict[[input$drop_var_id]]
        var_def <- var_def_NL_dict[[input$drop_var_id]]
      } else {
        var_label <- var_def_label_dict[[input$drop_var_id]]
        var_def <- var_def_dict[[input$drop_var_id]]
      }
      
      if (!is.null(var_label)) {
        if (!is.null(var_def)) {
          p(HTML(paste0("<strong>", var_label, ":</strong> ", var_def)))
        } else {
          p(HTML(paste0("<strong>", var_label, ":</strong> ", get_text("no_definition_available", lang))))
        }
      } else {
        if (!is.null(var_def)) {
          p(HTML(paste0("<strong>", input$drop_var_id, ":</strong> ", var_def)))
        } else {
          p(HTML(paste0("<strong>", input$drop_var_id, ":</strong> ", get_text("no_definition_available", lang))))
        }
      }
    } else {
      p(get_text("select_variable_for_definition", lang))
    }
  })
  
  output$data_source_wijken <- renderUI({
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    if (!is.null(input$drop_var_id) && input$drop_var_id != "") {
      # Use Dutch labels and data sources based on current language
      if (lang == "nl") {
        var_label <- var_def_label_NL_dict[[input$drop_var_id]]
        data_source <- var_def_data_NL_dict[[input$drop_var_id]]
      } else {
        var_label <- var_def_label_dict[[input$drop_var_id]]
        data_source <- var_def_data_dict[[input$drop_var_id]]
      }
      
      if (!is.null(var_label)) {
        if (!is.null(data_source)) {
          p(HTML(paste0("<strong>", var_label, " - ", get_text("data_source", lang), ":</strong> ", data_source)))
        } else {
          p(HTML(paste0("<strong>", var_label, ":</strong> ", get_text("data_source_not_available", lang))))
        }
      } else {
        if (!is.null(data_source)) {
          p(HTML(paste0("<strong>", input$drop_var_id, " - ", get_text("data_source", lang), ":</strong> ", data_source)))
        } else {
          p(HTML(paste0("<strong>", input$drop_var_id, ":</strong> ", get_text("data_source_not_available", lang))))
        }
      }
    } else {
      p(get_text("select_variable_for_data_source", lang))
    }
  })
  
  # Reactive values for Wijken
  dataset_subset <- reactiveVal()
  GMN_selected <- reactiveVal()
  WKN_selected <- reactiveVal()
  selected_map <- reactiveValues(groups = vector())
  selected_bar <- reactiveValues(name = vector())
  
  # Observe events for Wijken
  observeEvent(input$drop_var_id, {
    freezeReactiveValue(input, "drop_select_year")
    
    min_year <- min(as.numeric(unique(states_wkc[!is.na(states_wkc[[var_to_map()]]), ][["YEAR"]])), na.rm = TRUE)
    max_year <- max(as.numeric(unique(states_wkc[!is.na(states_wkc[[var_to_map()]]), ][["YEAR"]])), na.rm = TRUE)
    
    marks <- as.character(min_year:max_year)
    
    updateSelectInput(session, "drop_select_year",
                      choices =  marks, 
                      selected = (as.character(max_year))  
    )
    
    selected_map$groups  <- c()
    selected_bar$name <- c()
  })
  
  # Update variable choices based on language
  observeEvent(current_language(), {
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    if (lang == "nl") {
      # Use Dutch variable dictionary
      updateSelectInput(session, "drop_var_id",
        choices = var_dict_nl,
        selected = input$drop_var_id
      )
    } else {
      # Use English variable dictionary
      updateSelectInput(session, "drop_var_id",
        choices = var_dict_en,
        selected = input$drop_var_id
      )
    }
  })


  
  observeEvent(input$drop_municipality, {
    freezeReactiveValue(input, "drop_municipality_spec_id")

    if (municipality_to_map() %in% names(special_regions)) {
      dff <- states_wkc[states_wkc$GMN %in% special_regions[[municipality_to_map()]], ]
    } else {
      dff <- states_wkc[states_wkc$GMN == municipality_to_map(), ]
    }

    value_labels <- GMN_WKN_list[unique(dff$GMN)]
    selected_value <- unlist(GMN_WKN_list[unique(dff$GMN)])

    updateSelectInput(session, "drop_municipality_spec_id",
                      choices =  (value_labels),
                      selected = (selected_value)
    )
  })
  
  # Clear All button for Wijken tab
  observeEvent(input$clear_button_wijken, {
    updateSelectInput(session, "drop_municipality_spec_id", selected = character(0))
    selected_map$groups <- vector()
    selected_bar$name <- vector()
    updateSelectizeInput(session, "drop_selected_wijk_id", selected = character(0))
    
    leafletProxy("map") %>%
      clearGroup(group = "regions") %>%
      clearGroup(group = unique(states_wkc$WKC))
  })
 
  # Map output for Wijken
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      setView(lng = 4.490150, lat = 52.143211, zoom = 9.5) 
  })

  # Map observer for Wijken
  observeEvent(ignoreInit = FALSE, list(input$drop_var_id, input$drop_select_year, input$drop_municipality_spec_id), {
    lang <- current_language()
    if (is.null(lang) || length(lang) == 0) lang <- "en"
    
    states <- states_wkc %>%
      filter(YEAR == year_to_map()) %>%
      filter(WKN %in% neighbourhood_to_map()) %>%
      arrange(var_to_map())
    
    if (nrow(states) == 0) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }
    
    dataset_subset(states)
    
    var_values <- states[[var_to_map()]]
    if (length(var_values) == 0 || all(is.na(var_values))) {
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls()
      return()
    }
    
    paletteNum <- colorNumeric('Blues', domain = var_values)
    
    stateLabels <- sprintf('<b>%s : %s</b><br/> %s : %s <br/>%s : %g <br/> %s : %s',
                           get_text("gemeente_label", lang), states$GMN, 
                           get_text("wijk_label", lang), states$WKN, 
                           var_to_map(), states[[var_to_map()]], 
                           get_text("total_population_label", lang), prettyNum(states$Total_Population,big.mark=",")) %>%
      lapply(function(x) HTML(x))
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = states,
                  layerId = ~WKC,
                  weight = 1,
                  fillOpacity = .75,
                  fillColor = ~paletteNum(states[[var_to_map()]]),
                  label = ~stateLabels,
                  labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '10px'),
                  group = "regions",
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = 'yellow',
                    bringToFront = TRUE
                  ) 
      ) %>%
      addPolygons(data = states,
                  fillColor = "yellow",
                  fillOpacity = 0.25,
                  weight = 1,
                  color = "black",
                  label = ~stateLabels,
                  layerId = ~WK_CODE,
                  group = ~WKC) %>%
      hideGroup(group = states$WKC)  %>%
      addLegend(pal = paletteNum, values = states[[var_to_map()]],
                title = paste( "<small>", ifelse(is.null(var_def_label_dict[[var_to_map()]]), var_to_map(), var_def_label_dict[[var_to_map()]]), "<br>", get_text("per_wijk_label", lang), "</small>"),
                position = 'topleft') %>%
      onRender(paste("<script>", "map.doubleClickZoom.disable();", "</script>", sep = ""))
  })
  
  # Map click handler for Wijken
  observeEvent(input$map_shape_click, {
    if(input$map_shape_click$group == "regions" ){
      selected_map$groups <- c(selected_map$groups, input$map_shape_click$id)
      
      leafletProxy("map") %>% showGroup(group = (unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == unname(Code_dict_wkc_to_wkn[input$map_shape_click$id])]))))
    } else {
      selected_map$groups <- setdiff(selected_map$groups, (unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == unname(Code_dict_wkc_to_wkn[input$map_shape_click$group])]))) )
      leafletProxy("map") %>% hideGroup(group = (unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == unname(Code_dict_wkc_to_wkn[input$map_shape_click$group])]))) )
    }
    
    selected_bar$name <- unique(unname(Code_dict_wkc_to_wkn[selected_map$groups]))
    session$sendCustomMessage(type = 'bar_chart_set', message = unique(unname(Code_dict_wkc_to_wkn[selected_map$groups])))
    
    updateSelectizeInput(session,
                         inputId = "drop_selected_wijk_id",
                         choices = dataset_subset()$WKN,
                         selected = selected_bar$name)
  })

  # Bar chart selected handler for Wijken
  observeEvent(input$bar_chart_selected, ignoreNULL = FALSE, {
    bar_chart_selected <- unique(input$bar_chart_selected)
    
    if(!!length(setdiff(bar_chart_selected, selected_bar$name))){
      change_value = setdiff(bar_chart_selected, selected_bar$name)
      
      if (length(change_value) > 0) {
        added_wkc <- foreach(i = change_value) %do% {(unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == i])))}
        added_wkc <- unlist(added_wkc)
      } else {
        added_wkc <- character(0)
      }
      
      selected_map$groups <- c(selected_map$groups, added_wkc)
      selected_bar$name <- unique(unname(Code_dict_wkc_to_wkn[selected_map$groups]))
      leafletProxy("map") %>% showGroup(group = added_wkc)
      
      updateSelectizeInput(session,
                           inputId = "drop_selected_wijk_id",
                           choices = dataset_subset()$WKN,
                           selected = selected_bar$name)
    }
    else if(!!length(setdiff(selected_bar$name, bar_chart_selected)) ) {
      change_value = setdiff(selected_bar$name, bar_chart_selected )
      
      if (length(change_value) > 0) {
        reduced_wkc <- foreach(i = change_value) %do% {(unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == i])))}
        reduced_wkc <- unlist(reduced_wkc)
      } else {
        reduced_wkc <- character(0)
      }
      
      selected_map$groups <- setdiff(selected_map$groups, reduced_wkc )
      selected_bar$name <- unique(unname(Code_dict_wkc_to_wkn[selected_map$groups]))
      leafletProxy("map") %>% hideGroup(group = reduced_wkc )
      
      updateSelectizeInput(session,
                           inputId = "drop_selected_wijk_id",
                           choices = dataset_subset()$WKN,
                           selected = selected_bar$name)
    }
  })
  
  # Dropdown selection handler for Wijken
  observeEvent(input$drop_selected_wijk_id, ignoreNULL = FALSE, {
    if(!!length(setdiff(input$drop_selected_wijk_id, selected_bar$name))){
      change_value = setdiff(input$drop_selected_wijk_id, selected_bar$name)
      
      if (length(change_value) > 0) {
        added_wkc <- foreach(i = change_value) %do% {(unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == i])))}
        added_wkc <- unlist(added_wkc)
      } else {
        added_wkc <- character(0)
      }
      
      selected_map$groups <- c(selected_map$groups, added_wkc)
      selected_bar$name <- unique(unname(Code_dict_wkc_to_wkn[selected_map$groups]))
      leafletProxy("map") %>% showGroup(group = added_wkc)
    }
    else if(!!length(setdiff(selected_bar$name, input$drop_selected_wijk_id)) ) {
      change_value = setdiff(selected_bar$name, input$drop_selected_wijk_id )
      
      if (length(change_value) > 0) {
        reduced_wkc <- foreach(i = change_value) %do% {(unique(unname(Code_dict_wkn_to_wkc[names(Code_dict_wkn_to_wkc) == i])))}
        reduced_wkc <- unlist(reduced_wkc)
      } else {
        reduced_wkc <- character(0)
      }
      
      selected_map$groups <- setdiff(selected_map$groups, reduced_wkc )
      selected_bar$name <- unique(unname(Code_dict_wkc_to_wkn[selected_map$groups]))
      leafletProxy("map") %>% hideGroup(group = reduced_wkc )
    }
    
    session$sendCustomMessage(type = 'bar_chart_set', message = unique(unname(Code_dict_wkc_to_wkn[selected_map$groups])))
  })
  
  # Bar chart output for Wijken
  output$bar_chart <- renderGirafe({
    bar_data <- dataset_subset()
    
    if (is.null(bar_data) || nrow(bar_data) == 0) {
      df = data.frame(x = c(0, 10), y = c(0, 0))
      bar <- ggplot(df, aes(x = x, y = y)) +
        geom_col_interactive() +
        theme_void() +
        labs(x = get_text("wijk_label", current_language()), y = get_text("value_label", current_language())) + 
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA))
      return(girafe(ggobj = bar))
    }
    
    bar_data <- bar_data[!duplicated(bar_data$WKN, fromLast = TRUE), ]
    len_wkn <- length(unique(bar_data$WKN))
    height_adj <- ifelse(len_wkn < 15, 6, 25)
    
    if (!is.null(dim(bar_data))) {
      bar_data <- bar_data %>% drop_na(var_to_map())
      bar_data <- bar_data[!duplicated(bar_data$WKN), ]
      
      if (nrow(bar_data) == 0) {
        df = data.frame(x = c(0, 10), y = c(0, 0))
        bar <- ggplot(df, aes(x = x, y = y)) +
          geom_col_interactive() +
          theme_void() +
          labs(x = get_text("wijk_label", current_language()), y = get_text("value_label", current_language())) + 
          theme(plot.background = element_rect(fill = "transparent", color = NA),
                panel.background = element_rect(fill = "transparent", color = NA))
        return(girafe(ggobj = bar))
      }
      
      bar <- bar_data |>
        ggplot() +
        geom_col_interactive(aes(
          x = reorder(WKN, eval(parse(text=var_to_map()))),
          y = eval(parse(text=var_to_map())), 
          fill = reorder(WKN, eval(parse(text=var_to_map()))),
          data_id = WKN,
          tooltip = c(paste0(get_text("wijk_label", current_language()), ": ", WKN, "\n ", get_text("value_label", current_language()), ": ", eval(parse(text=var_to_map())) )),
        )) + 
        geom_text_interactive(aes(
          x = reorder(WKN, eval(parse(text=var_to_map()))),
          y = 0.02,
          label = WKN), colour="#0e1d6b", check_overlap = TRUE,  hjust = 0, size= height_adj /4) +
        coord_flip() + 
        xlab("WKN") +
        ylab(var_def_label_dict[var_to_map()]) +
        custom_theme() +
        theme(axis.text.y=element_blank(),
              axis.text.x = element_text(angle = 90, hjust = 1), 
              legend.position="none")+
        scale_fill_manual_interactive(values = grDevices::colorRampPalette(c("#f2fff6", "#3C50BF"))(169) )
      
      girafe(ggobj = bar, options = list(opts_selection(type = "multiple", css = "fill:yellow;stroke:white;r:5pt;"),
                                         opts_hover(css = "fill:wheat;stroke:white;r:5pt;") ), height_svg = height_adj * 3, width_svg = 8)
    }
  })
 
  # Line chart output for Wijken
  output$line_chart <- renderGirafe({
    states <- states_wkc %>%
      filter(WKN %in% neighbourhood_to_map()) %>%
      filter(WKN %in% selected_bar$name)
    
    if ( nrow(states) == 0) {
      df = data.frame(x = c(0, 10), y = c(0, 0))
      line <- ggplot(df, aes(x = x, y = y)) +
        geom_line_interactive() +
        geom_point_interactive() +
        theme_void() +
        labs(x = get_text("year_label", current_language()), y = get_text("value_label", current_language())) + 
        theme(plot.background = element_rect(fill = "transparent", color = NA),
              panel.background = element_rect(fill = "transparent", color = NA))
      return(girafe(ggobj = line))
    }
    
    line <- states[!is.na(states[[var_to_map()]]), ][!duplicated(states[c('WKN','YEAR')], fromLast = TRUE), ] |>
      ggplot(aes(
        x = YEAR,
        y = eval(parse(text=var_to_map())),
        colour = GMN,
        data_id = WKN,
      )) + 
      scale_x_continuous(n.breaks = length(unique(states[!is.na(states[[var_to_map()]]), ][["YEAR"]])) ) +
      scale_y_continuous(n.breaks = 8) +
      scale_color_manual(values = gmn_color_dict) +
      geom_line_interactive(linewidth = 0.5) +
      geom_point_interactive(aes(tooltip = c(paste0(get_text("wijk_label", current_language()), ": ", WKN, "\n ", get_text("year_label", current_language()), ": ", YEAR, "\n ", get_text("value_label", current_language()), ": ", eval(parse(text=var_to_map())) )) ), size = 1.5) +
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
  
  # Interactive legend for line chart
  output$line_legend <- renderUI({
    states <- states_wkc %>%
      filter(WKN %in% neighbourhood_to_map()) %>%
      filter(WKN %in% selected_bar$name)
    
    if (nrow(states) == 0) {
      return(NULL)
    }
    
    unique_wkn <- unique(states$WKN)
    legend_items <- lapply(unique_wkn, function(wkn) {
      gmn_for_wkn <- unique(states$GMN[states$WKN == wkn])
      color <- gmn_color_dict[gmn_for_wkn]
      div(
        class = "legend-item",
        div(
          class = "legend-color",
          style = paste0("background-color: ", color, ";")
        ),
        span(
          class = "legend-text",
          wkn
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
  
  # Report generation for Wijken
  output$wijk_report <- downloadHandler(
    filename = "wijk report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "wijk_report_basic.Rmd")
      file.copy("wijk_report_basic.Rmd", tempReport, overwrite = TRUE)
      
      selected_neighborhoods <- if(length(input$drop_selected_wijk_id) > 0) {
        input$drop_selected_wijk_id
      } else if(length(selected_bar$name) > 0) {
        selected_bar$name
      } else {
        character(0)
      }
      
      # Get the selected variable from the dashboard control panel
      selected_variable <- if(!is.null(input$drop_var_id) && input$drop_var_id != "") {
        input$drop_var_id
      } else {
        "Total_ICPCPat_Pop"  # Default fallback
      }
      
      params <- list(
        n = selected_neighborhoods,
        selected_variable = selected_variable
      )
      report_env <- new.env(parent = .GlobalEnv)
      
      # Pass all necessary data to the report environment
      report_env$states_wkc <- states_wkc
      report_env$states_gmc <- states_gmc
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