# Language System for ELAN Dashboard
# This file contains all language-specific text and functions for internationalization

# Language dictionary for English
english_text <- list(
  # Navigation
  "dashboard_title" = "ELAN Dashboard",
  "wijken_tab" = "Neighborhoods",
  "gemeente_tab" = "Municipalities",
  
  # Control Panel
  "control_panel" = "Control Panel",
  "select_variable" = "Select a variable:",
  "select_year" = "Select a year:",
  "select_region" = "Select a region:",
  "select_gemeente" = "Select Municipality:",
  "clear_all" = "Clear All",
  
  # Variable Information
  "variable_information" = "Variable Information",
  "variable_definition" = "Variable Definition",
  "data_source" = "Data Source",
  
  # Charts and Maps
  "map_heading" = "Geographic Distribution",
  "map_subtext" = "Click on a neighborhood to select it for detailed analysis",
  "line_chart_heading" = "Trend Analysis",
  "line_chart_subtext" = "Temporal trends showing changes over time",
  "bar_chart_heading" = "Comparative Analysis",
  "bar_chart_subtext" = "Comparison of selected neighborhoods",
  "selected_wijk" = "Selected Neighborhood:",
  "generate_report" = "Generate Report",
  
  # Chart specific texts
  "ranking_text" = "Ranking - ",
  "in_text" = " in ",
  "at_text" = " at ",
  "overtime_text" = " overtime",
  "click_map_bar_trendline" = "Click on Map or Bar figure to add trendline(s) ",
  "click_wijk_trendline" = "Click on wijk to show the trendline",
  "click_gemeente_trendline" = "Click on gemeente to show the trendline",
  
  # Map and chart labels
  "gemeente_label" = "Gemeente",
  "wijk_label" = "Wijk",
  "total_population_label" = "Total Population",
  "value_label" = "Value",
  "year_label" = "Year",
  "per_wijk_label" = "(per Wijk)",
  "per_gemeente_label" = "(per Gemeente)",
  "legend_title" = "Legend",
  
  # Footer
  "about_us" = "About Us",
  "change_log" = "Change Log",
  "variables_explanation" = "Variables Explanation",
  "variables_definition" = "Variables Definition",
  "data_sources" = "Data Sources",
  
  # Navbar panel names
  "variables_definition_panel" = "Variables Definition",
  "data_dictionary_panel" = "Data Dictionary",
  "change_log_panel" = "Change Log",
  "about_us_panel" = "About Us",
  "last_updated" = "Last Updated August 2025",
  "copyright" = "© ELAN DCC. All rights reserved.",
  "partners_title" = "The Health Campus The Hague is an initiative of",
  
  # Language switcher
  "english" = "English",
  "dutch" = "Nederlands",
  
  # Tooltips
  "remove_neighborhood_tooltip" = "Remove a neighborhood by selecting/clicking a neighborhood then press Delete or Backspace key",
  "remove_item_info" = "Click the item (Wijk or Gemeente) and press delete or backspace button",
  
  # Variable information messages
  "no_definition_available" = "No definition available.",
  "select_variable_for_definition" = "Select a variable to see its definition.",
  "data_source_not_available" = "Data source information not available.",
  "select_variable_for_data_source" = "Select a variable to see its data source.",
  
  # Default regions
  "elan_area" = "ELAN area",
  "delft_area" = "Delft en omstreken",
  "leiden_area" = "Leiden en omstreken",
  "denhaag_area" = "s-gravenhage en omstreken",
  "hadoks_area" = "Hadoks' area"
)

# Language dictionary for Dutch
dutch_text <- list(
  # Navigation
  "dashboard_title" = "ELAN Dashboard",
  "wijken_tab" = "Wijken",
  "gemeente_tab" = "Gemeenten",
  
  # Control Panel
  "control_panel" = "Bedieningspaneel",
  "select_variable" = "Selecteer een variabele:",
  "select_year" = "Selecteer een jaar:",
  "select_region" = "Selecteer een regio:",
  "select_gemeente" = "Selecteer Gemeente:",
  "clear_all" = "Alles wissen",
  
  # Variable Information
  "variable_information" = "Variabele Informatie",
  "variable_definition" = "Variabele Definitie",
  "data_source" = "Gegevensbron",
  
  # Charts and Maps
  "map_heading" = "Geografische Verdeling",
  "map_subtext" = "Klik op een wijk om deze te selecteren voor gedetailleerde analyse",
  "line_chart_heading" = "Trend Analyse",
  "line_chart_subtext" = "Tijdelijke trends die veranderingen in de tijd tonen",
  "bar_chart_heading" = "Vergelijkende Analyse",
  "bar_chart_subtext" = "Vergelijking van geselecteerde wijken",
  "selected_wijk" = "Geselecteerde Wijk:",
  "generate_report" = "Rapport Genereren",
  
  # Chart specific texts
  "ranking_text" = "Rangschikking - ",
  "in_text" = " in ",
  "at_text" = " in ",
  "overtime_text" = " in de loop van de tijd",
  "click_map_bar_trendline" = "Klik op Kaart of Balk figuur om trendlijn(en) toe te voegen",
  "click_wijk_trendline" = "Klik op wijk om de trendlijn te tonen",
  "click_gemeente_trendline" = "Klik op gemeente om de trendlijn te tonen",
  
  # Map and chart labels
  "gemeente_label" = "Gemeente",
  "wijk_label" = "Wijk",
  "total_population_label" = "Totaal aantal inwoners",
  "value_label" = "Waarde",
  "year_label" = "Jaar",
  "per_wijk_label" = "(per Wijk)",
  "per_gemeente_label" = "(per Gemeente)",
  "legend_title" = "Legenda",
  
  # Footer
  "about_us" = "Over Ons",
  "change_log" = "Wijzigingslog",
  "variables_explanation" = "Variabelen Uitleg",
  "variables_definition" = "Variabelen Definitie",
  "data_sources" = "Gegevensbronnen",
  
  # Navbar panel names
  "variables_definition_panel" = "Variabelen Definitie",
  "data_dictionary_panel" = "Gegevenswoordenboek",
  "change_log_panel" = "Wijzigingslog",
  "about_us_panel" = "Over Ons",
  "last_updated" = "Laatst bijgewerkt Augustus 2025",
  "copyright" = "© ELAN DCC. Alle rechten voorbehouden.",
  "partners_title" = "De Health Campus Den Haag is een initiatief van",
  
  # Language switcher
  "english" = "English",
  "dutch" = "Nederlands",
  
  # Tooltips
  "remove_neighborhood_tooltip" = "Verwijder een wijk door deze te selecteren/klikken en vervolgens Delete of Backspace te drukken",
  "remove_item_info" = "Klik op het item (Wijk of Gemeente) en druk op delete of backspace toets",
  
  # Variable information messages
  "no_definition_available" = "Geen definitie beschikbaar.",
  "select_variable_for_definition" = "Selecteer een variabele om de definitie te zien.",
  "data_source_not_available" = "Gegevensbron informatie niet beschikbaar.",
  "select_variable_for_data_source" = "Selecteer een variabele om de gegevensbron te zien.",
  
  # Default regions
  "elan_area" = "ELAN gebied",
  "delft_area" = "Delft en omstreken",
  "leiden_area" = "Leiden en omstreken",
  "denhaag_area" = "s-gravenhage en omstreken",
  "hadoks_area" = "Hadoks' gebied"
)

# Function to get text based on current language
get_text <- function(key, language = "en") {
  # Handle NULL or empty language parameter
  if (is.null(language) || length(language) == 0) {
    language <- "en"
  }
  
  if (language == "nl") {
    return(ifelse(is.null(dutch_text[[key]]), key, dutch_text[[key]]))
  } else {
    return(ifelse(is.null(english_text[[key]]), key, english_text[[key]]))
  }
}

# Function to update area dictionary based on language
get_area_dict <- function(language = "en") {
  if (language == "nl") {
    return(list(
      "s-gravenhage" = "s-Gravenhage",
      'Leiden' = 'Leiden',
      "Lisse" = "Lisse",
      'Leidschendam-Voorburg' = 'Leidschendam-Voorburg',
      'Wassenaar' = 'Wassenaar',
      'Zoetermeer' = 'Zoetermeer',
      "s-gravenhage en omstreken" = "s-gravenhage en omstreken",
      "Leiden en omstreken" = 'Leiden en omstreken',
      'Delft en omstreken' = 'Delft en omstreken',
      'ELAN area' = 'ELAN gebied',
      "Hadoks' area" = "Hadoks' gebied"
    ))
  } else {
    return(area_dict)  # Use the original English area_dict
  }
}

# Function to update variable dictionary based on language
get_var_dict <- function(language = "en") {
  if (language == "nl") {
    # Create Dutch variable dictionary
    dutch_var_dict <- list()
    for (category in names(var_dict)) {
      dutch_var_dict[[category]] <- list()
      for (var_name in names(var_dict[[category]])) {
        # Get Dutch label from the Dutch label dictionary
        dutch_label <- ifelse(is.null(var_def_label_NL_dict[[var_name]]), var_name, var_def_label_NL_dict[[var_name]])
        dutch_var_dict[[category]][[dutch_label]] <- var_name
      }
    }
    return(dutch_var_dict)
  } else {
    return(var_dict)  # Use the original English var_dict
  }
} 