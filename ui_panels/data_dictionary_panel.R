# Data Dictionary Panel
# This file contains the UI for the Data Dictionary page

data_dictionary_panel <- function() {
  nav_panel(uiOutput("data_dictionary_panel_title"),
    div(
      class = "page-container",
      h1("ELAN-GP", class = "page-heading"),
      div(
        class = "page-description",
        p("ELAN-H/GP (Huisartsen) is een uniek huisartsen-netwerk met (op dit moment) meer dan 140 deelnemende huisartsen en data van meer dan 800.000 patiënten in de ELAN-huisartsendata. Via ELAN-H zijn inmiddels tientallen onderzoeken verricht en we hopen dat in de komende jaren voort te kunnen zetten.")
      ),
      div(
        class = "button-container",
        tags$button(
          onclick = "window.open('https://elan-dcc.github.io/researchers/gp_data/', '_blank')",
          class = "yellow-button",
          "ELAN-H"
        )
      ),
      div(
        class = "table-container",
        DT::dataTableOutput("table_h", width = "100%")
      ),
      
      h1("ELAN-CBS", class = "section-heading"),
      div(
        class = "page-description",
        p("Gezond en Gelukkig Den Haag (GGDH) wordt door de afdeling PHEG op de LUMC-Campus Den Haag en het LUMC ondersteund met de data-infrastructuur van ELAN-CBS. In ELAN-CBS delen niet alleen huisartsen, maar ook de ziekenhuizen, de gemeente en in de toekomst ook andere zorgverleners gepseudonimiseerde gegevens met het CBS als \"trusted third party\". Dankzij het CBS kunnen deze gepseudonimiseerde gegevens nog verder worden verrijkt met gegevens die het CBS al onder haar hoede heeft, zoals huishoudsamenstelling en inkomen.")
      ),
      div(
        class = "button-container",
        tags$button(
          onclick = "window.open('https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/catalogus-microdata', '_blank')",
          class = "yellow-button",
          "ELAN-CBS"
        )
      ),
      div(
        class = "table-container",
        DT::dataTableOutput("table_cbs", width = "100%")
      )
    )
  )
} 