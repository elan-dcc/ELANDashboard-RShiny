# About Us Panel
# This file contains the UI for the About Us page

about_us_panel <- function() {
  nav_panel(uiOutput("about_us_panel_title"),
    div(
      class = "page-container",
      h1("About the Dashboard", class = "page-heading"),
      div(
        class = "page-description",
        p("Dit dashboard is gemaakt om onderzoekers, gezondheidswerkers en beleidsmakers in de publieke gezondheidssector te verbeteren en te betrekken. Elke dashboardpagina vertegenwoordigt verschillende onderzoeken die we hebben gedaan op Health Campus Den Haag en LUMC met behulp van gegevens uit onze ELAN-dataomgeving."),
        p("Neem voor meer informatie over het dashboard contact met ons op:"),
        p(HTML("<strong>ELAN.DCC@lumc.nl</strong>"))
      ),
      h1("ELAN", class = "section-heading"),
      div(
        class = "page-description",
        p("Binnen ELAN werken zorgverleners uit de regio samen met de afdeling Public Health en Eerstelijnsgeneeskunde en de Health Campus Den Haag van het LUMC aan ondersteuning, vernieuwing en verbetering van de zorg."),
        p("ELAN-H/GP (Huisartsen) is een uniek huisartsen-netwerk met (op dit moment) meer dan 140 deelnemende huisartsen en data van meer dan 800.000 patiënten in de ELAN-huisartsendata. Via ELAN-H zijn inmiddels tientallen onderzoeken verricht en we hopen dat in de komende jaren voort te kunnen zetten."),
        p("Voor meer informatie:"),
        div(
          class = "button-container",
          tags$button(
            onclick = "window.open('https://www.lumc.nl/over-het-lumc/partners/elan/', '_blank')",
            class = "yellow-button",
            "ELAN"
          ),
          tags$button(
            onclick = "window.open('https://www.elanresearch.nl', '_blank')",
            class = "yellow-button",
            "ELAN Research"
          ),
          tags$button(
            onclick = "window.open('https://www.lumc.nl/siteassets/over-het-lumc/partners/elan/data-governance.pdf', '_blank')",
            class = "yellow-button",
            "ELAN Data Governance"
          )
        )
      )
      # Commented out sections for future use
      # h1("Hadoks / Supply and Demand Themes", class = "section-heading"),
      # div(
      #   class = "page-description",
      #   p("Dashboard gemaakt in samenwerking met Hadoks")
      # ),
      # h1("Hartfalen Themes", class = "section-heading"),
      # div(
      #   class = "page-description",
      #   p("Dashboard in samenwerking met hart- en vaatonderzoeksgroep")
      # )
    )
  )
} 