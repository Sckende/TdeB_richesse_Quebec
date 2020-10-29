
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)

getwd()
occ_qc <- read.csv("./local_data/occ_qc.csv")
head(occ_qc)

CR01_sf <- sf::read_sf("./local_data", layer = "CR01_sf")

CR01_sf <- sf::read_sf("./local_data", layer = "CR01_sf")
# ----------  Création d'une fonction pour obtenir une couleur par province
pal <- colorFactor("Dark2", domain = CR01_sf$NOM_PROV_N)

# ----------  Création des pop-ups par province
labels <- sprintf( #Use C-style String Formatting Commands
  "<strong>%s</strong>",
  CR01_sf$NOM_PROV_N
) %>% lapply(htmltools::HTML)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(width = 4,
          height = 900,
          selectInput("sta",
                      "Statut de l'espèce",
                      c("Tous", "Éteint à l'état sauvage - EW", "En danger critique - CR", "En danger - EN", "Vulnérable - VU", "Quasi menacé - NT", "Préoccupation mineure - LC", "Données insuffisantes - DD", "Non évalué - NE")),
          uiOutput("statut")),
      box(width = 8,
          height = 900,
          leafletOutput("map", height = 880))
    ),
    fluidRow(
      box(width = 6,
          height = "auto",
          dataTableOutput("donnees"),
          downloadButton("DL_data", "Télécharger"))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Sélection des espèces par statut
  # occ_stat = reactive({
  #   occ_qc[occ_qc$xxxx == input$sta,]
  # })
  
  # Carte provinces naturelles du Québec
  
  output$map <- renderLeaflet({
    leaflet(CR01_sf) %>%
      addTiles() %>% # Affichage du fond de carte
      addPolygons(color = "white", # couleur des limites des polygones
                  weight = 1,
                  smoothFactor = 0.5,
                  fillColor = ~pal(NOM_PROV_N), # couleur du remplissage des polygones
                  fillOpacity = 0.6,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 4,
                                                      fillOpacity = 0.7,
                                                      bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions("font-weight" = "normal",
                                              padding = "3px 8px",
                                              textsize = "15px",
                                              direction = "auto")) %>%
      addMarkers(lng = occ_qc$lon,
                 lat = occ_qc$lat,
                 clusterOptions = markerClusterOptions()
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

