
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(iNEXT)
source("./functions.R")

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
      # box(width = 4,
      #     height = 900,
      #     selectInput("sta",
      #                 "Statut de l'espèce",
      #                 c("Tous", "Éteint à l'état sauvage - EW", "En danger critique - CR", "En danger - EN", "Vulnérable - VU", "Quasi menacé - NT", "Préoccupation mineure - LC", "Données insuffisantes - DD", "Non évalué - NE")),
      #     uiOutput("statut")),
      box(width = 4,
          height = 900,
              h3("Analyses de raréfaction pour le Québec"),
              h4("Richesse spécifique par groupe taxonomique"),
              tableOutput("richesse_qc"),
              h4("Occurences des espèces par groupe taxonomique"),
              selectInput("grou",
                          "",
                          c("Tous", unique(occ_qc$species_gr)),
                          selected = "Tous"),
              h4("Richesse spécifique par région"),
              h5("(Sélectionner une région sur la carte)"),
              tableOutput("richesse_locale")
          ),
      box(width = 8,
          height = 900,
          leafletOutput("map", height = 880))
    ),
    fluidRow(
      box(width = 12,
          height = "auto",
          dataTableOutput("donnees"),
          downloadButton("DL_data", "Télécharger"))
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  # ---------- Listes déroulantes
  
  # Sélection des espèces par statut
  # occ_stat = reactive({
  #   occ_qc[occ_qc$xxxx == input$sta,]
  # })
  
  #Sélection des espèces par groupe
  occ_grou = reactive({
    if(input$grou == "Tous"){
      occ_qc
    }else{
      occ_qc[occ_qc$species_gr == input$grou,]  
    }
    
  })
  
  # Carte provinces naturelles du Québec
  
  output$map <- renderLeaflet({
    leaflet(CR01_sf) %>%
      addTiles() %>% # Affichage du fond de carte
      addPolygons(color = "white", # couleur des limites des polygones
                  weight = 1,
                  smoothFactor = 0.5,
                  layerId = ~NOM_PROV_N,
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
      addMarkers(lng = occ_grou()$lon,
                 lat = occ_grou()$lat,
                 clusterOptions = markerClusterOptions()
      )
  })
########################################
## Analyse de raréfaction pour le Qc ##
#######################################

  output$richesse_qc <- renderTable(rare_shiny(occ_qc))
  ###########################
  ## Click on a map polygon ##
  ###########################
  
  observe({
  event <- input$map_shape_click
  print(event)
  if(!is.null(event$id)){
  output$richesse_locale <- renderTable(rare_shiny(occ_qc[occ_qc$NOM_PROV_N == event$id,]))
  output$donnees <- renderDataTable(occ_qc[occ_qc$NOM_PROV_N == event$id, c(2:7, 10)])
  }
  # ----------Obtention des données à télécharger

  output$DL_data <- downloadHandler(
    filename = function() {
      paste(event$id, '.csv', sep="")
    },
    content = function(file) {
      write.csv(occ_qc[occ_qc$NOM_PROV_N == event$id,], file)
    }
  )
})

}

# Run the application 
shinyApp(ui = ui, server = server)

