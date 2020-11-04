
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
      box(width = 3,
          height = 900,
          radioButtons("Statut", 
                       label = h3("Sélectionner le statut de précarité"), 
                       choices = list("Toutes les espèces" = 1, 
                                      "Abondantes"  = 2, 
                                      "Susceptibles" = 3,
                                      "Menacées" = 4,
                                      "Vulnérables" = 5
                       ), 
                       selected = 1),
          uiOutput("statut"),
          radioButtons("grou",
                      label = h3("Sélectionner le groupe taxonomique"),
                      c("Tous", unique(occ_qc$species_gr)),
                      selected = "Tous")),
      box(width = 6,
          height = 900,
          leafletOutput("map", height = 880)),
      box(width = 3,
          height = 900,
              h3("Analyses de raréfaction pour le Québec"),
              h4("Richesse spécifique par groupe taxonomique"),
              tableOutput("richesse_qc"),
              h3("Richesse spécifique par région"),
              h5("(Sélectionner une région sur la carte)"),
              tableOutput("richesse_locale")
          )),
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
  if(is.null(event$id)){
    mess <- unique(occ_qc[, c("scientific_name", "species_gr")])
    message <- mess[order(mess$scientific_name),]
    names(message) <- c("espèce", "groupe taxonomique")
    output$donnees <- renderDataTable(message)
  }else{
    mess <- occ_qc[occ_qc$NOM_PROV_N == event$id,]
    message <- unique(mess[c("scientific_name", "species_gr")])
    message <- message[order(message$scientific_name),]
    names(message) <- c("espèce", "groupe taxonomique")
    
    output$richesse_locale <- renderTable(rare_shiny(mess))
    output$donnees <- renderDataTable(message)
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

