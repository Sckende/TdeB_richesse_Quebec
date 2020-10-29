# make_local_data file
library(leaflet)
library(sp)
library(sf)
library(rgdal)
library(rmapshaper)
library(spatialEco)

#### Chargement des données ####
# ---------- Chargement des polygones
CR01 <- rgdal::readOGR("./source_data", layer = "CR_NIV_01_S", stringsAsFactors = FALSE, encoding = "UTF-8")
CR01 <- sp::spTransform(CR01, CRS("+proj=longlat +datum=WGS84 +no_defs"))

str(CR01, max.level = 3)


# ---------- Chargement des occurences d'espèces
occ <- readRDS("./source_data/tmp.rds")

#### Provinces naturelles ####

# ----------  Création d'une fonction pour obtenir une couleur par province
pal <- leaflet::colorFactor("Dark2", domain = CR01$NOM_PROV_N)

# ----------  Création des pop-ups par province
labels <- sprintf( #Use C-style String Formatting Commands
  "<strong>%s</strong>",
  CR01$NOM_PROV_N
) %>% lapply(htmltools::HTML)

# ---------- Association des provinces naturelles avec les occurences
# Intersection occurences et polygones
sp::coordinates(occ) <- ~lon+lat # création d'un objet de classe SpatialPointDataFrame
proj4string(occ) <- "+proj=longlat +datum=WGS84 +no_defs" # Définir le CRS des occurences
pts_in_poly <- spatialEco::point.in.poly(occ, CR01) # Association d'une province naturelle (ou NA si en dehors) et de tous ses attributs pour chaque occurence
head(pts_in_poly@data)
pts_in_poly@data$NOM_PROV_N <- as.factor(pts_in_poly@data$NOM_PROV_N)
summary(pts_in_poly@data)

occ_qc <- occ[!is.na(pts_in_poly@data$NOM_PROV_N),] # Retrait des occurences en dehors du Québec

occ_qc <- sf::st_as_sf(occ_qc) # Conversion d'un objet classe sp vers un objet classe sf

# ---------- Réduction de la résolution des polygones pour augmenter la vitesse
CR01_sf <- sf::st_as_sf(CR01)
CR01_sf <- rmapshaper::ms_simplify(CR01_sf, keep=.01)

#### Stockage des données traitées dans le fichier "local_data" ####

sf::write_sf(occ_qc, "./local_data/occ_qc.csv")
sf::write_sf(CR01_sf, "./local_data/CR01_sf.shp")
