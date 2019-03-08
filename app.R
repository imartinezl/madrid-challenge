
# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)

# Data Importation --------------------------------------------------------

# Contenedores Aceite
data_contenedores_aceite <- read.csv('data/RecogidaContenedoresAceiteUsado.csv', sep = ";",  dec = ",", stringsAsFactors = F)

# Contenedores Ropa
data_contenedores_ropa <- read.csv('data/ContenedoresRopa.csv', sep = ";", dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)) )

# Contenedores Pilas
data_contenedores_pilas <- read.csv('data/Marquesinas_contenedores_pilas_2017.csv', sep = ";",  dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", Longitud)),
                LATITUD = as.numeric(sub("\\s", "", Latitud)) )

# Contenedores Varios
data_contenedores_varios <- read.csv('data/Contenedores_varios.csv', sep = ";", dec = ",",stringsAsFactors = F)
data_contenedores_varios_coords <- data_contenedores_varios %>% 
  dplyr::select(COORDENADA.X, COORDENADA.Y) %>% 
  sp::SpatialPoints(proj4string=sp::CRS("+proj=utm +datum=WGS84")  ) %>% 
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
data_contenedores_varios <- data_contenedores_varios %>% 
  dplyr::mutate( LATITUD = data_contenedores_varios_coords@coords[,1],
                 LONGITUD = data_contenedores_varios_coords@coords[,2])
  
# Contenedores Vidrio
data_contenedores_vidrio <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", dec = ".", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                LONGITUD = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                LATITUD = as.numeric(sub(",", ".", Latitud, fixed = TRUE)))

# Puntos Limpios Fijos
data_puntos_limpios_fijos <- read.csv('data/200284-0-puntos-limpios.csv', sep = ";", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)) )

# Puntos Limpios Moviles
data_puntos_limpios_moviles <- read.csv('data/PuntosLimpiosMoviles.csv', sep = ";",  dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)) )

# Puntos Limpios Proximidad
data_puntos_limpios_proximidad <- read.csv('data/PUNTOS_LIMPIOS_PROXIMIDAD.csv', sep = ";",  dec = ",", stringsAsFactors = F)
  
madrid_central <- rgdal::readOGR("data/Madrid_Central/Madrid_Central.shp",
                    layer = "Madrid_Central", GDAL1_integer64_policy = TRUE)
madrid_central <- sp::spTransform(madrid_central, sp::CRS("+init=epsg:4326"))

# Leaflet -----------------------------------------------------------------

icons <- leaflet::awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "orange"
)

data_exploring_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  # leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$Wikimedia, group = "Tiles") %>%
  leaflet::addCircles(data = data_contenedores_aceite, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Aceite") %>%
  leaflet::addCircles(data = data_contenedores_ropa, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Ropa") %>%
  leaflet::addCircles(data = data_contenedores_pilas, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Pilas") %>%
  # leaflet::addCircles(data = data_contenedores_varios, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Varios") %>%
  leaflet::addCircles(data = data_contenedores_vidrio, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Vidrio") %>%
  leaflet::addCircles(data = data_puntos_limpios_fijos, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Puntos Limpios Fijos") %>%
  leaflet::addCircles(data = data_puntos_limpios_moviles, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Puntos Limpios Moviles") %>%
  leaflet::addCircles(data = data_puntos_limpios_proximidad, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Puntos Limpios Proximidad") %>%
  # leaflet::addAwesomeMarkers(data = data_contenedores_vidrio, lat = ~ LATITUD, lng = ~ LONGITUD, label = ~Nombre,
  #                            icon = icons, labelOptions = leaflet::labelOptions(textsize = "15px"),
  #                            group = "AwesomeMarkers") %>%
  leaflet::addPolygons(data = madrid_central, group = "Madrid Central") %>% 
  leaflet::addLayersControl(
    overlayGroups = c("Contenedores Aceite", "Contenedores Ropa", "Contenedores Pilas", "Contenedores Vidrio",
                      "Puntos Limpios Fijos", "Puntos Limpios Moviles", "Puntos Limpios Proximidad", "Madrid Central"),
    position = "topright",
    options = leaflet::layersControlOptions(collapsed = F)
  ) %>% 
  leaflet::addMiniMap(position = "bottomleft", toggleDisplay = T, minimized = T) %>% 
  leaflet::hideGroup(group = c("Contenedores Aceite", "Contenedores Ropa", "Contenedores Pilas",
                               "Puntos Limpios Fijos", "Puntos Limpios Moviles", "Puntos Limpios Proximidad")) %>% 
  leaflet::addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")
  



# Shiny -------------------------------------------------------------------

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("united"),
  leaflet::leafletOutput("mymap")
  
)

server <- function(input, output, session) {
  
  output$mymap <- leaflet::renderLeaflet({
    data_exploring_map
  })
  
}

shiny::shinyApp(ui, server)
