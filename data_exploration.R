
# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)

# Data Importation --------------------------------------------------------

data_vidrio <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", dec = ".", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                Longitud = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                Latitud = as.numeric(sub(",", ".", Latitud, fixed = TRUE)))

data_puntos_limpios <- read.csv('data/200284-0-puntos-limpios.csv', sep = ";", stringsAsFactors = F)
data_contenedores_varios <- read.csv('data/Contenedores_varios.csv', sep = ";", dec = ",",stringsAsFactors = F)
data_puntos_limpios <- read.csv('data/ContenedoresRopa.csv', sep = ";", dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)) )
data_puntos_limpios <- read.csv('data/200284-0-puntos-limpios.csv', sep = ";", stringsAsFactors = F)

madrid_central <- rgdal::readOGR("data/Madrid_Central/Madrid_Central.shp",
                    layer = "Madrid_Central", GDAL1_integer64_policy = TRUE)
madrid_central <- sp::spTransform(madrid_central, sp::CRS("+init=epsg:4326"))

data_vidrio %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=Longitud, y=Latitud))


# Leaflet -----------------------------------------------------------------

icons <- leaflet::awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "orange"
)

leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  # leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$Wikimedia, group = "Tiles") %>%
  leaflet::addCircles(data = data_vidrio, lat = ~ Latitud, lng = ~ Longitud, label = ~Nombre, group = "Circles") %>%
  leaflet::addAwesomeMarkers(data = data_vidrio, lat = ~ Latitud, lng = ~ Longitud, label = ~Nombre,
                             icon = icons, labelOptions = leaflet::labelOptions(textsize = "15px"),
                             group = "AwesomeMarkers") %>%
  leaflet::addPolygons(data = madrid_central, group = "Madrid Central") %>% 
  leaflet::addLayersControl(
    overlayGroups = c("Tiles", "Circles", "AwesomeMarkers","Madrid Central"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )



# Mapdeck -----------------------------------------------------------------

library(mapdeck)
mapdeck::set_token(Sys.getenv("MAPBOX"))
mapdeck::mapdeck_tokens()
ms = mapdeck::mapdeck_style("streets")
mapdeck::mapdeck(style = ms, pitch = 0, location = c(-3.70, 40.45), zoom = 11, height = "400px", width = "400px") %>%
  mapdeck::add_scatterplot(
    layer_id = "ms",
    data = data, 
    lat = "Latitud", 
    lon = "Longitud",
    radius = 50,
    update_view = FALSE)
