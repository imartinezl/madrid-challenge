# Data Exploration

library(dplyr)
library(zeallot)

data <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                Longitud = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                Latitud = as.numeric(sub(",", ".", Latitud, fixed = TRUE)))

a <- rgdal::readOGR("data/Madrid_Central/Madrid_Central.shp",
                    layer = "Madrid_Central", GDAL1_integer64_policy = TRUE)
mylines <- sp::spTransform(a, sp::CRS("+init=epsg:4326"))

data %>% 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=Longitud, y=Latitud))


library(leaflet)
icons <- leaflet::awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "orange"
)

epsg32630 <- leaflet::leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:32630",
  proj4def = a@proj4string@projargs,
  bounds = a@bbox,
  resolutions = 2^(16:7))


leaflet::leaflet(data = data, options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  # leaflet::addTiles() %>%
  leaflet::addProviderTiles(leaflet::providers$Wikimedia, group = "Tiles") %>%
  leaflet::addCircles(lat = ~ Latitud, lng = ~ Longitud, group = "Circles") %>%
  leaflet::addAwesomeMarkers( lat = ~ Latitud, lng = ~ Longitud, label = ~Nombre,
                             icon = icons, labelOptions = leaflet::labelOptions(textsize = "15px"),
                             group = "AwesomeMarkers") %>%
  leaflet::addPolygons(data = mylines, group = "Madrid Central") %>% 
  leaflet::addLayersControl(
    overlayGroups = c("Tiles", "Circles", "AwesomeMarkers","Madrid Central"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )



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
