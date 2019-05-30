# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)

# Data Importation --------------------------------------------------------
count.na <- function(df){
  round(apply(df,2,function(i){sum(!is.na(i))})/nrow(df),2)
}

# Distritos y barrios
data_barrios <- read.csv('data/CALLEJERO_VIGENTE_BARRIOS_201809.csv', sep=';')
barrios_shp <- rgdal::readOGR("data/Barrios_ETRS89/BARRIOS.shp", GDAL1_integer64_policy = TRUE)
barrios_shp <- sp::spTransform(barrios_shp, sp::CRS("+init=epsg:4326"))
barrios_shp@data <- barrios_shp@data %>% 
  dplyr::mutate(COLOR = sample(viridis::plasma(n())),
                LABEL = paste0("<b>Barrio:</b> ", NOMBRE))


data_distritos <- read.csv('data/CALLEJERO_VIGENTE_DISTRITOS_201809.csv', sep=';')
distritos_shp <- rgdal::readOGR("data/Distritos_ETRS89/DISTRITOS.shp", GDAL1_integer64_policy = TRUE)
distritos_shp <- sp::spTransform(distritos_shp, sp::CRS("+init=epsg:4326"))
distritos_shp@data <- distritos_shp@data %>% 
  dplyr::mutate(COLOR = sample(viridis::plasma(n())),
                AREA_KM = round(SHAPE_area/1e6,2),
                LABEL = paste0("<b>Distrito:</b> ", NOMBRE, "</br><b>Superficie:</b> ", AREA_KM, "km<sup>2</sup>"))

# Madrid Central
madrid_central <- rgdal::readOGR("data/Madrid_Central/Madrid_Central.shp", GDAL1_integer64_policy = TRUE)
madrid_central <- sp::spTransform(madrid_central, sp::CRS("+init=epsg:4326"))
madrid_central_data <- sp::coordinates(madrid_central)[[1]][[1]] %>% 
  as.data.frame() %>% 
  `colnames<-`(c("longitude","latitude"))
inside.madrid.central <- function(longitude, latitude, madrid_central_data){
  sp::point.in.polygon(longitude, latitude, 
                       madrid_central_data$longitude, 
                       madrid_central_data$latitude)
}

# Contenedores Aceite
data_contenedores_aceite <- read.csv('data/RecogidaContenedoresAceiteUsado.csv', sep = ";",  dec = ",", stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(HORARIO = stringi::stri_wrap(HORARIO, 50) %>% paste(collapse = "</br>"),
                OBSERVACIONES = stringi::stri_wrap(OBSERVACIONES, 50) %>% paste(collapse = "</br>")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(OBSERVACIONES = ifelse(nchar(OBSERVACIONES)>1, OBSERVACIONES, NA),
                LABEL = paste("<b>Dirección:</b> ", DIRECCION_COMPLETA, 
                              "<br/><b>Centro:</b> ", CENTRO, 
                              "<br/><b>Horario:</b> ", HORARIO, ifelse(!is.na(OBSERVACIONES), paste(",</br>", OBSERVACIONES), "") ))

# Contenedores Ropa
data_contenedores_ropa <- read.csv('data/ContenedoresRopa.csv', sep = ";", dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)),
                CENTRO = ifelse(nchar(CENTRO)>1, CENTRO, NA),
                HORARIO = ifelse(nchar(HORARIO)>1, HORARIO, NA),
                LABEL = paste("<b>Dirección:</b> ", DIRECCION_COMPLETA, 
                              ifelse(!is.na(CENTRO), paste("<br/><b>Centro:</b> ", CENTRO), ""),
                              ifelse(!is.na(HORARIO), paste("<br/><b>Horario:</b> ", HORARIO), "") ))

# Contenedores Pilas
data_contenedores_pilas <- read.csv('data/Marquesinas_contenedores_pilas_2017.csv', sep = ";",  dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", Longitud)),
                LATITUD = as.numeric(sub("\\s", "", Latitud)),
                VIAL = ifelse(nchar(VIAL)>1, stringr::str_to_title(VIAL), NA),
                Nº = ifelse(nchar(Nº)>1, stringr::str_to_title(Nº), NA),
                DIRECCIÓN = ifelse(nchar(DIRECCIÓN)>1, stringr::str_to_title(DIRECCIÓN), NA),
                emplazamiento = ifelse(nchar(emplazamiento)>1, stringr::str_to_title(emplazamiento), NA),
                sentido = ifelse(nchar(sentido)>1, stringr::str_to_title(sentido), NA),
                LABEL = paste0("<b>Dirección:</b> ", ifelse(!is.na(VIAL), VIAL, ""), " ", DIRECCIÓN, " ", ifelse(!is.na(Nº), Nº, ""), 
                               ifelse(!is.na(emplazamiento), paste("</br><b>Emplazamiento:</b> ", emplazamiento), ""), 
                               ifelse(!is.na(sentido), paste("</br><b>Sentido:</b> ", sentido), ""))
  )

# Contenedores Varios
data_contenedores_varios <- read.csv('data/Contenedores_varios.csv', sep = ";", dec = ",",stringsAsFactors = F)
data_contenedores_varios_coords <- data_contenedores_varios %>% 
  dplyr::select(COORDENADA.X, COORDENADA.Y) %>% 
  sp::SpatialPoints(proj4string=sp::CRS("+proj=utm +datum=WGS84")  ) %>% 
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
data_contenedores_varios <- data_contenedores_varios %>% 
  dplyr::mutate( LATITUD = data_contenedores_varios_coords@coords[,1],
                 LONGITUD = data_contenedores_varios_coords@coords[,2])

data_contenedores_varios %>% 
  filter(Tipo.Contenedor == "VIDRIO")

# Contenedores Vidrio
data_contenedores_vidrio <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", dec = ".", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                LONGITUD = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                LATITUD = as.numeric(sub(",", ".", Latitud, fixed = TRUE)),
                Nombre = stringr::str_to_title(Nombre),
                LABEL = paste0("<b>Dirección:</b> ", Nombre))

# Puntos Limpios Fijos
data_puntos_limpios_fijos <- read.csv('data/200284-0-puntos-limpios.csv', sep = ";", stringsAsFactors = F) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(HORARIO = stringi::stri_wrap(HORARIO, 50) %>% paste(collapse = "</br>")) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)),
                CLASE.VIAL = stringr::str_to_title(CLASE.VIAL),
                NOMBRE.VIA = stringr::str_to_title(NOMBRE.VIA),
                DISTRITO = stringr::str_to_title(DISTRITO),
                BARRIO = stringr::str_to_title(BARRIO),
                LABEL = paste0("<b>Dirección:</b> ", CLASE.VIAL, " ", NOMBRE.VIA, " ", NUM, 
                               "</br><b>Distrito:</b> ", DISTRITO, 
                               "</br><b>Barrio:</b> ", BARRIO, 
                               "</br><b>Horario:</b> ", HORARIO))

# Puntos Limpios Moviles
data_puntos_limpios_moviles <- read.csv('data/PuntosLimpiosMoviles.csv', sep = ";",  dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LONGITUD = as.numeric(sub("\\s", "", LONGITUD)),
                LATITUD = as.numeric(sub("\\s", "", LATITUD)),
                DÍA.SEMANA = stringr::str_to_title(DÍA.SEMANA),
                TURNO = stringr::str_to_lower(TURNO),
                LABEL = paste0("<b>Dirección:</b> ", DIRECCION_COMPLETA, 
                               "</br><b>Horario:</b> ", DÍA.SEMANA, " por la ", TURNO, " de ", HORA.INICIO, " a ", HORA.FINAL))

# Puntos Limpios Proximidad
data_puntos_limpios_proximidad <- read.csv('data/PUNTOS_LIMPIOS_PROXIMIDAD.csv', sep = ";",  dec = ",", stringsAsFactors = F) %>% 
  dplyr::mutate(LABEL = paste0("<b>Dirección:</b> ", DIRECCION_COMPLETA, 
                               "<br/><b>Centro:</b> ", CENTRO,
                               "<br/><b>Horario:</b> ", HORARIO))



# Leaflet -----------------------------------------------------------------

icon_contenedores_aceite <- leaflet::makeIcon( iconUrl = "icons/oil.png", iconWidth = 15, iconHeight = 15)
icon_contenedores_ropa <- leaflet::makeIcon( iconUrl = "icons/hanger.png", iconWidth = 15, iconHeight = 15)
icon_contenedores_pilas <- leaflet::makeIcon( iconUrl = "icons/battery.png", iconWidth = 13, iconHeight = 13)
icon_contenedores_vidrio <- leaflet::makeIcon( iconUrl = "icons/glass-container.png", iconWidth = 13, iconHeight = 13)
icon_puntos_limpios_fijos <- leaflet::makeIcon( iconUrl = "icons/trash.png", iconWidth = 15, iconHeight = 15)
icon_puntos_limpios_moviles <- leaflet::makeIcon( iconUrl = "icons/garbage-truck.png", iconWidth = 17, iconHeight = 17)
icon_puntos_limpios_proximidad <- leaflet::makeIcon( iconUrl = "icons/delete.png", iconWidth = 15, iconHeight = 15)

data_exploring_map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
  leaflet::addProviderTiles(leaflet::providers$Wikimedia, group = "Tiles") %>%
  leaflet::addMarkers(data = data_contenedores_aceite, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Aceite", 
                      icon = icon_contenedores_aceite, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addMarkers(data = data_contenedores_ropa, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Ropa", 
                      icon = icon_contenedores_ropa, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addMarkers(data = data_contenedores_pilas, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Pilas",
                      icon = icon_contenedores_pilas, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addMarkers(data = data_contenedores_vidrio, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Contenedores Vidrio",
                      icon = icon_contenedores_vidrio, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addMarkers(data = data_puntos_limpios_fijos, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Puntos Limpios Fijos",
                      icon = icon_puntos_limpios_fijos, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addMarkers(data = data_puntos_limpios_moviles, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Puntos Limpios Moviles",
                      icon = icon_puntos_limpios_moviles, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addMarkers(data = data_puntos_limpios_proximidad, lat = ~ LATITUD, lng = ~ LONGITUD, group = "Puntos Limpios Proximidad",
                      icon = icon_puntos_limpios_proximidad, label = ~lapply(LABEL, htmltools::HTML)) %>%
  leaflet::addPolygons(data = madrid_central, group = "Madrid Central",
                       color = "#1c1c1c", weight = 3, opacity = 0.4, 
                       fillColor = "#1c1c1c", fillOpacity = 0.2, dashArray="4") %>% 
  leaflet::addPolygons(data = distritos_shp, group = "Distritos",
                       label = ~lapply(LABEL, htmltools::HTML), stroke = F,
                       fillColor = ~COLOR, fillOpacity = 0.5, dashArray="4",
                       highlight = leaflet::highlightOptions(weight = 5, color = "#666",dashArray = "",fillOpacity = 0.8,bringToFront = TRUE)) %>% 
  leaflet::addPolygons(data = barrios_shp, group = "Barrios",
                       label = ~lapply(LABEL, htmltools::HTML), stroke = F,
                       fillColor = ~COLOR, fillOpacity = 0.5, dashArray="4",
                       highlight = leaflet::highlightOptions(weight = 5, color = "#666",dashArray = "",fillOpacity = 0.8,bringToFront = TRUE)) %>% 
  leaflet::addLayersControl(
    overlayGroups = c("Distritos", "Barrios", "Madrid Central"),
    baseGroups =  c("Contenedores Aceite", "Contenedores Ropa", "Contenedores Pilas", "Contenedores Vidrio",
                    "Puntos Limpios Fijos", "Puntos Limpios Moviles", "Puntos Limpios Proximidad"),
    position = "topright", 
    options = leaflet::layersControlOptions(collapsed = F)
  ) %>% 
  leaflet::addMiniMap(position = "bottomleft", toggleDisplay = T, minimized = T) %>% 
  leaflet::hideGroup(group = c("Contenedores Aceite", "Contenedores Ropa", "Contenedores Pilas", "Contenedores Vidrio",
                               "Puntos Limpios Fijos", "Puntos Limpios Moviles", "Puntos Limpios Proximidad")) %>% 
  leaflet::setView(lat = 40.416673, lng = -3.703803, zoom = 13) %>% 
  leaflet::addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")


