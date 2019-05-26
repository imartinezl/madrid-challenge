# Route Optimization

# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)
source('route_request.R')
source('route_create.R')
source('route_opt_fnc.R')

# Data Importation --------------------------------------------------------

# Madrid Central
madrid_central <- rgdal::readOGR("data/Madrid_Central/Madrid_Central.shp", GDAL1_integer64_policy = TRUE)
madrid_central <- sp::spTransform(madrid_central, sp::CRS("+init=epsg:4326"))

# Contenedores Vidrio
data_contenedores_vidrio <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", dec = ".", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                LONGITUD = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                LATITUD = as.numeric(sub(",", ".", Latitud, fixed = TRUE)),
                Nombre = stringr::str_to_title(Nombre),
                LABEL = paste0("<b>Dirección:</b> ", Nombre))

points <- data_contenedores_vidrio %>% 
  dplyr::slice(1:5) %>% 
  dplyr::select(LONGITUD, LATITUD) %>% 
  plyr::rename(c("LONGITUD"="longitude", "LATITUD"="latitude"))


# Graph: Edges & Points ---------------------------------------------------

edges_haversine <- edges.haversine(points)
edges_route <- edges.route(points, edges_haversine)
edges_summary <- edges.summary(edges_route)
edges.plot(points, edges_route)

distance_matrix <- distance.matrix(edges_summary)


# Optimization ------------------------------------------------------------

c(d,tour) %<-% simulated.annealing(points, edges_summary)
plot.tour(tour, edges_route)
plot.tour.map(tour, edges_route)


