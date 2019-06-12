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
madrid_central_data <- sp::coordinates(madrid_central)[[1]][[1]] %>% 
  as.data.frame() %>% 
  `colnames<-`(c("longitude","latitude"))
inside.madrid.central <- function(longitude, latitude, madrid_central_data){
  sp::point.in.polygon(longitude, latitude, 
                       madrid_central_data$longitude, 
                       madrid_central_data$latitude)
}

# Contenedores Vidrio
data_contenedores_vidrio <- read.csv('data/Contenedores_vidrio_con_publicidad.csv', sep = ";", dec = ".", stringsAsFactors = F) %>% 
  dplyr::mutate(FECHA.PUESTA.EN.SERVICIO = as.Date(FECHA.PUESTA.EN.SERVICIO, format="%d/%m/%Y"),
                LONGITUD = as.numeric(sub(",", ".", Longitud, fixed = TRUE)),
                LATITUD = as.numeric(sub(",", ".", Latitud, fixed = TRUE)),
                Nombre = stringr::str_to_title(Nombre),
                LABEL = paste0("<b>Dirección:</b> ", Nombre))

points <- data_contenedores_vidrio %>% 
  # dplyr::slice(1:10) %>% 
  dplyr::select(LONGITUD, LATITUD) %>%
  plyr::rename(c("LONGITUD"="longitude", "LATITUD"="latitude")) %>% 
  dplyr::mutate(inside_madrid_central = inside.madrid.central(longitude, latitude, madrid_central_data))

ggplot2::ggplot()+
  ggplot2::geom_point(data=points, ggplot2::aes(x=longitude,y=latitude,color=factor(inside_madrid_central)))+
  ggplot2::geom_path(data=madrid_central_data, ggplot2::aes(x=longitude,y=latitude))

points <- points %>% 
  dplyr::filter(inside_madrid_central == 1) %>% 
  dplyr::select(-inside_madrid_central)

# Graph: Edges & Points ---------------------------------------------------

edges_haversine <- edges.haversine(points) #%>% edges.reduce(5)
edges_route <- edges.route(points, edges_haversine)
edges_summary <- edges.summary(edges_route)

edges.plot(points, edges_route)
# edges_route_matrix <- edges.route.matrix(edges_route)

# distance_matrix <- distance.matrix(edges_summary, variable)


# Optimization ------------------------------------------------------------

if(F){
c(d,tours,best_tour) %<-% simulated.annealing(points, distance_matrix)

atsp <- TSP::ATSP(distance_matrix)
tour <- TSP::solve_TSP(x = atsp, method="arbitrary_insertion") %>% as.integer()
tour <- TSP::solve_TSP(x = atsp, method="nearest_insertion") %>% as.integer()
tour <- TSP::solve_TSP(x = atsp, method="farthest_insertion") %>% as.integer()

plot.tour(best_tour, edges_route)
plot.tour.map(best_tour, edges_route)
}


