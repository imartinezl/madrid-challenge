# Route Optimization

# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)

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


# Route Request -----------------------------------------------------------
source('route_request.R')


# DFS for graph completeness ----------------------------------------------

dfs.from.v <- function(g,v){
  vertex_names <- igraph::vertex.attributes(g)$name
  # vertex_num <- vertex_names %>% length()
  vertex <- c()
  mode <- c()
  f.in <- function(graph, data, extra) {
    mode <<- c(mode,"in")
    vertex <<- c(vertex,vertex_names[data[[1]]+1])
    # cat("in:", paste(collapse=", ", data), "\n")
    FALSE
  }
  f.out <- function(graph, data, extra) {
    mode <<- c(mode,"out")
    vertex <<- c(vertex,vertex_names[data[[1]]+1])
    # cat("out:", paste(collapse=", ", data), "\n")
    FALSE
  }
  igraph::dfs(g, root=v, neimode="out", in.callback=f.in, out.callback=f.out)
  complete_graph <- rle(mode)$lengths %>% length() == 2
  return(complete_graph)
}
complete.graph <- function(g){
  vertex_names <- igraph::vertex.attributes(g)$name
  for(v in vertex_names){
    if(dfs.from.v(g,v)){
      return(T)
    }
  }
  return(F)
}

# Reduce graph connectivity -----------------------------------------------
get.edges <- function(points){
  geosphere::distm(points, points, fun = geosphere::distHaversine) %>%
    reshape2::melt(varnames = c("i", "j"), value.name = "distance") %>% 
    dplyr::filter(distance > 0) %>% 
    dplyr::mutate(belong = T) %>%  
    dplyr::arrange(-distance)
}
reduce.graph <- function(edges, nneighbors){
  g <- igraph::graph.data.frame(edges %>% dplyr::filter(belong))
  # igraph::get.edgelist(g) %>% View
  if(!complete.graph(g)){
    stop("WFT!, Initial graph is not complete")
  }
  neighbors <- edges %>% 
    dplyr::filter(belong) %>% 
    dplyr::group_by(i) %>% 
    dplyr::summarise(n = n())
  for (e in 1:nrow(edges)) {
    if(e %% 10 == 0){print(e)}
    if(neighbors$n[edges$i[e]] > nneighbors){
      edges$belong[e] <- F
      # g <- igraph::graph.data.frame(edges %>% dplyr::filter(belong))
      to_remove <- edges %>% dplyr::slice(e) %>% dplyr::mutate(remove = paste0(i,'|',j)) %>% dplyr::pull(remove)
      g <- igraph::delete.edges(g,to_remove) #edges[e,c('i','j')])
      if(!complete.graph(g)){
        edges$belong[e] <- T
        g <- igraph::add.edges(g,to_remove) #edges[e,c('i','j')])
        next()
      }
      neighbors$n[edges$i[e]] <- neighbors$n[edges$i[e]] - 1
    }
  }
  # plot(g)
  # complete.graph(g)
  edges
}

edges <- get.edges(points)


# Route Download
route.download <- function(points, edges){ 
  routes_file <- "routes_backup.csv"
  if(!file.exists(routes_file)){
    routes_coords <- edges %>% 
      dplyr::filter(belong) %>% 
      dplyr::select(i,j,distance) %>% 
      pbapply::pbapply(1,function(x){
        i <- x['i']; j <- x['j'];
        if(i!=j){
          data.frame(i,j,route.calc(i,j,points), stringsAsFactors = F)
        }
      }) %>% 
      dplyr::bind_rows(.id = "route_id") %>% 
      dplyr::mutate(step_id = 1:n())
    write.csv(routes_coords, routes_file, row.names = F)
  }else{
    routes_coords <- read.csv(routes_file)
  }
}
routes_coords <- route.download(points, edges)
routes <- routes_coords %>% 
  dplyr::select(route_id,i,j,traffic_time,base_time,distance) %>% 
  unique() %>% 
  dplyr::arrange(i,j)

routes.plot <- function(points, routes_coords){
  ggplot2::ggplot()+
    ggplot2::geom_path(data = routes_coords, ggplot2::aes(x=route_long,y=route_lat, group=factor(route_id), color=factor(i)))+
    ggplot2::geom_label(data = points %>% dplyr::mutate(point_id=1:n()),
                        ggplot2::aes(x=longitude, y=latitude, label=point_id),size=5)+
    ggplot2::coord_equal()
}
routes.plot(points, routes_coords)





distance_matrix <- matrix(1e9, nrow = n, ncol = n)
diag(distance_matrix) <- 1e9
for(x in 1: nrow(routes)){
  distance_matrix[routes$i[x],routes$j[x]] <- routes$traffic_time[x]
}
image(distance_matrix)
distance_matrix


# Plot Tour ---------------------------------------------------------------

plot.tour <- function(tour, routes_coords){
  tour <- as.integer(tour)
  tour[length(tour)+1] <- tour[1]
  data.frame(i=tour, j=lead(tour,1)) %>% 
    na.omit() %>% 
    dplyr::mutate(solution_id = 1:n()) %>% 
    merge(routes_coords, by=c("i","j"), all.x = T, sort=F) %>% 
    dplyr::arrange(solution_id, step_id) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_path(ggplot2::aes(x=route_long, y=route_lat, group=factor(route_id), color=factor(solution_id)))+
    # ggplot2::geom_point(ggplot2::aes(x=route_long, y=route_lat))+
    ggplot2::geom_label(data = points %>% dplyr::mutate(point_id=1:n()),
                        ggplot2::aes(x=longitude, y=latitude, label=point_id),size=3)+
    ggplot2::coord_equal()
}
plot.tour(tour, routes_coords)

plot.tour.map <- function(tour, routes_coords){
  tour_path <- embed(c(i=tour, tour[1]), 2) %>% 
    as.data.frame() %>% 
    plyr::rename(c("V1"="i","V2"="j")) %>% 
    dplyr::mutate(solution_id = 1:n()) %>% 
    merge(routes_coords, by=c("i","j"), all.x = T, sort=F) %>% 
    dplyr::arrange(solution_id, step_id) %>% 
    dplyr::mutate(label = paste0("From ",i," to ", j))
  map <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 0, maxZoom = 18)) %>% 
    leaflet::addProviderTiles(leaflet::providers$Wikimedia, group = "Tiles") %>% 
    leaflet::addMarkers(data = points %>% dplyr::mutate(point_id=1:n()), 
                        lat = ~latitude, lng = ~longitude, label = ~as.character(point_id), 
                        group = "Contenedores Vidrio")
  groups <- tour_path %>% dplyr::pull(solution_id) %>% unique()
  pal <- leaflet::colorFactor(viridis::plasma(length(tour)-1), groups)
  for(g in groups){
    data_g <- tour_path[tour_path$solution_id == g, ]
    map <- map %>% leaflet::addPolylines(data = data_g, group = paste0("Lines_",g), 
                                         lng = ~route_long, lat = ~route_lat,
                                         color = ~pal(g), label = ~lapply(label, htmltools::HTML),
                                         weight = 2
    )
    
  }
  map %>% leaflet::setView(lat = 40.416673, lng = -3.703803, zoom = 14)
}
plot.tour.map(tour,routes_coords)

