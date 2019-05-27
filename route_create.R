

# Graph Completeness ------------------------------------------------------

dfs.from.v <- function(g,v){
  # DFS for graph completeness ----------------------------------------------
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


# Graph Edges -------------------------------------------------------------

edges.haversine <- function(points){
  geosphere::distm(points, points, fun = geosphere::distHaversine) %>%
    reshape2::melt(varnames = c("i", "j"), value.name = "distance") %>% 
    dplyr::filter(distance > 0) %>% 
    dplyr::mutate(belong = T) %>%  
    dplyr::arrange(-distance)
}

edges.reduce <- function(edges, nneighbors){
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

edges.route <- function(points, edges){ 
  routes_file <- "routes_backup.csv"
  if(!file.exists(routes_file)){
    edges_route <- edges %>% 
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
    write.csv(edges_route, routes_file, row.names = F)
  }else{
    edges_route <- read.csv(routes_file)
  }
  edges_route
}

edges.summary <- function(edges_route){
  edges_route %>% 
    dplyr::select(route_id,i,j,traffic_time,base_time,distance) %>% 
    unique() %>% 
    dplyr::arrange(i,j)
}

distance.matrix <- function(edges){
  n <- max(edges$i, edges$j)
  distance_matrix <- matrix(1e9, nrow = n, ncol = n)
  diag(distance_matrix) <- 1e9
  for(x in 1: nrow(edges)){
    distance_matrix[edges$i[x],edges$j[x]] <- edges$traffic_time[x]
  }
  # image(distance_matrix)
  distance_matrix
}

# Plot Tour ---------------------------------------------------------------

edges.plot <- function(points, edges_route){
  ggplot2::ggplot()+
    ggplot2::geom_path(data = edges_route, ggplot2::aes(x=route_long, y=route_lat, group=factor(route_id), color=factor(i)) )+
    ggplot2::geom_label(data = points %>% dplyr::mutate(point_id=1:n()),
                        ggplot2::aes(x=longitude, y=latitude, label=point_id),size=5)+
    ggplot2::coord_equal()+
    ggplot2::theme(legend.position = "none")
}

plot.tour <- function(tour, edges_route){
  # tour <- as.integer(tour)
  # tour[length(tour)+1] <- tour[1]
  # data.frame(i=tour, j=lead(tour,1)) %>% 
  #   na.omit() %>% 
  embed(c(i=tour, tour[1]), 2) %>% 
    as.data.frame() %>% 
    plyr::rename(c("V1"="i","V2"="j")) %>%
    dplyr::mutate(solution_id = 1:n()) %>% 
    merge(edges_route, by=c("i","j"), all.x = T, sort=F) %>% 
    dplyr::arrange(solution_id, step_id) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_path(data = edges_route, ggplot2::aes(x=route_long,y=route_lat, group=factor(route_id)), color="#dddddd")+
    ggplot2::geom_path(ggplot2::aes(x=route_long, y=route_lat, group=factor(route_id), color=factor(solution_id)))+
    # ggplot2::geom_point(ggplot2::aes(x=route_long, y=route_lat))+
    ggplot2::geom_label(data = points %>% dplyr::mutate(point_id=1:n()),
                        ggplot2::aes(x=longitude, y=latitude, label=point_id),size=3)+
    ggplot2::coord_equal()
}

plot.tour.map <- function(tour, edges_route){
  tour_path <- embed(c(i=tour, tour[1]), 2) %>% 
    as.data.frame() %>% 
    plyr::rename(c("V1"="i","V2"="j")) %>% 
    dplyr::mutate(solution_id = 1:n()) %>% 
    merge(edges_route, by=c("i","j"), all.x = T, sort=F) %>% 
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
