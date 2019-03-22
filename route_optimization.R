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
  dplyr::slice(1:10) %>% 
  dplyr::select(LONGITUD, LATITUD) %>% 
  plyr::rename(c("LONGITUD"="longitude", "LATITUD"="latitude"))


# Route Request -----------------------------------------------------------

route.request <- function(waypoint0, waypoint1, mode){
  base <- "https://route.api.here.com/routing/7.2/"
  endpoint <- "calculateroute.json"
  app_id <- Sys.getenv("HERE_APP_ID")
  app_code <- Sys.getenv("HERE_APP_CODE")
  
  url <- paste0(base,endpoint,"?","app_id=", app_id, "&app_code=", app_code, 
                "&waypoint0=geo!", waypoint0, "&waypoint1=geo!", waypoint1, 
                "&mode=", mode)
  tryCatch({
    r <- httr::GET(url)
    httr::stop_for_status(r)
    content <- jsonlite::fromJSON(httr::content(r, as="text"))
    return(content)
  }, http_error=function(e) {
    ## log error or otherwise recover
    warning(paste0("HTTP error ", e))
  })
}
route.info <- function(content){
  distance <- content$response$route$summary$distance
  traffic_time <- content$response$route$summary$trafficTime
  base_time <- content$response$route$summary$baseTime
  route_lat <- content$response$route$leg[[1]]$maneuver[[1]]$position$latitude
  route_long <- content$response$route$leg[[1]]$maneuver[[1]]$position$longitude
  route_dist <- content$response$route$leg[[1]]$maneuver[[1]]$length
  route_time <- content$response$route$leg[[1]]$maneuver[[1]]$travelTime
  start_street_side <- content$response$route$leg[[1]]$start$sideOfStreet
  start_street_name <- content$response$route$leg[[1]]$start$mappedRoadName
  stop_street_side <- content$response$route$leg[[1]]$end$sideOfStreet
  stop_street_name <- content$response$route$leg[[1]]$end$mappedRoadName
  
  data.frame(distance, traffic_time, base_time, route_lat, route_long,
             route_dist, route_time, start_street_side, start_street_name, 
             stop_street_side, stop_street_name, stringsAsFactors = F)
}
route.calc <- function(i,j, points){
  waypoint0 <- paste0(points$latitude[i],',',points$longitude[i])
  waypoint1 <- paste0(points$latitude[j],',',points$longitude[j])
  mode <- "fastest;truck;traffic:disabled"
  route.request(waypoint0, waypoint1, mode) %>% route.info()
}

# Route Download
n <- nrow(points)
nneighbors <- 2
routes_coords <- expand.grid(i=1:n, j=1:n) %>% 
  apply(1,function(x){
    x <- as.list(x)
    i <- x$i; j <- x$j;
    if(i!=j){
      distance <- geosphere::distm(points[i,], points[j,], fun = geosphere::distHaversine) # calculate distance from points
      data.frame(i,j,distance, stringsAsFactors = F)
    }
  }) %>% 
  dplyr::bind_rows() %>% 
  dplyr::group_by(i) %>% 
  dplyr::top_n(n = nneighbors, wt = 1/distance) %>% 
  pbapply::pbapply(1,function(x){
    x <- as.list(x)
    i <- x$i; j <- x$j;
    if(i!=j){
      data.frame(i,j,route.calc(i,j,points), stringsAsFactors = F)
    }
  }) %>% dplyr::bind_rows(.id = "route_id") %>% 
  dplyr::mutate(step_id = 1:n())

# routes_coords %>% 
#   tidyr::gather(key,value,distance,traffic_time,base_time) %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_histogram(ggplot2::aes(x=value,fill=key)) +
#   ggplot2::facet_grid(cols=vars(key),scales="free")

ggplot2::ggplot()+
  ggplot2::geom_path(data = routes_coords, ggplot2::aes(x=route_long,y=route_lat, group=factor(route_id), color=factor(i)))+
  ggplot2::geom_label(data = points %>% dplyr::mutate(point_id=1:n()),
                      ggplot2::aes(x=longitude, y=latitude, label=point_id),size=5)

routes <- routes_coords %>% 
  dplyr::select(route_id,i,j,traffic_time,base_time,distance) %>% 
  unique() %>% 
  dplyr::arrange(i)

nvar <- nrow(routes)
nconstraint <- n

distance_matrix <- matrix(1e10, nrow = n, ncol = n)
diag(distance_matrix) <- 0
for(x in 1:nvar){
  distance_matrix[routes$i[x],routes$j[x]] <- routes$traffic_time[x]
}
image(distance_matrix)
distance_matrix

# TSP Package -------------------------------------------------------------

# Nearest, farthest, cheapest and arbitrary insertion algorithms
atsp <- TSP::ATSP(distance_matrix)
tour <- TSP::solve_TSP(x = atsp, method="arbitrary_insertion")
as.integer(tour)

plot.tour <- function(tour, routes_coords){
  data.frame(i=as.integer(tour), j=lead(as.integer(tour),1)) %>% 
    na.omit() %>% 
    dplyr::mutate(solution_id = 1:n()) %>% 
    merge(routes_coords, by=c("i","j"), all.x = T, sort=F) %>% 
    dplyr::arrange(solution_id, step_id) %>% 
    ggplot2::ggplot()+
    ggplot2::geom_path(ggplot2::aes(x=route_long, y=route_lat, group=factor(route_id), color=factor(solution_id)))+
    ggplot2::geom_point(ggplot2::aes(x=route_long, y=route_lat))+
    ggplot2::geom_point(data=points, ggplot2::aes(x=longitude, y=latitude), size=3, color="red")
}
plot.tour(tour, routes_coords)



# Simulated Annealing -----------------------------------------------------

calculate_tour_distance <- function(tour, distance_matrix) {
  sum(distance_matrix[embed(c(tour, tour[1]), 2)])
}
current_temperature <- function(iter, s_curve_amplitude, s_curve_center, s_curve_width) {
  s_curve_amplitude * s_curve(iter, s_curve_center, s_curve_width)
}
s_curve <- function(x, center, width) {
  1 / (1 + exp((x - center) / width))
}
run_intermediate_annealing_process <- function(points, distance_matrix, tour, tour_distance, best_tour, best_distance,
                                               starting_iteration, number_of_iterations,
                                               s_curve_amplitude, s_curve_center, s_curve_width) {
  n_points = nrow(points)
  for(i in 1:number_of_iterations) {
    iter = starting_iteration + i
    temp = current_temperature(iter, s_curve_amplitude, s_curve_center, s_curve_width)
    
    candidate_tour = tour
    swap = sample(n_points, 2)
    candidate_tour[swap[1]:swap[2]] = rev(candidate_tour[swap[1]:swap[2]])
    candidate_dist = calculate_tour_distance(candidate_tour, distance_matrix)
    
    if (temp > 0) {
      ratio = exp((tour_distance - candidate_dist) / temp)
    } else {
      ratio = as.numeric(candidate_dist < tour_distance)
    }
    
    if (runif(1) < ratio) {
      tour = candidate_tour
      tour_distance = candidate_dist
      
      if (tour_distance < best_distance) {
        best_tour = tour
        best_distance = tour_distance
      }
    }
  }
  return(list(tour=tour, tour_distance=tour_distance, best_tour=best_tour, best_distance=best_distance))
}

points <- points
distance_matrix <- distance_matrix
tour <- sample(n)
tour_distance <- calculate_tour_distance(tour, distance_matrix)
best_tour <- c()
best_distance <- Inf
total_iterations <- 25000
iter <- 0
plot_every_iterations <- 500
s_curve_amplitude <- 4000
s_curve_center <- 1000
s_curve_width <- 3000

number_of_loops <- ceiling(total_iterations / plot_every_iterations)
distances <- rep(NA, number_of_loops)
plot.tour(tour, routes_coords)

for(i in 1:number_of_loops){
  intermediate_results <- run_intermediate_annealing_process(points, distance_matrix, tour, tour_distance, best_tour, best_distance,
                                                             iter, plot_every_iterations,
                                                             s_curve_amplitude, s_curve_center, s_curve_width)
  
  tour <- intermediate_results$tour
  tour_distance <- intermediate_results$tour_distance
  best_tour <- intermediate_results$best_tour
  best_distance <- intermediate_results$best_distance
  
  iter <- iter + plot_every_iterations
  
  distances[ceiling(iter / plot_every_iterations)] <- intermediate_results$tour_distance
}
plot(distances)
plot.tour(best_tour, routes_coords)





# Linear Programming Problem ----------------------------------------------------------------

objective.in <- routes$traffic_time
const.rhs <- rep(1,nconstraint)
const.dir  <- rep("==", nconstraint)

# Find the optimal solution
optimum <-  lpSolve::lp(direction="min",
                        objective.in = objective.in,
                        const.mat = const.mat,
                        const.dir = const.dir,
                        const.rhs = const.rhs,
                        all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)
# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("x_4p", "x_3p", "x_w") 
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total cost: ", optimum$objval, sep=""))



# Define Problem Objective ------------------------------------------------

# what is the solution we want to get?
# Is it a circular path to visit every node, or just the shortest path to visit all the nodes?

