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
routes_coords <- expand.grid(i=1:n, j=1:n) %>% 
  pbapply::pbapply(1,function(x){
    x <- as.list(x)
    i <- x$i; j <- x$j;
    if(i!=j){
      data.frame(i,j,route.calc(i,j,points), stringsAsFactors = F)
    }
  }) %>% dplyr::bind_rows() %>% 
  dplyr::mutate(id = 1:n())


routes_coords %>% 
  tidyr::gather(key,value,distance,traffic_time,base_time) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_histogram(ggplot2::aes(x=value,fill=key))+
  ggplot2::facet_grid(cols=vars(key),scales="free")

routes <- routes_coords %>% 
  dplyr::select(i,j,traffic_time) %>% 
  unique()

nvar <- nrow(routes)
nconstraint <- n

dist.mat <- matrix(0, nrow = n, ncol = n)
for(x in 1:nvar){
  dist.mat[routes$i[x],routes$j[x]] <- routes$traffic_time[x]
}
image(dist.mat)


atsp <- TSP::ATSP(dist.mat)
tour <- TSP::solve_TSP(atsp)
tour

solution <- data.frame(i=as.integer(tour), j=lead(as.integer(tour),1)) %>% 
  na.omit()

merge(solution, routes_coords, by=c("i","j"), sort=F) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_path(ggplot2::aes(x=route_lat, y=route_long))+
  ggplot2::geom_point(data=points, ggplot2::aes(x=latitude, y=longitude))



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
