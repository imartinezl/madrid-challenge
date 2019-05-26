

# Route Request -----------------------------------------------------------

route.calc.request <- function(waypoint0, waypoint1, mode, representation){
  base <- "https://route.api.here.com/routing/7.2/"
  endpoint <- "calculateroute.json"
  app_id <- Sys.getenv("HERE_APP_ID")
  app_code <- Sys.getenv("HERE_APP_CODE")
  
  url <- paste0(base,endpoint,"?","app_id=", app_id, "&app_code=", app_code, 
                "&waypoint0=geo!", waypoint0, "&waypoint1=geo!", waypoint1, 
                "&mode=", mode, "&representation=",representation)
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
route.shape <- function(content){
  lapply(content$response$route$shape[[1]],function(p){
    p %>% 
      stringr::str_split(',') %>% 
      unlist() %>% 
      as.numeric()
  }) %>% 
    unlist() %>% 
    matrix(byrow = T, ncol=2) %>% 
    as.data.frame() %>% 
    `colnames<-`(c("lat","long"))
}
route.calc.info <- function(content){
  distance <- content$response$route$leg[[1]]$summary$distance
  traffic_time <- content$response$route$leg[[1]]$summary$trafficTime
  base_time <- content$response$route$leg[[1]]$summary$baseTime
  route_shape <- route.shape(content)
  route_lat <- route_shape$lat 
  route_long <- route_shape$long
  start_street_side <- content$response$route$leg[[1]]$start$sideOfStreet
  start_street_name <- content$response$route$leg[[1]]$start$mappedRoadName
  stop_street_side <- content$response$route$leg[[1]]$end$sideOfStreet
  stop_street_name <- content$response$route$leg[[1]]$end$mappedRoadName
  
  data.frame(distance, traffic_time, base_time, route_lat, route_long, 
             start_street_side, start_street_name, stop_street_side, stop_street_name, 
             stringsAsFactors = F)
}

route.calc <- function(i,j, points){
  waypoint0 <- paste0(points$latitude[i],',',points$longitude[i])
  waypoint1 <- paste0(points$latitude[j],',',points$longitude[j])
  mode <- "fastest;truck;traffic:disabled;"
  representation <- "navigation"
  route.calc.request(waypoint0, waypoint1, mode, representation) %>% route.calc.info()
}