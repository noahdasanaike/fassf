#' @export

require(sf)
require(geosphere)
require(tidyverse)
require(doParallel)

circumscription <- function(data_input, geographic_feature,
                            angle = 20, max_distance = 1000,
                            par = TRUE, quiet = FALSE){
  if(!st_crs(data_input)$input == "EPSG:3857"){
    data_input <- st_transform(data_input, "EPSG:3857")
  }
  if(!st_crs(geographic_feature)$input == "EPSG:3857"){
    geographic_feature <- st_transform(geographic_feature, "EPSG:3857")
  }
  
  sf::sf_use_s2(FALSE)
  
  all_points <- st_centroid(data_input$geometry)
  
  if(par == TRUE){
    library(doParallel)
    no_cores <- detectCores() - 1
    registerDoParallel(cores = no_cores)  
    cl <- makeCluster(no_cores, type = "PSOCK")  
    clusterCall(cl, function() library(sf))
    
    if(quiet == FALSE){print("starting parallel\n")}
    
    distances <- parLapply(cl, all_points$x, fun = radial_distances_parallel,
                           geographic_feature = geographic_feature, 
                           angle = angle, quiet = quiet,
                           max_distance = max_distance)
    
    stopCluster(cl)
  }
  else{
    distances <- radial_distances(all_points, geographic_feature, angle, quiet, 
                                  max_distance)
  }
  
  all_points$constrained <- constrained_calculation(distances, angle)
  return(all_points)
}

constrained_calculation <- function(distances, angle){
  base <- rep(1, 360 / angle)
  n_distance <- rep(NA, length(distances))
  
  for(i in 1:length(distances)){
    n_distance[i] <- norm(as.matrix(base - distances[[i]][[1]]))
  }
  return(n_distance)
}

radial_distances_parallel <- function(points, geographic_feature, angle, max_distance, quiet) {
  directions <- expand.grid(degrees = seq(0, 360 - angle, angle), distance = NA)
  cell <- as.matrix(st_coordinates(points))
  
  for(i in 1:nrow(directions)){
    travel_dist <- 0
    at_edge <- FALSE
    while(at_edge == FALSE){
      travel_dist <- travel_dist + 1
      
      dest_point <- geosphere::destPoint(p = cell, d = travel_dist * 1000, b = directions$degrees[i]) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('lon', 'lat')) 
      
      st_crs(dest_point) <- st_crs(geographic_feature)
      
      if(sum(suppressMessages(st_contains(geographic_feature, dest_point, sparse = F))) > 0){
        at_edge <- TRUE
        directions$distance[i] <- travel_dist
      }
      if(travel_dist == max_distance){
        at_edge <- TRUE
        directions$distance[i] <- max_distance
      }
    }
  }
  return(list(directions$distance))
}

radial_distances <- function(points, geographic_feature, max_distance, angle, quiet) {
  points$distances <- NA
  for(y in 1:nrow(points)){
    directions <- expand.grid(degrees = seq(0, 360 - angle, angle), distance = NA)
    cell <- as.matrix(st_coordinates(st_centroid(points[y,])))
    
    for(i in 1:nrow(directions)){
      if(!quiet){
        cat(paste0("\r", round((y - 1) / nrow(points) * 100), 
                   ".", str_split(round(i / nrow(directions), 2), pattern = "\\.")[[1]][2], "%"))
      }
      travel_dist <- 0
      at_edge <- FALSE
      while(at_edge == FALSE){
        travel_dist <- travel_dist + 1
        
        dest_point <- geosphere::destPoint(p = cell, d = travel_dist * 1000, b = directions$degrees[i]) %>%
          as.data.frame() %>%
          st_as_sf(coords = c("lon", "lat")) 
        
        st_crs(dest_point) <- st_crs(geographic_feature)
        
        if(sum(suppressMessages(st_contains(geographic_feature, dest_point, sparse = F))) > 0){
          at_edge <- TRUE
          directions$distance[i] <- travel_dist
        }
        if(travel_dist == max_distance){
          at_edge <- TRUE
          directions$distance[i] <- max_distance
        }
      }
    }
    points$distances[y] <- list(directions$distance)
  }
  return(points$distances)
}
