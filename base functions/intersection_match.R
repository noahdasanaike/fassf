intersection_match <- function(data, 
                               intersecting_data, 
                               id_intersecting,
                               tie_breaker = "largest",
                               force_planar = TRUE,
                               quiet = FALSE){
  require(geos)
  sf::sf_use_s2(FALSE)
  if(force_planar == TRUE){
    if(st_crs(data) != st_crs(intersecting_data) & st_crs(data) != "EPSG:3857"){
      if(quiet == FALSE){print("correcting CRS, forcing planar")}
      if(!st_crs(data) == "EPSG:3857"){
        data <- data %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
      }
      if(!st_crs(intersecting_data) == st_crs(data)){
        intersecting_data <- intersecting_data %>% st_as_sf() %>% st_transform(crs = st_crs(data))
      }
    }
  }else{
    if(st_crs(data) != st_crs(intersecting_data)){
      if(quiet == FALSE){print("correcting CRS")}
      
      intersecting_data <- intersecting_data %>% st_as_sf() %>% st_transform(crs = st_crs(data))
    }
  }
  
  
  if(!sum(st_is_valid(data)) == nrow(data)){
    if(quiet == FALSE){print("fixing st validity, base data")}
    data <- st_make_valid(data)
  }
  if(!sum(st_is_valid(intersecting_data)) == nrow(intersecting_data)){
    if(quiet == FALSE){print("fixing st validity, intersecting data")}
    intersecting_data <- st_make_valid(intersecting_data)
  }
  
  if(quiet == FALSE){print("creating intersection matrix")}
  intersected_object <- geos_intersects_matrix(data$geometry, intersecting_data$geometry)
  
  if(quiet == FALSE){print("assigning matches")}
  data[[paste(id_intersecting)]] <- NA
  for(i in 1:length(intersected_object)){
    if(quiet == FALSE){cat("\r", i / length(intersected_object))}
    if(length(intersected_object[i][[1]]) > 1){
      if(tie_breaker == "largest"){
        data[[paste(id_intersecting)]][i] <- st_join(data[i, -which(names(data) == paste(id_intersecting))], 
                                                     intersecting_data[intersected_object[i][[1]],], 
                                                     largest = TRUE)[1,][[paste(id_intersecting)]]
      }
    }else{
      if(length(intersected_object[i][[1]]) == 0){
        data[[paste(id_intersecting)]][i] <- NA
      }else{
        data[[paste(id_intersecting)]][i] <- 
          intersecting_data[[paste(id_intersecting)]][intersected_object[i][[1]]]
      }
    }
  }
  return(data[[paste(id_intersecting)]])
}
