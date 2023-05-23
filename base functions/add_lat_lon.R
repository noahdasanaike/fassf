add_lat_lon <- function(polygons, quiet = FALSE){
  require(sf)
  if(!st_is_longlat(polygons)){
    if(quiet == FALSE){print("correcting CRS")}
    
    polygons <- polygons %>% st_as_sf() %>% st_transform(crs = "EPSG:4326")
  }
  polygons$lon <- st_coordinates(st_centroid(polygons$geometry))[,1]
  polygons$lat <- st_coordinates(st_centroid(polygons$geometry))[,2]
  
  return(polygons)
}
