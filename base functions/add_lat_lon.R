add_lat_lon <- function(polygons, quiet = FALSE){
  sf_use_s2(FALSE)
  original_crs <- st_crs(polygons)
  require(sf)
  if(!st_is_longlat(polygons)){
    if(quiet == FALSE){print("correcting CRS")}
    
    polygons <- polygons %>% st_as_sf() %>% st_transform(crs = "EPSG:4326")
  }
  polygons$longitude <- st_coordinates(st_centroid(polygons$geometry))[,1]
  polygons$latitude <- st_coordinates(st_centroid(polygons$geometry))[,2]
  polygons <- polygons %>% st_transform(crs = original_crs)
  
  return(polygons)
}
