faster_st_filter <- function(original, intersecting, quiet = FALSE){
  require(geos)
  if(st_crs(original) != st_crs(intersecting) & st_crs(original) != "EPSG:3857"){
    if(quiet == FALSE){print("correcting CRS")}
    
    original <- original %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    intersecting <- intersecting %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
  }
  return(original[which(lengths(geos_intersects_matrix(original$geometry, 
                                                intersecting$geometry)) > 0),])
}