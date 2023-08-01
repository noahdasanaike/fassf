faster_st_filter <- function(original, intersecting, quiet = FALSE){
  require(geos)
  if(st_crs(original) != st_crs(intersecting) & st_crs(original) != "EPSG:3857"){
    if(quiet == FALSE){print("correcting CRS")}
    
    if(!st_crs(original) == "EPSG:3857"){
        original <- original %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
      }
      if(!st_crs(intersecting) == st_crs(data)){
        intersecting <- intersecting %>% st_as_sf() %>% st_transform(crs = st_crs(original))
     }
  }
  return(original[which(lengths(geos_intersects_matrix(st_geometry(original), st_geometry(intersecting))) > 0),])
}
