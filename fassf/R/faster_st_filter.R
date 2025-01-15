#' @export

faster_st_filter <- function(original, intersecting, quiet = FALSE, transform = TRUE, transform_crs = "EPSG:3857") {
  require(geos)
  
  if (st_crs(original) != st_crs(intersecting)) {
    if (quiet == FALSE) {
      print("correcting CRS")
    }
    
    if (transform) {
      if (!st_crs(original) == transform_crs) {
        original <- original %>% st_as_sf() %>% st_transform(crs = transform_crs)
      }
      if (!st_crs(intersecting) == transform_crs) {
        intersecting <- intersecting %>% st_as_sf() %>% st_transform(crs = transform_crs)
      }
    } else {
      intersecting <- intersecting %>% st_as_sf() %>% st_transform(crs = st_crs(original))
    }
  }
  
  return(original[which(lengths(geos_intersects_matrix(st_geometry(original), 
                                                       st_geometry(intersecting))) > 0), ])
}
