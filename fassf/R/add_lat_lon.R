#' @export

add_lat_lon <- function(polygons, quiet = FALSE){
  sf_use_s2(FALSE)
  original_crs <- st_crs(polygons)
  require(sf)
  if(!st_is_longlat(polygons)){
    if(quiet == FALSE){print("correcting CRS")}

    polygons <- st_transform(polygons, crs = "EPSG:4326")
  }
  polygons$longitude <- st_coordinates(st_centroid(st_geometry(polygons)))[,1]
  polygons$latitude <- st_coordinates(st_centroid(st_geometry(polygons)))[,2]
  polygons <- st_transform(polygons, crs = original_crs)

  return(polygons)
}
