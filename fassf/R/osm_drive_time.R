#' @export

osm_drive_time <- function(a, b){
  require(sf)
  require(osrm)
  
  return(unlist(map(st_transform(st_geometry(st_centroid(a)), crs = 4326), 
             ~osrm::osrmRoute(st_set_crs(st_sfc(.),  4326), st_transform(b, crs = 4326))$dist)))
}