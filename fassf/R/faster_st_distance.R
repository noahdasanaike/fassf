#' @export

faster_st_distance <- function(a, b, centroid = TRUE, summarize_geometry = TRUE,
                               quiet = FALSE){
  if(summarize_geometry == TRUE){
    if(!quiet){print("summarizing geometry")}
    b <- st_union(st_geometry(b))
  }
  if(st_crs(a) != st_crs(b) & st_crs(a) != "EPSG:3857"){
    a <- a %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    b <- b %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
  }
  if(centroid == TRUE){
    centroids <- st_centroid(a)
  }else{
    centroids <- a
  }
  return(as.numeric(unlist(nngeo::st_nn(centroids, b, k = 1, returnDist = TRUE, progress = !quiet)$dist)) / 1000)
}
