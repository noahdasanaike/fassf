faster_st_distance <- function(a, b, centroid = TRUE, summarize_geometry = TRUE,
                               quiet = FALSE){
  if(nrow(b) > 0 & summarize_geometry == TRUE){
    b <- b %>% summarize(geometry = st_union(st_geometry(b)))
  }
  if(st_crs(a) != st_crs(b) & st_crs(a) != "EPSG:3857"){
    a <- a %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    b <- b %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
  }
  if(centroid == TRUE){
    centroids <- st_centroid(a)
    if(quiet == TRUE){
      return(unlist(suppressMessages(map(st_geometry(centroids), 
                                         ~nngeo::st_nn(st_set_crs(st_sfc(.), st_crs(b)), b, k = 1, returnDist = TRUE, progress = FALSE)$dist))) / 1000)
    }else{
      return(unlist(suppressMessages(map(st_geometry(centroids), 
                                         ~nngeo::st_nn(st_set_crs(st_sfc(.), st_crs(b)), b, k = 1, returnDist = TRUE, progress = FALSE)$dist, 
                                         .progress = list(
                                           type = "iterator", 
                                           format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                           clear = TRUE)))) / 1000)
    }
  }else{
    if(quiet == TRUE){
      return(unlist(suppressMessages(map(st_geometry(a), 
                                         ~nngeo::st_nn(st_set_crs(st_sfc(.), st_crs(b)), b, k = 1, returnDist = TRUE, progress = FALSE)$dist))) / 1000)
    }
    else{
      return(unlist(suppressMessages(map(st_geometry(a), 
                                         ~nngeo::st_nn(st_set_crs(st_sfc(.), st_crs(b)), b, k = 1, returnDist = TRUE, progress = FALSE)$dist, 
                                         .progress = list(
                                           type = "iterator", 
                                           format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                           clear = TRUE)))) / 1000)
    }
  }
}