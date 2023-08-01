faster_st_distance <- function(a, b, centroid = TRUE, summarize_geometry = TRUE){
  require(parallel)
  cores <- parallel::detectCores() - 1
  if(nrow(b) > 0 & summarize_geometry == TRUE){
    b <- b %>% summarize(geometry = st_union(geometry))
  }
  if(st_crs(a) != st_crs(b) & st_crs(a) != "EPSG:3857"){
    a <- a %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    b <- b %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
  }
  if(centroid == TRUE){
    centroids <- st_sfc(map(st_geometry(b), ~st_centroid(.)), crs = st_crs(b))
    data <- tibble(index = nngeo::st_nn(centroids,
                                        a,
                                        k = nrow(a),
                                        parallel = cores,
                                        returnDist = FALSE)[[1]],
                   dist = unlist(nngeo::st_nn(centroids,
                                              a,
                                              k = nrow(a),
                                              parallel = cores,
                                              returnDist = TRUE)$dist) / 1000) %>% 
      arrange(index)
    return(data$dist)
  }else{
    data <- tibble(index = nngeo::st_nn(b,
                                        a,
                                        k = nrow(a),
                                        parallel = cores,
                                        returnDist = FALSE)[[1]],
                   dist = unlist(nngeo::st_nn(b,
                                              a,
                                              k = nrow(a),
                                              parallel = cores,
                                              returnDist = TRUE)$dist) / 1000) %>% 
      arrange(index)
    return(data$dist)
  }
}
