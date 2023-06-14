faster_st_distance <- function(a, b, centroid = TRUE){
  cores <- parallel::detectCores() - 1
  if(centroid == TRUE){
    centroids <- st_centroid(a$geometry)
    return(unlist(nngeo::st_nn(centroids,
                               b$geometry,
                               parallel = cores,
                               returnDist = TRUE)$dist) / 1000)
  }else{
    return(unlist(nngeo::st_nn(a$geometry,
                               b$geometry,
                               parallel = cores,
                               returnDist = TRUE)$dist) / 1000)
  }
}