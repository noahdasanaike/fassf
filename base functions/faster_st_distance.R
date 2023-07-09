faster_st_distance <- function(a, b, centroid = TRUE){
  cores <- parallel::detectCores() - 1
  if(centroid == TRUE){
    centroids <- st_centroid(a)
    return(unlist(nngeo::st_nn(centroids,
                               b,
                               parallel = cores,
                               returnDist = TRUE)$dist) / 1000)
  }else{
    return(unlist(nngeo::st_nn(a,
                               b,
                               parallel = cores,
                               returnDist = TRUE)$dist) / 1000)
  }
}
