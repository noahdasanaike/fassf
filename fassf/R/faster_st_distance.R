#' @export

faster_st_distance <- function(a, b, centroid = TRUE, summarize_geometry = TRUE,
                               quiet = FALSE,
                               cores = NULL){
  sf::sf_use_s2(FALSE)
  if(summarize_geometry == TRUE){
    if(!quiet){print("summarizing geometry")}
    b <- st_union(st_geometry(b))
  }
  if(st_crs(a) != st_crs(b) & st_crs(a) != "EPSG:3857"){
    a <- a %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    b <- b %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    a <- st_make_valid(a); b <- st_make_valid(b)
  }else{
    a <- st_as_sf(a)
    b <- st_as_sf(b)
  }
  if(centroid == TRUE){
    centroids <- st_centroid(a)
  }else{
    centroids <- a
  }

  library(parallel)
  library(pbapply)

  if(is.null(cores)){cores <- parallel::detectCores() - 1}

  cl <- makeCluster(cores)
  clusterEvalQ(cl = cl, c(library(sf), library(nngeo)))

  # st_nn_mod <- function(x, y, k, returnDist, quiet, crs){
  #   x <- st_sfc(x); y <- st_sfc(y)
  #   st_crs(x) <- st_crs(y) <- crs
  #   return(as.numeric(unlist(nngeo::st_nn(x, y, k = k,
  #                                         returnDist = returnDist,
  #                                         progress = !quiet)$dist)) / 1000)
  # }
  st_distance_mod <- function(x, y, crs){
    x <- st_sfc(x); y <- st_sfc(y)
    st_crs(x) <- st_crs(y) <- crs
    return(as.numeric(st_distance(x, y)) / 1000)
  }

  # out <- pbsapply(cl = cl,
  #                 X = centroids,
  #                 FUN = st_nn_mod,
  #                 y = b,
  #                 k = 1,
  #                 returnDist = TRUE,
  #                 quiet = FALSE,
  #                 crs = st_crs(centroids))
  pboptions(type = "timer")
  out <- pbsapply(cl = cl,
                  X = st_as_sfc(centroids),
                  FUN = st_distance_mod,
                  y = st_as_sfc(b),
                  crs = st_crs(centroids))

  parallel::stopCluster(cl); rm(cl); base::closeAllConnections()

  return(out)

  # return(as.numeric(unlist(nngeo::st_nn(centroids, b, k = 1,
  #                                       returnDist = TRUE,
  #                                       progress = !quiet)$dist)) / 1000)
}
