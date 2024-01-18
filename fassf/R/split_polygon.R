#' @export

split_poly <- function(polygon, splits, point_sample_size = 10000){
  # adapted from gis.stackexchange user Geoline
  
  require(dismo)
  require(sf)
  
  points_rnd <- st_sample(polygon, size = point_sample_size)
  
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% 
    setNames(c("lon", "lat"))
  
  k_means <- kmeans(points, centers = splits)
  
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = polygon)
  
  crs(voronoi_polys) <- crs(polygon)
  
  voronoi_sf <- st_as_sf(voronoi_polys)
  
  return(st_intersection(voronoi_sf, polygon))
}