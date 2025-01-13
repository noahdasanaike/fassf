#' @export
better_voronoi_polygons <- function (x, intersection, quiet = FALSE) {
  require(sf)
  require(future)
  require(furrr)
  require(progressr)
  require(terra)
  
  original_crs <- st_crs(x)
  bbox <- st_bbox(st_transform(intersection, crs = st_crs(original_crs)))
  sf::sf_use_s2(FALSE)
  if (length(x) != length(unique(x))) {
    return("not all geometries are unique")
  }
  require(deldir)
  require(sp)
  if (!quiet){
    print("converting to spatial format")
  }
  x <- as_Spatial(x)
  if (!quiet){
    print("constructing Thiessen polygons")
  }
  crds <- if (.hasSlot(x, "coords")) {
    x@coords} else {
      x
    }
  z <- deldir(crds[, 1], crds[, 2], rw = c(bbox[1], bbox[3], 
                                           bbox[2], bbox[4]))
  w <- tile.list(z)
  polys <- vector(mode = "list", length = length(w))
  for (i in seq(along = polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1, ])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  if (!quiet) {
    print("converting to sf format")
  }
  SP <- SpatialPolygons(polys)
  intersection <- st_make_valid(st_transform(intersection, 
                                             crs = original_crs))
  
  # Modified conversion section
  terra1_sf <- st_as_sf(SP)
  
  ###
  test <- terra1_sf
  test$intersects <- lengths(st_intersects(st_set_crs(test, original_crs), intersection))
  test %>% 
    st_set_crs(original_crs) %>%
    ggplot() +
    geom_sf(aes(fill = intersects)) +
    geom_sf(data = intersection, fill = NA, color = "red") +
    geom_sf(data = st_as_sf(x), color = "green")
  ###
  
  terra1_sf_valid <- st_make_valid(terra1_sf)
  
  terra1 <- vect(terra1_sf_valid)
  
  if(st_geometry_type(intersection) == "GEOMETRYCOLLECTION"){
    df_int <- st_intersection(st_set_crs(terra1_sf_valid, original_crs), intersection)
  }else{
    terra2 <- vect(intersection)
    df_int <- intersect(terra1, terra2) %>% st_as_sf()
  }
  return(st_set_crs(st_sfc(df_int$geometry), original_crs))
}
