#' @export

trial_voronoi_polygons <- function (x, intersection, quiet = FALSE) {
  require(sf)
  require(future)
  require(furrr)
  require(progressr)
  
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
  terra1_sf_valid <- st_make_valid(terra1_sf)
  
  # Filter to keep only POLYGONs and buffer any LINESTRINGs slightly
  is_linestring <- st_geometry_type(terra1_sf_valid) == "LINESTRING"
  if (any(is_linestring)) {
    if (!quiet) {
      print(paste("Converting", sum(is_linestring), "LINESTRING(s) to POLYGON"))
    }
    # Buffer the linestring slightly to create a very thin polygon
    terra1_sf_valid$geometry[is_linestring] <- st_buffer(
      terra1_sf_valid$geometry[is_linestring], 
      dist = 1e-6
    )
  }
  
  terra1 <- vect(terra1_sf_valid)
  terra2 <- vect(intersection)
  
  df_int <- intersect(terra1, terra2) %>% st_as_sf()
  return(st_set_crs(st_sfc(df_int$geometry), original_crs))
}
