#' @export
better_voronoi_polygons <- function(x, intersection, quiet = FALSE) {
  require(sf)
  require(future)
  require(furrr)
  require(progressr)
  require(terra)
  require(deldir)
  require(sp)
  
  sf::sf_use_s2(FALSE)
  
  if (length(x) != length(unique(x))) {
    return("not all geometries are unique")
  }
  
  original_crs <- st_crs(x)
  bbox <- st_bbox(st_transform(intersection, crs = original_crs))
  
  if (!quiet) print("converting to spatial format")
  
  x_sp <- as_Spatial(x)
  
  if (!quiet) print("constructing Thiessen polygons")
  
  crds <- if (.hasSlot(x_sp, "coords")) {
    x_sp@coords
  } else {
    x_sp
  }
  
  z <- deldir(crds[, 1], crds[, 2], 
              rw = c(bbox[1], bbox[3], bbox[2], bbox[4]))
  w <- tile.list(z)
  
  polys <- vector(mode = "list", length = length(w))
  for (i in seq(along = polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1, ])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  
  if (!quiet) print("converting to sf format")
  
  SP <- SpatialPolygons(polys)
  
  intersection <- st_make_valid(st_transform(intersection, crs = original_crs))
  
  terra1_sf <- st_as_sf(SP)
  terra1_sf_valid <- st_make_valid(terra1_sf)
  
  if (inherits(st_geometry(intersection), "sfc_GEOMETRYCOLLECTION")) {
    df_int <- st_intersection(
      st_set_crs(terra1_sf_valid, original_crs),
      intersection
    )
  } else {
    terra1 <- vect(terra1_sf_valid)
    terra2 <- vect(intersection)
    df_int <- intersect(terra1, terra2) %>% st_as_sf()
  }
  
  result <- st_set_crs(st_sfc(df_int$geometry), original_crs)
  
  return(result)
}
