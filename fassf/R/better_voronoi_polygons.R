#' @export
better_voronoi_polygons <- function(x, intersection, quiet = FALSE) {
  require(sf)
  require(future)
  require(furrr)
  require(progressr)
  
  x <- st_transform(x, crs = 4326)
  bbox <- st_bbox(st_transform(intersection, crs = 4326))
  sf::sf_use_s2(FALSE)
  
  if (length(x) != length(unique(x))) {
    return("not all geometries are unique")
  }
  
  require(deldir)
  require(sp)
  if (!quiet) print("converting to spatial format")
  x <- as_Spatial(x)
  
  if (!quiet) print("constructing Thiessen polygons")
  crds <- if (.hasSlot(x, "coords")) x@coords else x
  z <- deldir(crds[, 1], crds[, 2], rw = c(bbox[1], bbox[3], 
                                           bbox[2], bbox[4]))
  w <- tile.list(z)
  
  polys <- vector(mode = "list", length = length(w))
  for (i in seq(along = polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1, ])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID = as.character(i))
  }
  
  if (!quiet) print("converting to sf format")
  SP <- SpatialPolygons(polys)
  final <- st_make_valid(st_geometry(st_as_sf(SP)))
  st_crs(final) <- 4326
  
  final <- st_make_valid(st_transform(final, crs = "EPSG:3857"))
  intersection <- st_make_valid(st_transform(intersection, crs = "EPSG:3857"))
  final_b <- map(final, ~st_intersection(st_set_crs(st_sfc(.), "EPSG:3857"), 
                                         st_geometry(intersection)))
  object <- st_as_sf(tibble(geometry = final)) %>% 
    rowwise() %>% 
    mutate(geometry_b = ifelse(length(st_intersection(geometry, intersection)) == 1,
                               st_intersection(geometry, intersection), NA))
  object$geometry_b <- st_sfc(object$geometry_b, crs = "EPSG:3857")

  return(object$geometry_b)
}
