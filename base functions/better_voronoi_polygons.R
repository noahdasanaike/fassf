better_voronoi_polygons <- function(x, quiet = FALSE,
                                    intersection){
  # adapted from carson farmer
  x <- st_transform(x, crs = 4326)
  
  if(length(x) != length(unique(x))){
    return("not all geometries are unique")
  }
  
  require(deldir)
  require(sp)
  
  if(!quiet == TRUE){print("converting to spatial format")}
  
  x <- as_Spatial(x)
  
  if(!quiet == TRUE){print("constructing Thiessen polygons")}
  
  if(.hasSlot(x, "coords")){crds <- x@coords}else{crds <- x}
  z <- deldir(crds[,1], crds[,2])
  w <- tile.list(z)
  polys <- vector(mode = "list", length = length(w))
  
  for (i in seq(along = polys)) {
    pcrds <- cbind(w[[i]]$x, w[[i]]$y)
    pcrds <- rbind(pcrds, pcrds[1,])
    polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  
  if(!quiet == TRUE){print("converting to sf format")}
  
  SP <- SpatialPolygons(polys)
  final <- st_geometry(st_as_sf(SP))
  st_crs(final) <- 4326
  if(missing(intersection)){
    return(final)
  }else{
    final <- st_transform(final, crs = "EPSG:3857")
    intersection <- st_transform(intersection, crs = "EPSG:3857")
    
    final_b <- map(final, 
                 ~st_intersection(st_set_crs(st_sfc(.), 
                                             "EPSG:3857"), st_geometry(intersection)))
    
    object <- st_as_sf(tibble(geometry = final)) %>% 
      rowwise() %>%
      mutate(geometry_b = ifelse(length(st_intersection(geometry, intersection)) == 1,
                                 st_intersection(geometry, intersection),
                                 NA))
    
    object$geometry_b <- st_sfc(object$geometry_b, crs = "EPSG:3857")
    
    return(object$geometry_b)
  }
}
