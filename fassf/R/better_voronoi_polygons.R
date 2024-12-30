#' @export
better_voronoi_polygons <- function(x, intersection, workers = parallel::detectCores() - 1, parallel = FALSE, quiet = FALSE) {
  x <- st_transform(x, crs = 4326)
  sf::sf_use_s2(FALSE)
  
  if (length(x) != length(unique(x))) {
    return("not all geometries are unique")
  }
  
  if (!parallel) {
    # Non-parallel method
    require(deldir)
    require(sp)
    
    if (!quiet) {
      print("converting to spatial format")
    }
    
    x <- as_Spatial(x)
    
    if (!quiet) {
      print("constructing Thiessen polygons")
    }
    
    if (.hasSlot(x, "coords")) {
      crds <- x@coords
    } else {
      crds <- x
    }
    z <- deldir(crds[,1], crds[,2])
    w <- tile.list(z)
    polys <- vector(mode = "list", length = length(w))
    
    for (i in seq(along = polys)) {
      pcrds <- cbind(w[[i]]$x, w[[i]]$y)
      pcrds <- rbind(pcrds, pcrds[1,])
      polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
    }
    
    if (!quiet) {
      print("converting to sf format")
    }
    
    SP <- SpatialPolygons(polys)
    final <- st_make_valid(st_geometry(st_as_sf(SP)))
    st_crs(final) <- 4326
  } else {
    # Parallel method
    if (!quiet) {
      message("converting to spatial format")
    }
    x_sp <- as(x, "Spatial")
    
    if (!quiet) {
      message("constructing Thiessen polygons")
    }
    
    crds <- coordinates(x_sp)
    z <- deldir(crds[, 1], crds[, 2])
    w <- tile.list(z)
    
    plan(multisession, workers = workers)
    
    with_progress({
      p <- progressor(steps = length(w))
      
      polys <- future_map(seq_along(w), function(i) {
        pcrds <- cbind(w[[i]]$x, w[[i]]$y)
        pcrds <- rbind(pcrds, pcrds[1, ])
        poly <- Polygons(list(Polygon(pcrds)), ID = as.character(i))
        p()
        poly
      }, .options = furrr_options(seed = TRUE))
    })
    
    if (!quiet) {
      message("converting to sf format")
    }
    
    SP <- SpatialPolygons(polys)
    SP <- SpatialPolygonsDataFrame(SP, data = data.frame(id = row.names(SP)), match.ID = FALSE)
    final <- st_as_sf(SP)
    final <- st_make_valid(st_geometry(final))
    st_crs(final) <- 4326
  }
  
  if (missing(intersection)) {
    return(final)
  } else {
    if (!parallel) {
      # Non-parallel intersection method
      final <- st_make_valid(st_transform(final, crs = "EPSG:3857"))
      intersection <- st_make_valid(st_transform(intersection, crs = "EPSG:3857"))
      
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
    } else {
      # Parallel intersection method
      final <- st_make_valid(st_transform(final, crs = 3857))
      intersection <- st_make_valid(st_transform(intersection, crs = 3857))
      
      with_progress({
        p <- progressor(steps = length(final))
        
        final_b <- future_map(st_geometry(final), function(geom) {
          result <- st_intersection(st_set_crs(st_sfc(geom), 3857), st_geometry(intersection))
          p()
          result
        }, .options = furrr_options(seed = TRUE))
      })
      
      object <- st_as_sf(tibble(geometry = st_geometry(final))) %>%
        mutate(geometry_b = final_b)
      
      object$geometry_b[unlist(map(object$geometry_b, ~length(.))) == 0][[1]] <- st_polygon()
      
      safe_extract <- function(geom, i) {
        tryCatch({
          if (length(geom) == 0) {
            st_geometrycollection()  # Return empty geometry for length 0
          } else {
            geom[[1]]  # Extract the first (and usually only) geometry
          }
        }, error = function(e) {
          message("Error in geometry ", i, ": ", e$message)
          st_geometrycollection()  # Return empty geometry on error
        })
      }
      
      extracted_geometries <- map2(object$geometry_b, seq_along(object$geometry_b), safe_extract)
      
      result_sfc <- st_sfc(extracted_geometries, crs = st_crs(object$geometry_b[[1]]))
      return(result_sfc)
    }
  }
}
