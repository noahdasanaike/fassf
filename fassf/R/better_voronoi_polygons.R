#' @export
better_voronoi_polygons <- function (x, intersection, quiet = FALSE) 
{
    # Remove the requires and use :: notation instead
    sf::sf_use_s2(FALSE)
    
    if (length(st_geometry(x)) != length(unique(st_geometry(x)))) {
        return("not all geometries are unique")
    }
    original_crs <- sf::st_crs(x)
    bbox <- sf::st_bbox(sf::st_transform(intersection, crs = original_crs))

    if(!z$n.data == nrow(crds)){
        stop("geometries dropped from falling outside transformed bounding box; remember that bbox is transformed to geometry (x) crs")
      }
    
    if (!quiet) 
        print("converting to spatial format")
    x_sp <- sf::as_Spatial(x)
    
    if (!quiet) 
        print("constructing Thiessen polygons")
    crds <- if (.hasSlot(x_sp, "coords")) {
        x_sp@coords
    }
    else {
        x_sp
    }
    z <- deldir::deldir(crds[, 1], crds[, 2], rw = c(bbox[1], bbox[3], 
        bbox[2], bbox[4]))
    w <- deldir::tile.list(z)
    polys <- vector(mode = "list", length = length(w))
    for (i in seq(along = polys)) {
        pcrds <- cbind(w[[i]]$x, w[[i]]$y)
        pcrds <- rbind(pcrds, pcrds[1, ])
        polys[[i]] <- sp::Polygons(list(sp::Polygon(pcrds)), ID = as.character(i))
    }
    
    if (!quiet) 
        print("converting to sf format")
    SP <- sp::SpatialPolygons(polys)
    intersection <- sf::st_make_valid(sf::st_transform(intersection, 
        crs = original_crs))
    terra1_sf <- sf::st_as_sf(SP)
    terra1_sf_valid <- sf::st_make_valid(terra1_sf)
    
    if (inherits(sf::st_geometry(intersection), "sfc_GEOMETRYCOLLECTION")) {
        df_int <- sf::st_intersection(sf::st_set_crs(terra1_sf_valid, 
            original_crs), intersection)
    }
    else {
        terra1 <- terra::vect(terra1_sf_valid)
        terra2 <- terra::vect(st_union(intersection))
        df_int <- terra::intersect(terra1, terra2) %>% sf::st_as_sf()
    }
    result <- sf::st_set_crs(sf::st_sfc(df_int$geometry), original_crs)
    return(result)
}
