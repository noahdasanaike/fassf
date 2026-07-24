#' @export
better_voronoi_polygons <- function (x, intersection, quiet = FALSE)
{
    # Remove the requires and use :: notation instead
    sf::sf_use_s2(FALSE)

    if (length(st_geometry(x)) != length(unique(st_geometry(x)))) {
        stop("not all geometries are unique")
    }
    original_crs <- sf::st_crs(x)
    intersection <- sf::st_make_valid(sf::st_transform(intersection,
        crs = original_crs))
    bbox <- sf::st_bbox(intersection)
    n_total <- nrow(x)

    # Points outside `intersection` get no Voronoi cell (and must not shift
    # the alignment of the cells that follow them): drop them before
    # tessellation and give them an explicit empty geometry in the output
    # instead of either a misaligned neighbor's polygon or a bogus sliver
    # clipped from their unbounded (bbox-only) cell. See GitHub issues #75
    # ("coordinates with missing Thiessen boundaries" kept in the output
    # anyway) and #84 ("obviously incorrect Thiessen assignments", e.g. a
    # remote point like Cocos Island getting assigned a nearby polygon).
    inside <- lengths(sf::st_intersects(x, sf::st_union(intersection))) > 0
    if (!quiet && any(!inside)) {
        print(paste(sum(!inside), "of", n_total,
            "points fall outside `intersection` and will receive an empty geometry"))
    }
    x_in <- x[inside, ]
    if (nrow(x_in) == 0) {
        stop("no points fall inside `intersection`")
    }

    if (!quiet)
        print("converting to spatial format")
    x_sp <- sf::as_Spatial(x_in)

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

    if(!z$n.data == nrow(crds)){
        stop("geometries dropped from falling outside transformed bounding box; remember that bbox is transformed to geometry (x) crs")
    }

    w <- deldir::tile.list(z)
    polys <- vector(mode = "list", length = length(w))
    for (i in seq(along = polys)) {
        pcrds <- cbind(w[[i]]$x, w[[i]]$y)
        pcrds <- rbind(pcrds, pcrds[1, ])
        polys[[i]] <- sp::Polygons(list(sp::Polygon(pcrds)), ID = as.character(i))
    }

    if (!quiet)
        print("converting to sf format")
    # Carry the pre-clip index through as an explicit attribute column so the
    # clipped result can be re-aligned back onto x_in's row order below, even
    # when st_intersection/terra::intersect drops or splits some cells.
    SP <- sp::SpatialPolygonsDataFrame(sp::SpatialPolygons(polys),
        data = data.frame(.voronoi_id = seq_along(polys)), match.ID = FALSE)
    terra1_sf <- sf::st_as_sf(SP)
    terra1_sf_valid <- sf::st_make_valid(terra1_sf)

    if (inherits(sf::st_geometry(intersection), "sfc_GEOMETRYCOLLECTION")) {
        df_int <- sf::st_intersection(sf::st_set_crs(terra1_sf_valid,
            original_crs), intersection)
    }
    else {
        terra1 <- terra::vect(terra1_sf_valid)
        terra2 <- terra::vect(sf::st_union(intersection))
        df_int <- terra::intersect(terra1, terra2) %>% sf::st_as_sf()
    }

    # A single input cell can be split into multiple pieces by clipping
    # (e.g. a boundary with disjoint parts); collapse back to one geometry
    # per .voronoi_id before re-indexing.
    ids <- sort(unique(df_int$.voronoi_id))
    unioned <- lapply(ids, function(id) {
        sf::st_union(sf::st_geometry(df_int)[df_int$.voronoi_id == id])
    })

    out_geom <- sf::st_sfc(lapply(seq_len(n_total), function(i) sf::st_polygon()),
        crs = original_crs)
    in_idx <- which(inside)
    out_geom[in_idx[ids]] <- do.call(c, unioned)
    return(out_geom)
}
