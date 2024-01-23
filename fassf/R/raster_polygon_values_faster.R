#' @export

raster_polygon_values_faster <- function(raster, polygons, quiet = FALSE, fun = NULL,
                                         chunks = 20,
                                         drop_na = TRUE,
                                         replace_missing = NA){
  require(sf)
  require(raster)
  require(stars)
  require(terra)
  require(pbapply)

  raster_file <- rast(raster)

  if(!quiet == TRUE){"re-projecting raster file"}

  raster_file <- tryCatch({terra::project(raster_file, crs(polygons))}, error = function(e){return(e)})
  if(grepl(raster_file, pattern = "incorrect number of values")){
    polygons <- st_transform(polygons, crs = "EPSG:3857")
    raster_file <- rast(raster)
    raster_file <- tryCatch({project(raster_file, crs(polygons))}, error = function(e){return(e)})
  }
  if(!quiet == TRUE){"obtaining estimates"}

  get_values <- function(i, chunks, polygons, raster_file, quiet, fun, replace_missing, drop_na){
    polygon <- st_as_sf(st_union(st_make_valid(st_geometry(polygons)[chunk_indices[[i]]])))
    x <- tryCatch(crop(raster_file, ext(polygon)), error = function(e){NA})
    if(typeof(x) == "logical"){return(NA)}
    y <- mask(x, polygon)
    z <- st_as_sf(st_as_stars(y))

    return(intersection_fun(st_geometry(polygons)[chunk_indices[[i]]], z,
                            colnames(z)[1], fun = fun, quiet = TRUE, replace_missing = replace_missing))
  }

  chunk_indices <- split(1:nrow(polygons), cut(seq_along(1:nrow(polygons)), chunks, labels = FALSE))

  final_values <- pbsapply(FUN = get_values,
                           X = 1:length(chunk_indices),
                           chunks = chunk_indices,
                           polygons = polygons,
                           raster_file = raster_file,
                           quiet = quiet,
                           replace_missing = replace_missing,
                           fun = fun,
                           drop_na = drop_na)

  if(!is.null(fun)){
    return <- unlist(c(final_values))
  }else{
    return <- do.call(c, final_values)
  }

  return(return)
}
