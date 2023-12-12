#' @export

raster_polygon_values <- function(raster, polygons, quiet = FALSE, fun = NULL, drop_na = TRUE){
  require(sf)
  require(raster)
  require(stars)
  require(terra)
  
  raster_file <- rast(raster)
  
  if(!quiet == TRUE){"re-projecting raster file"}
  
  raster_file <- tryCatch({terra::project(raster_file, crs(polygons))}, error = function(e){return(e)})
  if(grepl(raster_file, pattern = "incorrect number of values")){
    polygons <- st_transform(polygons, crs = "EPSG:3857")
    raster_file <- rast(raster)
    raster_file <- tryCatch({project(raster_file, crs(polygons))}, error = function(e){return(e)})
  }
  if(!quiet == TRUE){"obtaining estimates"}
  
  polygon_geometry <- st_geometry(polygons)
  
  get_values <- function(i, polygons, raster_file, quiet, fun, drop_na){
    if(!quiet == TRUE){cat("\r", i / length(polygon_geometry))}
    polygon <- st_as_sf(st_make_valid(polygon_geometry[i]))
    x <- tryCatch(crop(raster_file, ext(polygon)), error = function(e){NA})
    if(typeof(x) == "logical"){return(NA)}
    y <- mask(x, polygon)
    if(is.null(fun)){
      return(values(y, na.rm = drop_na))
    }else{
      return(lapply(list(values(y, na.rm = drop_na)), get(fun)))
    }
  }
  
  library(pbapply)
  
  final_values <- pbsapply(FUN = get_values,
                         X = 1:length(polygon_geometry),
                         polygons = polygons,
                         raster_file = raster_file,
                         quiet = quiet,
                         fun = fun,
                         drop_na = drop_na)
  
  return(final_values)
}