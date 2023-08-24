raster_polygon_values <- function(raster, polygons, quiet = FALSE){
  require(sf)
  require(raster)
  require(stars)
  require(terra)
  
  raster_file <- rast(raster)
  
  if(!quiet == TRUE){"re-projecting raster file"}
  
  raster_file <- project(raster_file, crs(polygons))
  
  if(!quiet == TRUE){"obtaining estimates"}
  
  get_values <- function(i, polygons, raster_file, quiet){
    if(!quiet == TRUE){cat("\r", i / nrow(polygons))}
    polygon <- st_as_sf(st_make_valid(polygons$geometry[i]))
    x <- tryCatch(crop(raster_file, ext(polygon)), error = function(e){NA})
    if(typeof(x) == "logical"){return(NA)}
    y <- mask(x, polygon)
    return(values(y, na.rm = T))
  }
  
  library(pbapply)
  
  final_values <- pbsapply(FUN = get_values,
                         X = 1:nrow(polygons),
                         polygons = polygons,
                         raster_file = raster_file,
                         quiet = quiet)
  
  return(final_values)
}