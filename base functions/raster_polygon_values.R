raster_polygon_values <- function(raster, polygons, quiet = FALSE){
  raster_file <- rast("satellite/science_tcc_CONUS_2021_v2021-4/science_tcc_conus_2021_v2021-4.tif")
  
  if(!quiet == TRUE){"re-projecting raster file"}
  
  raster_file <- project(raster_file, crs(polygons))
  
  get_values <- function(i, polygons, raster_file, quiet){
    if(!quiet == TRUE){cat("\r", i / nrow(polygons))}
    polygon <- st_as_sf(st_make_valid(polygons$geometry[i]))
    x <- crop(raster_file, ext(polygon))
    y <- mask(x, polygon)
    return(values(y, na.rm = T))
  }
  
  library(pbapply)
  
  final_values <- sapply(FUN = get_values,
                         X = 1:nrow(polygons),
                         polygons = polygons,
                         raster_file = raster_file,
                         quiet = quiet)
  
  return(final_values)
}