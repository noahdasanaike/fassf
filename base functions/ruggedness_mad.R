ruggedness_mad <- function(polygons, z_level = 7, quiet = FALSE, override_size_check = FALSE){
  require(spatialEco)
  require(elevatr)
  
  if(!st_is_longlat(polygons)){
    if(quiet == FALSE){print("correcting CRS")}
    
    polygons <- polygons %>% st_as_sf() %>% st_transform(crs = "EPSG:4326")
  }
  
  tri_fun <- function(x, w) {
    x <- x[!is.na(x)]
    return(sqrt(sum(((median(x) - x)^2))))
  }
  
  if(quiet == FALSE){print("converting polygons")}
  
  polygons <- st_as_sf(polygons)
  
  mun_sf <- as(polygons, "Spatial")
  
  if(quiet == FALSE){print("generating elevation raster")}
  
  tryCatch({elev <- get_elev_raster(mun_sf, z = z_level, verbose = !quiet, 
                                    override_size_check = override_size_check)}, 
           error = function(e){if(grepl(e, pattern = "gdal_utils warp: an error occured")){
             return(cat("\r", "gdal_utils warp error: likely too much memory requested"))
           }}, finally = {})
  
  if(quiet == FALSE){print("calculating ruggedness")}
  
  return(as.numeric(exactextractr::exact_extract(elev, polygons, fun = tri_fun,
                                                 progress = !quiet)))
}
