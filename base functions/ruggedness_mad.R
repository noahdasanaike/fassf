ruggedness_mad <- function(polygons, z_level = 7, quiet = FALSE){
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
  
  mun_sf <- as(polygons, "Spatial")
  
  if(quiet == FALSE){print("generating elevation raster")}
  
  elev <- get_elev_raster(mun_sf, z = z_level, verbose = !quiet)
  
  if(quiet == FALSE){print("calculating ruggedness")}
  
  return(as.numeric(exactextractr::exact_extract(elev, polygons, fun = tri_fun,
                                                 progress = !quiet)))
}