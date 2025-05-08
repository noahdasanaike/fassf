#' @export

ruggedness_mad <- function(polygons, z_level = 7, quiet = FALSE, 
                           override_size_check = FALSE, 
                           split = TRUE, split_chunks = 10){
  require(spatialEco)
  require(elevatr)
  
  suppressWarnings({
    suppressMessages({
      if(!st_is_longlat(polygons)){
        if(quiet == FALSE){print("correcting CRS")}
        
        polygons <- polygons %>% st_as_sf() %>% st_transform(crs = "EPSG:4326")
      }
      
      tri_fun <- function(x, w) {
        x <- x[!is.na(x)]
        return(sqrt(sum(((median(x) - x)^2))))
      }
      
      polygons <- st_as_sf(polygons)
      
      if(split == TRUE){
        if(quiet == FALSE){cat("\r", 
                               paste0("generating elevation raster and calculating ruggedness, by chunk"))}
        
        
        split <- polygons %>% group_by(row_number() %/% (nrow(polygons) / split_chunks)) %>% group_map(~ .x)
    
        ruggedness <- c()
        
        for(i in 1:length(split)){
          if(quiet == FALSE){cat("\r", paste0(round(100 * i / split_chunks, 2), "%"))}
          elev <- NA
          
          # mun_sf <- as(split[[i]], "Spatial")
          mun_sf <- st_cast(split[[i]], "MULTIPOLYGON")
          
          tryCatch({elev <- get_elev_raster(mun_sf, z = z_level, verbose = !quiet, 
                                            override_size_check = override_size_check)}, 
                   error = function(e){if(grepl(e, pattern = "gdal_utils warp: an error occured")){
                     return(cat("\r", "gdal_utils warp error: likely too much memory requested"))
                   }}, finally = {})
          
          ruggedness <- c(ruggedness, as.numeric(exactextractr::exact_extract(elev, split[[i]], fun = tri_fun,
                                                                              progress = !quiet)))
        }
      }else{
        if(quiet == FALSE){print("converting polygons to spatial")}
        
        # mun_sf <- as(polygons, "Spatial")
        mun_sf <- st_cast(split[[i]], "MULTIPOLYGON")
        
        if(quiet == FALSE){print("generating elevation raster")}
        
        tryCatch({elev <- get_elev_raster(mun_sf, z = z_level, verbose = !quiet, 
                                          override_size_check = override_size_check)}, 
                 error = function(e){if(grepl(e, pattern = "gdal_utils warp: an error occured")){
                   return(cat("\r", "gdal_utils warp error: likely too much memory requested"))
                 }}, finally = {})
        
        if(quiet == FALSE){print("calculating ruggedness")}
        
        ruggedness <- as.numeric(exactextractr::exact_extract(elev, polygons, fun = tri_fun,
                                                              progress = !quiet))
      }
      return(ruggedness)
    })
  })
}
