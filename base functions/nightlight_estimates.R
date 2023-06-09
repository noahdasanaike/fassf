nightlight_estimates<- function(years, 
                                shapefiles,
                                unit_names,
                                shapefile_dir,
                                light_download_dir,
                                results_dir,
                                harmonized_light_option = TRUE,
                                quiet = FALSE,
                                parallel = TRUE){
  if(as.numeric(years)){years <- as.character(years)}
  devtools::unload("sf")
  Sys.setenv("PROJ_NETWORK" = "ON")
  library(sf)
  if(!nzchar(system.file(package = "nightlightstats"))){
    return("please install nightlightstats: https://github.com/JakobMie/nightlightstats")
  }
  if(!typeof(unit_names) == "character"){
    return("error: unit names must be type character")
  }
  if(!length(unique(shapefiles[[paste(unit_names)]])) == nrow(shapefiles)){
    return("error: more data observations than unit names")
  }
  if(substr(shapefile_dir, nchar(shapefile_dir), nchar(shapefile_dir)) != "/" |
     substr(light_download_dir, nchar(light_download_dir), nchar(light_download_dir)) != "/"| 
     substr(results_dir, nchar(results_dir), nchar(results_dir)) != "/"){
    return("please add forward slash to end of directory calls")
  }
  require(nightlightstats)
  require(R.utils)
  if(!dir.exists(shapefile_dir)){dir.create(shapefile_dir, recursive = TRUE)}
  if(!dir.exists(light_download_dir)){dir.create(light_download_dir, recursive = TRUE)}
  if(!dir.exists(results_dir)){dir.create(results_dir, recursive = TRUE)}
  
  if(harmonized_light_option == TRUE){
    if(max(as.numeric(years)) > 2018){
      if(quiet == FALSE){print("years greater than 2018, checking for manually added files")}
      years_greater_2018 <- years[years > 2018]
      for(y in 1:length(years_greater_2018)){
        if(y == 1){
          check <- sum(grepl(list.files(night_download_dir, pattern = ".tif"), pattern = years_greater_2018[y]))}else{
            check <- check + sum(grepl(list.files(night_download_dir, pattern = ".tif"), pattern = years_greater_2018[y]))
          }
      }
      if(check != length(years_greater_2018)){
        return(paste0("years greater than 2018, manually add to lights folder if desired. URL: https://figshare.com/articles/dataset/Harmonization_of_DMSP_and_VIIRS_nighttime_light_data_from_1992-2018_at_the_global_scale/9828827"))
      }else{
        manual <- TRUE
        years_download <- years[!years %in% years_greater_2018]
      }
    }else{
      manual <- FALSE
      years_download <- years
    }
  }else{
    if(max(as.numeric(years)) > 2013){
      years_greater_2018 <- years[years > 2013]
      for(y in 1:length(years_greater_2018)){
        if(y == 1){
          check <- sum(grepl(list.files(night_download_dir, pattern = ".tif"), pattern = years_greater_2018[y]))}else{
            check <- check + sum(grepl(list.files(night_download_dir, pattern = ".tif"), pattern = years_greater_2018[y]))
          }
      }
      if(check != length(years_greater_2018)){
        return(paste0("years greater than 2018, manually add to lights folder if desired.\nURL: "))
      }else{
        manual <- TRUE
        years_download <- years[!years %in% years_greater_2018]
      }
    }else{
      manual <- FALSE
      years_download <- years
    }
  }
  
  
  if(file.exists(paste0(shapefile_dir, "nightlight_shapefiles.shp"))){
    file.remove(paste0(shapefile_dir, "nightlight_shapefiles.shp"))
  }
  
  if(!st_crs(shapefiles) == "EPSG:3857"){
    if(quiet == FALSE){print("fixing shapefile crs")}
    shapefiles <- st_transform(shapefiles, crs = "EPSG:3857")
  }
  
  if(quiet == FALSE){print("writing shapefiles")}
  shapefiles$NAME_3 <- shapefiles[[paste0(unit_names)]]
  
  st_write(shapefiles, paste0(shapefile_dir, "nightlight_shapefiles.shp"))

  if(quiet == FALSE){print("reading shapefiles")}
  shapefiles <- read_sf(paste0(shapefile_dir, "nightlight_shapefiles.shp")) %>%
    st_make_valid()
  if(quiet == FALSE){print("downloading nightlights")}
  shapefiles$area_name <- "nightlight_shapefiles"
  if(length(years_download) != 0){
    nightlight_download(
      area_names = "nightlight_shapefiles",
      time = c(as.character(min(years_download)),
               as.character(max(years_download))),
      harmonized_lights = harmonized_light_option,
      shapefile_location = paste0(shapefile_dir, "nightlight_shapefiles.shp"),
      light_location = light_download_dir)
  }else{
    if(quiet == FALSE){print("skipping download")}
  }
  file.remove(list.files(light_download_dir, full.names = TRUE)[!grepl(list.files(light_download_dir, full.names = TRUE),
                                                                       pattern = paste0(years, collapse = "|"))])
  if(quiet == FALSE){print("generating estimates")}
  existing_nightlights <- list.files("nightlights/results",
                                     recursive = TRUE, full.names = TRUE)
  if(length(existing_nightlights) > 0){
    repeat{
      delete_old <- readline(prompt = "Existing nightlight output detected. Delete (y) or fill missing (n)?")
      if(delete_old %in% c("y", "n")){break}
    }
  }else{
    delete_old <- "n"
  }
  if(parallel == TRUE){
    nightlights <- function(i, years, shapefiles){
      invisible(nightlight_calculate(
        harmonized_lights = harmonized_light_option,
        area_names = "nightlight_shapefiles",
        time = as.character(years[i]),
        shapefile_location = shapefile_dir,
        light_location = light_download_dir,
        admlevel = 3))
      invisible(lights <- lights %>%
                  dplyr::select_at(vars(c(NAME_3, mean))) %>%
                  rename(!! paste0("lights_", years[i]) := mean))
      invisible(shapefiles <- shapefiles %>%
                  left_join(lights))
      saveRDS(shapefiles, paste0(results_dir, "nightlights", years[i],".RDS"))
    }
    
    library(parallel)
    library(pbapply)
    cl <- makeCluster(detectCores() - 1)
    clusterEvalQ(cl, c(library(sf), library(nightlightstats), library(tidyverse)))
    clusterExport(cl, c("shapefile_dir",
                        "light_download_dir",
                        "harmonized_light_option",
                        "results_dir"))
    
    if(delete_old == "y"){
      if(length(existing_nightlights) > 0){unlink(existing_nightlights)}
      pbsapply(cl = cl, X = 1:length(years), 
               FUN = nightlights, 
               years = years,
               shapefiles = shapefiles)
      stopCluster(cl)
    }else{
      if(length(existing_nightlights) > 0){
        existing_nightlights <- as.numeric(substr(unlist(strsplit(existing_nightlights, "results/nightlights"))[c(FALSE, TRUE)], 1, 4))
        years <- years[!years %in% existing_nightlights]
      }
      pbsapply(cl = cl, X = 1:length(years), 
               FUN = nightlights, 
               years = years,
               shapefiles = shapefiles)
      stopCluster(cl)
    }
  }else{
    if(delete_old == "y"){
      if(length(existing_nightlights) > 0){unlink(existing_nightlights)}
      for(i in 1:length(years)){
        cat("\r", i / length(years))
        invisible(nightlight_calculate(
          harmonized_lights = harmonized_light_option,
          area_names = "nightlight_shapefiles",
          time = as.character(years[i]),
          shapefile_location = shapefile_dir,
          light_location = light_download_dir,
          admlevel = 3))
        invisible(lights <- lights %>%
                    dplyr::select_at(vars(c(NAME_3, mean))) %>%
                    rename(!! paste0("lights_", years[i]) := mean))
        invisible(shapefiles <- shapefiles %>%
                    left_join(lights))
        saveRDS(shapefiles, paste0(results_dir, "nightlights", years[i],".RDS"))
      }
    }else{
      if(length(existing_nightlights) > 0){
        existing_nightlights <- as.numeric(substr(unlist(strsplit(existing_nightlights, "results/nightlights"))[c(FALSE, TRUE)], 1, 4))
        years <- years[!years %in% existing_nightlights]
      }
      
      for(i in 1:length(years)){
        cat("\r", i / length(years))
        invisible(nightlight_calculate(
          harmonized_lights = harmonized_light_option,
          area_names = "nightlight_shapefiles",
          time = as.character(years[i]),
          shapefile_location = shapefile_dir,
          light_location = light_download_dir,
          admlevel = 3))
        invisible(lights <- lights %>%
                    dplyr::select_at(vars(c(NAME_3, mean))) %>%
                    rename(!! paste0("lights_", years[i]) := mean))
        invisible(shapefiles <- shapefiles %>%
                    left_join(lights))
        saveRDS(shapefiles, paste0(results_dir, "nightlights", years[i],".RDS"))
      }
    }
  }
}
