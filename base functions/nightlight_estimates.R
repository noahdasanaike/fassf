nightlight_estimates<- function(years, 
                                shapefiles,
                                unit_names,
                                shapefile_dir,
                                light_download_dir,
                                results_dir,
                                harmonized_light_option = TRUE,
                                quiet = FALSE){
  if(!nzchar(system.file(package = "nightlightstats"))){
    return("please install nightlightstats: https://github.com/JakobMie/nightlightstats")
  }
  if(!typeof(unit_names) == "character"){
    return("error: unit names must be character string")
  }
  if(!length(unique(shapefiles[[paste(unit_names)]])) == nrow(shapefiles)){
    return("error: more data observations than unit names")
  }
  require(nightlightstats)
  require(R.utils)
  if(!dir.exists(shapefile_dir)){
    dir.create(shapefile_dir, recursive = TRUE)
  }
  if(file.exists(paste0(shapefile_dir, "nightlight_shapefiles.shp"))){
    file.remove(paste0(shapefile_dir, "nightlight_shapefiles.shp"))
  }
  if(quiet == FALSE){print("writing shapefiles")}
  shapefiles$NAME_3 <- shapefiles[[paste0(unit_names)]]
  st_write(shapefiles, paste0(shapefile_dir, "nightlight_shapefiles.shp"))

  if(!dir.exists(light_download_dir)){dir.create(light_download_dir, recursive = TRUE)}
  if(!dir.exists(results_dir)){dir.create(results_dir, recursive = TRUE)}
  if(quiet == FALSE){print("reading shapefiles")}
  shapefiles <- read_sf(paste0(shapefile_dir, "nightlight_shapefiles.shp"))
  if(quiet == FALSE){print("downloading nightlights")}
  shapefiles$area_name <- "nightlight_shapefiles"
  nightlight_download(
    area_names = "nightlight_shapefiles",
    time = c(as.character(min(years)),
             as.character(max(years))),
    harmonized_lights = harmonized_light_option,
    shapefile_location = paste0(shapefile_dir, "nightlight_shapefiles.shp"),
    light_location = light_download_dir)
  file.remove(list.files(paste0(light_download_dir, "/", shapefiles, "/"), full.names = TRUE)[!grepl(list.files(paste0(light_download_dir, "/", shapefiles, "/"), full.names = TRUE),
                                                                                             pattern = paste0(years, collapse = "|"))])
  if(quiet == FALSE){print("generating estimates")}
  
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
    rm(lights)
  }
  saveRDS(shapefiles, paste0(results_dir, "/", shapefiles, "_nightlights.RDS"))
}