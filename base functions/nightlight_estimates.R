nightlight_estimates<- function(years, unit_name, 
                                shapefile_dir,
                                light_download_dir,
                                results_dir,
                                harmonized_light_option = TRUE){
  if(nzchar(system.file(package = "nightlightstats"))){
    return("please install nightlightstats: https://github.com/JakobMie/nightlightstats")
    }
  require(nightlightstats)
  shapefiles <- read_sf(paste0(dir, "/", unit_name, ".shp"))
  nightlight_download(
    time = c(as.character(min(years)),
             as.character(max(years))),
    harmonized_lights = harmonized_light_option,
    shapefile_location = paste0(dir, "/", unit_name),
    shapefiles = unit_name,
    download_shape = ".shp",
    light_location = paste0(light_download_dir, "/", unit_name))
  file.remove(list.files(paste0(light_download_dir, "/", unit_name, "/"), full.names = TRUE)[!grepl(list.files(paste0(light_download_dir, "/", unit_name, "/"), full.names = TRUE),
                                                                                             pattern = paste0(years, collapse = "|"))])
  
  for(i in 1:length(years)){
    cat("\r", i / length(years))
    nightlight_calculate(
      harmonized_lights = harmonized_light_option,
      area_names = unit_name,
      time = as.character(years[i]),
      shapefile_location = paste0(dir, "/"),
      light_location = paste0(light_download_dir, "/", unit_name),
      admlevel = 3)
    lights <- lights %>%
      dplyr::select(NAME_3, mean) %>%
      rename(!! paste0("lights_", years[i]) := mean)
    shapefiles <- shapefiles %>%
      left_join(lights)
    rm(lights)
  }
  saveRDS(shapefiles, paste0(results_dir, "/", unit_name, "_nightlights.RDS"))
}
