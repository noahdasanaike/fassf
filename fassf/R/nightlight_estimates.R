#' @export

nightlight_estimates <- function(years, polygons, identifier, quiet = FALSE){
  require(sf)
  require(raster)
  require(stars)
  require(terra)
  urls <- data.frame(year = 1992:2021,
                     urls = c("https://figshare.com/ndownloader/files/17626052",
                              "https://figshare.com/ndownloader/files/17626055",
                              "https://figshare.com/ndownloader/files/17626061",
                              "https://figshare.com/ndownloader/files/17626067",
                              "https://figshare.com/ndownloader/files/17626070",
                              "https://figshare.com/ndownloader/files/17626073",
                              "https://figshare.com/ndownloader/files/17626079",
                              "https://figshare.com/ndownloader/files/17626082",
                              "https://figshare.com/ndownloader/files/17626085",
                              "https://figshare.com/ndownloader/files/17626088",
                              "https://figshare.com/ndownloader/files/17626091",
                              "https://figshare.com/ndownloader/files/17626094",
                              "https://figshare.com/ndownloader/files/17626097",
                              "https://figshare.com/ndownloader/files/17626100",
                              "https://figshare.com/ndownloader/files/17626103",
                              "https://figshare.com/ndownloader/files/17626109",
                              "https://figshare.com/ndownloader/files/17626016",
                              "https://figshare.com/ndownloader/files/17626019",
                              "https://figshare.com/ndownloader/files/17626022",
                              "https://figshare.com/ndownloader/files/17626025",
                              "https://figshare.com/ndownloader/files/17626031",
                              "https://figshare.com/ndownloader/files/17626034",
                              "https://figshare.com/ndownloader/files/17626037",
                              "https://figshare.com/ndownloader/files/17626040",
                              "https://figshare.com/ndownloader/files/17626043",
                              "https://figshare.com/ndownloader/files/17626046",
                              "https://figshare.com/ndownloader/files/17626049",
                              "https://figshare.com/ndownloader/files/26477462",
                              "https://figshare.com/ndownloader/files/28166733",
                              "https://figshare.com/ndownloader/files/34751056"))
  dir.create(".temp_lights/")
  for(i in 1:length(years)){
    if(isFALSE(quiet)){cat("\r", i / length(years))}
    download.file(urls$urls[urls$year == years[i]],
                  paste0(".temp_lights/", years[i], ".tif"), mode = "wb")
    values <- raster_polygon_values(paste0(".temp_lights/", years[i], ".tif"),
                                         st_geometry(polygons),
                                    quiet = quiet)
    values <- unlist(map(values, ~mean(.)))
    out <- data.frame(year = rep(years[i], length(values)),
                      identifier = polygons[[identifier]],
                      nightlight_means = values)
    names(out)[names(out) == "identifier"] <- identifier
    if(i == 1){all_out <- out}else{all_out <- bind_rows(all_out, out)}
  }
  unlink(".temp_lights", recursive = TRUE)
  return(all_out)
}