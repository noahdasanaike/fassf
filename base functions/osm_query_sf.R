osm_query_sf <- function(query, filter, attempts = 10, quiet = FALSE){
  require(httr)
  require(jsonlite)
  require(sf)
  
  httr::set_config(httr::config(http_version = 0))
  
  base_url <- "https://nominatim.openstreetmap.org/search"
  
  for(i in 1:length(query)){
    if(quiet != TRUE){cat("\r", (i - 1) / length(query))}
    z <- 0
    while(z < attempts){
      Sys.sleep(1)
      response <- GET(url = base_url, query = list(q = query[i], format = "geojson",
                                                   polygon_geojson = 1))
      if(!grepl(content(response, "text"), pattern = "502 Bad Gateway")){
        break
      }else{
        z <- z + 1
      }
    }
    result <- fromJSON(content(response, "text"), flatten = TRUE)$features
    if(length(result) == 0){
      object <- data.frame(query = query[i],
                           missing = TRUE)
    }else{
      result$geometry <- NA
      for(p in 1:nrow(result)){
        geom <- tibble(lon = result$geometry.coordinates[[p]][,,1],
                       lat = result$geometry.coordinates[[p]][,,2]) %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
          summarize(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        result$geometry[p] <- st_geometry(geom)
      }
      object <- st_as_sf(result, crs = 4326)
      object <- object[, !colnames(object) %in% c("licence", "bbox", "geometry.coordinates")] 
      object$query <- query[i]; object$missing <- FALSE
      if(!missing(filter)){
        object <- st_filter(st_transform(object, crs = "EPSG:3857"),
                            st_transform(filter, crs = "EPSG:3857"))
        if(nrow(object) == 0){object <- data.frame(query = query[i])}
      }
    }
    if(i == 1){final <- object}else{final <- bind_rows(final, object)}
  }
  return(final)
}
