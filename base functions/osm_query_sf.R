osm_query_sf <- function(query, filter, attempts = 10, quiet = FALSE){
  require(httr)
  require(jsonlite)
  require(sf)
  
  base_url <- "https://nominatim.openstreetmap.org/search"
  
  for(i in 1:length(query)){
    if(quiet != TRUE){cat("\r", (i - 1) / length(query))}
    z <- 0
    while(z < attempts){
      Sys.sleep(1)
      response <- GET(url = base_url, query = list(q = query[i], format = "json"))
      if(!grepl(content(response, "text"), pattern = "502 Bad Gateway")){
        break
      }else{
        z <- z + 1
      }
    }
    result <- fromJSON(content(response, "text"), flatten = TRUE)
    if(length(result) == 0){
      object <- data.frame(query = query[i],
                           missing = TRUE)
    }else{
      object <- st_as_sf(result,
                         coords = c("lon", "lat"),
                         remove = FALSE,
                         crs = 4326)
      object <- object[, !colnames(object) %in% c("licence", "boundingbox")] 
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
