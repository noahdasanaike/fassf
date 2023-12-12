osm_query_sf <- function(query, filter, attempts = 10, quiet = FALSE){
  require(httr)
  require(jsonlite)
  require(sf)
  require(dplyr)
  
  httr::set_config(httr::config(http_version = 0))
  
  base_url <- "https://nominatim.openstreetmap.org/search"
  
  for(i in 1:length(query)){
    if(!is.character(query[i])){return("query not string")}
    if(quiet != TRUE){cat("\r", (i - 1) / length(query))}
    z <- 0
    while(z < attempts){
      Sys.sleep(1)
      response <- GET(url = base_url, query = list(q = query[i], format = "geocodejson",
                                                   polygon_geojson = 1))
      if(!grepl(content(response, "text"), pattern = "502 Bad Gateway")){
        break
      }else{
        z <- z + 1
      }
    }
    result <- fromJSON(content(response, "text"), flatten = TRUE)[["features"]]
    if(length(result) == 0){
      object <- data.frame(query = query[i],
                           missing = TRUE)
    }else{
      result$geometry <- NA
      for(p in 1:nrow(result)){
        if(result$geometry.type[p] == "Point"){
          geom <- tibble(lon = result$geometry.coordinates[[p]][1],
                         lat = result$geometry.coordinates[[p]][2]) %>%
            st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
            st_cast("POINT")
        }else if (result$geometry.type[p] == "LineString"){
          geom <- tibble(lon = result$geometry.coordinates[[p]][,1],
                         lat = result$geometry.coordinates[[p]][,2]) %>%
            st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
            summarize(geometry = st_combine(geometry)) %>%
            st_cast("MULTILINESTRING")
        }else if (result$geometry.type[p] == "MultiLineString"){
         if(is.null(dim(result$geometry.coordinates[[p]]))){
           geom <- map(result$geometry.coordinates[[p]],
                       ~tibble(lon = .[,1],
                               lat = .[,2]) %>%
                         st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
                         summarize(geometry = st_combine(geometry)) %>%
                         st_cast("LINESTRING"))
           
           geom <- do.call(rbind, geom) %>% 
             summarize(geometry = st_combine(geometry)) %>%
             st_cast("MULTILINESTRING")
         }
         else if(dim(result$geometry.coordinates[[p]])[1] > 1){
           geom <- map(result$geometry.coordinates[[p]],
                       ~tibble(lon = .[,1],
                               lat = .[,2]) %>%
                         st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
                         summarize(geometry = st_combine(geometry)) %>%
                         st_cast("LINESTRING"))
           
           geom <- do.call(rbind, geom) %>% 
             summarize(geometry = st_combine(geometry)) %>%
             st_cast("MULTILINESTRING")
             
         }else{
           geom <- tibble(lon = result$geometry.coordinates[[p]][,,1],
                          lat = result$geometry.coordinates[[p]][,,2]) %>%
             st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
             summarize(geometry = st_combine(geometry)) %>%
             st_cast("MULTILINESTRING")
         }
        }else if (result$geometry.type[p] == "Polygon"){
          if(!is.null(dim(result$geometry.coordinates[[p]]))){
            geom <- tibble(lon = result$geometry.coordinates[[p]][,,1],
                         lat = result$geometry.coordinates[[p]][,,2]) %>%
            st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
            summarize(geometry = st_combine(geometry)) %>%
            st_cast("POLYGON")
          }else{
            for(k in 1:length(result$geometry.coordinates[[p]])){
              geom_part <- tibble(lon = result$geometry.coordinates[[p]][[k]][,1],
                             lat = result$geometry.coordinates[[p]][[k]][,2]) %>%
                st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
                summarize(geometry = st_combine(geometry)) %>%
                st_cast("POLYGON") %>%
                st_make_valid()
              if(k == 1){geom <- geom_part}else{geom <- bind_rows(geom, geom_part)}
            }
            geom <- st_combine(geom)
          }
        }else{
          polygon_list <- lapply(result$geometry.coordinates[[p]], function(p) {
            if (is.list(p)) {
              lapply(p, matrix, ncol = 2, byrow = TRUE)
            } else {
              matrix(p, ncol = 2, byrow = TRUE)
            }
          })
          corrected_polygon_list <- lapply(polygon_list, function(p) {
            if (is.matrix(p)) {
              list(p)
            } else {
              p
            }
          })
          rearrange_coords <- function(mat) {
            n <- nrow(mat) * ncol(mat)
            half_n <- n / 2
            cbind(c(t(mat))[1:half_n], c(t(mat))[(half_n + 1):n])
          }
          
          corrected_coords_list <- lapply(corrected_polygon_list, function(poly) {
            lapply(poly, rearrange_coords)
          })
          
          geom <- st_multipolygon(corrected_coords_list)
          geom <- st_geometry(geom); st_crs(geom) <- 4326
        }
        result$geometry[p] <- geom
      }
      result$geometry <- sapply(result$geometry, st_sfc)
      object <- st_as_sf(result, crs = 4326)
      object <- object[, !colnames(object) %in% c("licence", "bbox", "geometry.coordinates")] 
      object$query <- query[i]; object$missing <- FALSE
      if(nrow(object) > 0){object$geometry <- st_make_valid(object$geometry)}
      if(!missing(filter)){
        object <- st_filter(st_transform(object, crs = "EPSG:3857"),
                            st_transform(filter, crs = "EPSG:3857"))
        if(nrow(object) == 0){object <- data.frame(query = query[i])}
      }
      if(nrow(object) == 1 & ncol(object) == 1){
        object <- data.frame(query = query[i],
                             missing = TRUE)
      }
    }
    if(i == 1){final <- object}else{final <- bind_rows(final, object)}
  }
  return(final)
}
