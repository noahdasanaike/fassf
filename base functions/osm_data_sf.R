osm_data_sf <- function(shapes, key, value, additional, additional_type, quiet = TRUE){
  library(osmdata)
  data <- tibble()
  sf::sf_use_s2(FALSE)
  if(!missing(additional)){
    if(!is.data.frame(additional) | !is.tibble(additional)){
      return("please format additional arguments as data frame or tibble with key and value columns")
    }
    if(!additional_type %in% c("and", "or")){
      return("please specify additional feature condition: 'and' or 'or'")
    }
  }
  for(i in 1:nrow(shapes)){
    if(quiet == FALSE){cat("\r", (i - 1) / nrow(shapes))}
    if(quiet == FALSE){cat("\n", "checking for cross-dateline polygons")}
    split_shapes <- st_wrap_dateline(st_transform(shapes[i,], crs = "EPSG:3857")) %>% st_cast("POLYGON") %>% 
      st_transform(crs = 4326) %>%
      rowwise() %>%
      mutate(is_east = ifelse(st_coordinates(st_centroid(geometry))[,1] > 0, 1, 0)) %>% 
      group_by(is_east) %>% 
      summarize(geometry = st_union(geometry))
    
    if(!st_crs(split_shapes) == 4326){split_shapes <- st_transform(split_shapes, crs = 4326)}
    
    for(y in 1:nrow(split_shapes)){
      boolFalse <- FALSE
      while(boolFalse == FALSE){
        if(quiet == FALSE){cat("\n", "trying download")}
        tryCatch({
          if(missing(additional)){
            dat1 <- opq(bbox = st_bbox(split_shapes$geometry[y]),
                        timeout = 10000) %>%
              add_osm_feature(key = key, value = value) %>%
              osmdata_sf()
          }else{
            if(additional_type == "and"){
              dat1 <- opq(bbox = st_bbox(split_shapes$geometry[y]),
                          timeout = 10000) %>%
                add_osm_feature(key = key, value = value)
              for(f in 1:nrow(additional)){
                if(quiet == FALSE){cat("\n", paste0("downloading additional feature ", f, " of ", f / nrow(additional)))}
                dat1 <- dat1  %>%
                  add_osm_feature(key = additional$key[f], value = additional$value[f])
              }
              dat1 <- osmdata_sf(dat1)
            }else{
              dat1 <- opq(bbox = st_bbox(split_shapes$geometry[y]),
                          timeout = 10000) %>%
                add_osm_feature(key = key, value = value) %>% 
                osmdata_sf()
              for(f in 1:nrow(additional)){
                if(quiet == FALSE){cat("\n", paste0("downloading additional feature ", f, " of ", f / nrow(additional)))}
                dat1 <- c(dat1, opq(bbox = st_bbox(split_shapes$geometry[y]),
                                   timeout = 10000) %>%
                            add_osm_feature(key = additional$key[f], value = additional$value[f])%>% 
                            osmdata_sf())
              }
            }
          }
          boolFalse <- TRUE
        }, error = function(e){
        }, finally = {})
      }
      if(quiet == FALSE){cat("\n", "filtering shapes")}
      
      values <- c("polygons", "points", "lines", "multipolygons")
      for(z in 1:length(values)){
        if(z == 1){
          tryCatch({
            dat2 <- dat1[[paste0("osm_", values[z])]] %>%
              st_make_valid() %>%
              st_filter(split_shapes$geometry[y]) %>% 
              mutate(type_osm = values[z])
          }, error = function(e){
          }, finally = {next})
          }
        else{
          dat2 <- bind_rows(dat2, dat1[[paste0("osm_", values[z])]] %>%
                              st_make_valid() %>%
                              st_filter(split_shapes$geometry[y])) %>% 
            mutate(type_osm = values[z])
        }
      }
      if(nrow(dat2) == 0){
        next
      }else{
        dat2 <- dat2 %>%
          as_data_frame()
        
        if(nrow(data) == 0){
          data <- dat2
        }else{
          data <- bind_rows(data, dat2)
        }
      }
    }
  }
 return(data)
}
