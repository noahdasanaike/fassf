osm_data_sf <- function(shapes, key, value, quiet = TRUE){
  library(osmdata)
  data <- tibble()
  for(i in 1:nrow(shapes)){
    if(quiet == FALSE){cat("\r", (i - 1) / nrow(shapes))}
    if(quiet == FALSE){cat("\n", "checking for cross-dateline")}
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
          dat1 <- opq(bbox = st_bbox(split_shapes$geometry[y]),
                      timeout = 10000) %>%
            add_osm_feature(key = key, value = value) %>%
            osmdata_sf()
          boolFalse <- TRUE
        }, error = function(e){
        }, finally = {})
      }
      if(quiet == FALSE){cat("\n", "filtering shapes")}
      
      values <- c("polygons", "points", "lines", "multipolygons")
      for(z in 1:length(values)){
        if(z == 1){
          dat2 <- dat1[[paste0("osm_", values[z])]] %>%
            st_filter(split_shapes$geometry[y]) %>% 
            mutate(type_osm = values[z])
          }
        else{
          dat2 <- bind_rows(dat2, dat1[[paste0("osm_", values[z])]] %>%
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
