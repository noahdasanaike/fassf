osm_data_sf <- function(shapes, key, value, return, quiet = TRUE){
  library(osmdata)
  data <- tibble()
  for(i in 1:nrow(shapes)){
    if(quiet == FALSE){cat("\r", (i - 1) / nrow(shapes))}
    split_shapes <- st_wrap_dateline(shapes[i,]) %>% st_cast("POLYGON") %>% 
      rowwise() %>%
      mutate(is_east = ifelse(st_coordinates(st_centroid(geometry))[,1] > 0, 1, 0)) %>% 
      group_by(is_east) %>% 
      summarize(geometry = st_union(geometry))
    
    for(y in 1:nrow(split_shapes)){
      boolFalse <- FALSE
      while(boolFalse == FALSE)
      {
        tryCatch({
          dat1 <- opq(bbox = st_bbox(split_shapes$geometry[y]),
                      timeout = 10000) %>%
            add_osm_feature(key = key, value = value) %>%
            osmdata_sf()
          boolFalse <- TRUE
        }, error = function(e){
        }, finally = {})
      }
      
      dat2 <- dat1[["osm_polygons"]] %>%
        st_filter(split_shapes$geometry[y])
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
