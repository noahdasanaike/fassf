osm_data_sf <- function(shapes, key, value){
  data <- tibble()
  for(i in 1:nrow(shapes)){
    cat("\r", i / nrow(shapes))
    dat1 <- opq(bbox = shapes$bbox[i],
                timeout = 10000) %>%
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf()
    dat2 <- dat1[["osm_polygons"]] %>%
      st_filter(shapes$geometry[i])
    if(nrow(dat2) == 0){next}
    dat2 <- dat2%>%
      dplyr::select(osm_id, name, geometry) %>%
      as_data_frame()

    data <- data %>%
      rbind(dat2)
  }
  return(data)
}