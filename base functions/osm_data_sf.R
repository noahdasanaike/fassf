osm_data_sf <- function(shapes, key, value, additional, additional_type, 
                        filter_type = "none",
                        filter_percentage = NA,
                        quiet = TRUE,
                        save){
  suppressWarnings({
    suppressMessages({
      library(osmdata)
      data <- tibble()
      sf::sf_use_s2(FALSE)
      original_crs <- st_crs(shapes)
      
      if(!key %in% readRDS(url("https://www.dropbox.com/scl/fi/h92fu9gwuox4a7qevm724/all_keys_list.RDS?rlkey=y50tmtnpdaf4iaokvs1vzxv0l&dl=1", "rb"))){return(cat(paste0("invalid key: ", key)))}

      if(!missing(additional)){
        if(!is.data.frame(additional) | !is.tibble(additional)){
          return("please format additional arguments as data frame or tibble with key and value columns")
        }
        if(!additional_type %in% c("and", "or")){
          return("please specify additional feature condition: 'and' or 'or'")
        }
        for(b in 1:nrow(additional)){
          if(!additional$key[b] %in% available_features()){return(cat(paste0("invalid key: ", additional$key[b])))}
          
          if(!additional$value[b] %in% available_tags(additional$key[b])$Value &  !"*" %in% available_tags(additional$key[b])){
            if("(number)" %in% available_tags(additional$key[b])$Value){
              if(is.na(as.numeric(additional$value[b]))){
                return(cat(paste0("invalid value: ", additional$value[b])))
              }
            }else{return(cat(paste0("invalid value: ", additional$value[b])))}
          }
        }
      }
      shapes$geometry <- st_geometry(shapes)
      for(i in 1:nrow(shapes)){
        if(quiet == FALSE){cat("\n", paste0(round(100 * (i - 1) / nrow(shapes), 2), "%"))}
        if(quiet == FALSE){cat("\n", "checking for cross-dateline polygons")}
        split_shapes <- st_wrap_dateline(st_transform(shapes[i,], crs = "EPSG:3857")) %>% st_cast("POLYGON") %>% 
          st_transform(crs = 4326)  %>%
          st_make_valid() %>%
          rowwise() %>%
          mutate(is_east = ifelse(st_coordinates(st_centroid(geometry))[,1] > 0, 1, 0)) %>% 
          group_by(is_east) %>% 
          summarize(geometry = st_union(geometry)) %>%
          st_make_valid()
        
        if(!st_crs(split_shapes) == 4326){split_shapes <- st_transform(split_shapes, crs = 4326)}
        
        for(y in 1:nrow(split_shapes)){
          boolFalse <- FALSE
          while(boolFalse == FALSE){
            if(nrow(split_shapes) > 1){
              if(quiet == FALSE){cat("\n", paste0("trying download, split shape ", y, " of ", nrow(split_shapes)))}
            }else{
              if(quiet == FALSE){cat("\n", paste0("trying download"))}
            }
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
                    if(quiet == FALSE){cat("\n", paste0("downloading with additional feature ", f, " of ", f / nrow(additional)))}
                    dat1 <- dat1 %>%
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
                                add_osm_feature(key = additional$key[f], value = additional$value[f]) %>% 
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
          dat2 <- dat1[[paste0("osm_", values[1])]] %>% head(0)
          for(z in 1:length(values)){
            add_data <- dat1[[paste0("osm_", values[z])]]
            if(!is.null(add_data)){
              if(nrow(add_data) > 0){
                add_data <- add_data %>%
                  st_make_valid() %>%
                  st_filter(split_shapes$geometry[y]) %>% 
                  mutate(type_osm = values[z])
              }
              dat2 <- bind_rows(dat2, add_data)
            }
          }
          if(missing(additional)){
            dat2 <- dat2[dat2[[key]] == value & !is.na(dat2[[key]]),]
          }else{
            all_keys <- c(key, additional$key)
            all_values <- c(value, additional$value)
            if(!length(dat2) == 0 & !is.null(nrow(dat2))){
              for(h in 1:length(all_keys)){
                dat2[[all_keys[h]]][is.na(dat2[[all_keys[h]]])] <- FALSE
                dat3 <- dat2[dat2[[all_keys[h]]] == value,]
                if(h == 1){dat4 <- dat3}else{dat4 <- bind_rows(dat4, dat3)}
              }
              dat2 <- dat4
              if(additional_type == "or"){
                dat2 <- dat2[rowSums(is.na(st_drop_geometry(dat2[, all_keys]))) != length(all_keys),]
              }else{
                dat2 <- dat2[rowSums(is.na(st_drop_geometry(dat2[, all_keys]))) == 0,]
              }
            }
          }
          if(is.null(nrow(dat2))){
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
      if(!filter_type == "none"){
        if(!filter_type %in% values){cat("\n", "filter type not in list (polygons, points, lines, multipolygons), returning all")}else{
          data <- data %>% filter(type_osm == filter_type)
        }
      }
      if(!is.na(filter_percentage)){
        if(!filter_percentage > 0 & !filter_percentage < 100){cat("\n", "invalid filter percentage, returning all")}else{
          if(quiet == FALSE){cat("\n", paste0("filtering to intersection of ", filter_percentage, "%"))}
          data$row_number <- 1:nrow(data)
          intersect_pct <- st_intersection(st_as_sf(data), split_shapes) %>% 
            mutate(intersect_area_function = st_area(.)) %>% 
            st_drop_geometry() %>%
            group_by(row_number) %>% 
            summarize(intersect_area_function = sum(intersect_area_function))
          data <- left_join(data, intersect_pct) %>% 
            mutate(coverage_area_function = 100 * as.numeric(intersect_area_function / st_area(geometry))) %>% 
            filter(coverage_area_function >= filter_percentage)
          if(nrow(data) == 0){return(cat("invalid query: 0 polygons overlapping"))}
          data <- data %>%
            dplyr::select(-c(coverage_area_function, intersect_area_function, row_number))
        }
      }
      if(!missing(save)){saveRDS(data %>% st_as_sf() %>% st_transform(crs = original_crs),
                                 save)}
      return(data %>% st_as_sf() %>% st_transform(crs = original_crs))
    })
  })
}
