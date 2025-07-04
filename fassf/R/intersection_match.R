#' @export

intersection_match <- function(data,
                               intersecting_data,
                               id_intersecting,
                               tie_breaker = "largest",
                               force_planar = TRUE,
                               quiet = FALSE){
  require(geos)
  require(sf)
  require(pbapply)

  sf::sf_use_s2(FALSE)
  if(!id_intersecting %in% colnames(intersecting_data)){
    stop("error: provided id not in colnames of intersecting_data")
  }
  if(nrow(data) == 0){
    stop("data object has 0 rows")
  }
  if(nrow(intersecting_data) == 0){
    stop("intersecting data object has 0 rows")
  }
  if(force_planar == TRUE){
    if(st_crs(data) != st_crs(intersecting_data) & st_crs(data) != "EPSG:3857"){
      if(quiet == FALSE){print("correcting CRS, forcing planar")}
      if(!st_crs(data) == "EPSG:3857"){
        data <- data %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
      }
      if(!st_crs(intersecting_data) == st_crs(data)){
        intersecting_data <- intersecting_data %>% st_as_sf() %>% st_transform(crs = st_crs(data))
      }
    }
  }else{
    if(st_crs(data) != st_crs(intersecting_data)){
      if(quiet == FALSE){print("correcting CRS")}

      intersecting_data <- intersecting_data %>% st_as_sf() %>% st_transform(crs = st_crs(data))
    }
  }

  data <- ungroup(data); intersecting_data <- ungroup(intersecting_data)


  if(!sum(st_is_valid(data)) == nrow(data)){
    if(quiet == FALSE){print("fixing st validity, base data")}
    data <- st_make_valid(data)
  }
  if(!sum(st_is_valid(intersecting_data)) == nrow(intersecting_data)){
    if(quiet == FALSE){print("fixing st validity, intersecting data")}
    intersecting_data <- st_make_valid(intersecting_data)
  }

  if(quiet == FALSE){print("creating intersection matrix")}

  intersected_object <- geos_intersects_matrix(st_geometry(data), st_geometry(intersecting_data))

  if(quiet == FALSE){print("assigning matches")}

  match_fun <- function(i, data, intersected_object, id_intersecting){
    if(length(intersected_object[i][[1]]) > 1){
      if(tie_breaker == "largest"){
        data[[paste(id_intersecting)]][i] <- st_join(data[i, -which(names(data) == paste(id_intersecting))],
                                                     intersecting_data[intersected_object[i][[1]],],
                                                     largest = TRUE)[1,][[paste(id_intersecting)]]
      }
    }else{
      if(length(intersected_object[i][[1]]) == 0){
        return(NA)
      }else{
        return(intersecting_data[[paste(id_intersecting)]][intersected_object[i][[1]]])
      }
    }
  }

  data[[paste(id_intersecting)]] <- NA

  if(quiet == FALSE){
    data[[paste(id_intersecting)]] <- unlist(pbsapply(X = 1:length(intersected_object),
                                                      FUN = match_fun,
                                                      data = data,
                                                      intersected_object = intersected_object,
                                                      id_intersecting = id_intersecting))
  }else{
    data[[paste(id_intersecting)]] <- unlist(parSapply(X = 1:length(intersected_object),
                                                       FUN = match_fun,
                                                       data = data,
                                                       intersected_object = intersected_object,
                                                       id_intersecting = id_intersecting))
  }
  return(data[[paste(id_intersecting)]])
}
