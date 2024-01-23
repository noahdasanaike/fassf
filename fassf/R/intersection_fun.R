#' @export

intersection_fun <- function(data, intersecting_data,
                             var,
                             quiet = FALSE,
                             replace_missing = NA,
                             fun = NULL){
  require(geos)

  if(st_crs(data) != st_crs(intersecting_data)){
    print("correcting CRS")
    intersection_matrix <- geos_intersects_matrix(data %>%
                                                    st_transform(crs = "EPSG:3857"),
                                                  intersecting_data %>%
                                                    st_transform(crs = "EPSG:3857"))
  }else{
    intersection_matrix <- geos_intersects_matrix(data, intersecting_data)
  }
  if(quiet == FALSE){print("obtaining original data means")}

  if(is.null(fun)){
    return_obj <- c()
  }else{
    return_obj <- rep(NA, times = length(intersection_matrix))
  }

  for(i in 1:length(intersection_matrix)){
    if(quiet == FALSE){cat("\r", i / length(intersection_matrix))}
    if(is.null(fun)){
      return_obj <- c(return_obj, list(na.omit(intersecting_data[[paste(var)]][intersection_matrix[i][[1]]])))
    }else{
      return_obj[i] <- unlist(lapply(list(na.omit(intersecting_data[[paste(var)]][intersection_matrix[i][[1]]])), get(fun)))
    }
  }

  if(length(which(is.na(return_obj))) > 0){
    if(is.null(fun)){
      return_obj[[which(is.na(return_obj))]] <- replace_missing
    }else{
      return_obj[which(is.na(return_obj))] <- replace_missing
    }
  }
  return(return_obj)
}
