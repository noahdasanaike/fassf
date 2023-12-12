#' @export

intersection_fun <- function(data, intersecting_data, 
                              var_mean,
                              quiet = FALSE,
                              intermediary = FALSE,
                              grid_cells = NA, # replace with default for package 
                              replace_missing = 0,
                              fun = NULL){
  require(geos)
  
  if(intermediary == TRUE){
    if(is.na(grid_cells)){
      if(quiet == FALSE){print("missing grid cells, using default")}
      grid_cells <- readRDS("objects/grid_cells.RDS")
    }
    if(st_crs(data) != st_crs(intersecting_data) & st_crs(data) != "EPSG:3857"){
      if(quiet == FALSE){print("correcting CRS")}
      
      data <- data %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
      intersecting_data <- intersecting_data %>% st_as_sf() %>% st_transform(crs = "EPSG:3857")
    }
    
    if(quiet == FALSE){print("constructing grid cell matrix")}
    
    ### filter grid cells AND intersecting data 
    
    intersected_object <- geos_intersects_matrix(grid_cells,data)
    grid_cells <- grid_cells[which(lengths(intersected_object) > 0),]
    
    intersected_object <- geos_intersects_matrix(intersecting_data,data)
    intersecting_data <- intersecting_data[which(lengths(intersected_object) > 0),]
    
    if(quiet == FALSE){print("obtaining grid cell means")}
    
    grid_cell_matrix <- geos_intersects_matrix(grid_cells,
                                               intersecting_data)
    
    for(i in 1:length(grid_cell_matrix)){
      if(quiet == FALSE){cat("\r", i / length(grid_cell_matrix))}
      grid_cells[[paste(var_mean)]][i] <- lapply(list(na.omit(intersecting_data[[paste(var_mean)]][grid_cell_matrix[i][[1]]])), get(fun))
    }
    grid_cells[[paste(var_mean)]][is.na(grid_cells[[paste(var_mean)]])] <- replace_missing
    
    if(quiet == FALSE){print("obtaining original data means")}
    
    intersection_matrix <- geos_intersects_matrix(data, grid_cells)
    
    return_obj <- rep(NA, times = length(intersection_matrix))
    
    for(i in 1:length(intersection_matrix)){
      if(quiet == FALSE){cat("\r", i / length(intersection_matrix))}
      return_obj[i] <- lapply(list(na.omit(grid_cells[[paste(var_mean)]][intersection_matrix[i][[1]]])), get(fun))
    }
    return_obj[is.na(return_obj)] <- replace_missing
    return(return_obj)
  }else{
    if(st_crs(data) != st_crs(intersecting_data)){
      print("correcting CRS")
      intersection_matrix <- geos_intersects_matrix(data %>%
                                                      st_transform(crs = "EPSG:3857"), 
                                                    intersecting_data %>%
                                                      st_transform(crs = "EPSG:3857"))
    }else{
      intersection_matrix <- geos_intersects_matrix(data, 
                                                    intersecting_data)
    }
    if(quiet == FALSE){print("obtaining original data means")}
    
    return_obj <- rep(NA, times = length(intersection_matrix))
    
    for(i in 1:length(intersection_matrix)){
      if(quiet == FALSE){cat("\r", i / length(intersection_matrix))}
      return_obj[i] <- lapply(list(na.omit(intersecting_data[[paste(var_mean)]][intersection_matrix[i][[1]]])), get(fun))
    }
    return_obj[is.na(return_obj)] <- replace_missing
    return(return_obj)
  }
}
