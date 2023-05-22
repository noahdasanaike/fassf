library(geos)

intersection_mean <- function(data, intersecting_data, var_mean,
                              quiet = FALSE){
  intersection_matrix <- geos_intersects_matrix(data$geometry %>%
                                                  st_transform(crs = "EPSG:3857"), 
                                                intersecting_data$geometry %>%
                                                  st_transform(crs = "EPSG:3857"))
  return_obj <- rep(1:length(intersection_matrix), NA)
  
  for(i in 1:length(intersection_matrix)){
    if(quiet == FALSE){cat("\r", i / length(intersection_matrix))}
    return_obj[i] <- mean(intersecting_data[[paste(var_mean)]][intersection_matrix[i][[1]]])
  }
  return(return_obj)
}