faster_st_filter <- function(original, intersecting){
  require(geos)
  return(x[which(lengths(geos_intersects_matrix(original$geometry, 
                                                intersecting$geometry)) > 0),])
}