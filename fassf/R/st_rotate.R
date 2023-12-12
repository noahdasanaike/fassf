st_rotate <- function(obj, a) {
  m <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  obj <- sf::st_set_precision(obj, 1e3)
  cnt <- sf::st_centroid(sf::st_as_sfc(sf::st_bbox(obj)))
  cnt <- sf::st_set_precision(cnt, 1e3)
  objr <- (sf::st_geometry(obj) - cnt) * m
  objr <- sf::st_set_precision(objr, 1e3)
  objr <- sf::st_simplify(objr, dTolerance = 1e-3)
  objr <- objr + cnt
  sf::st_geometry(obj) <- objr
  sf::st_crs(obj) <- sf::st_crs(cnt)
  return(obj)
}