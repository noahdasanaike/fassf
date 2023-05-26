# fassf

[note: currently not in package form]

An R package with various improved and original functions for spatial analysis in R, as well as a repository for basic spatial data. Mostly wrappers that increase functionality.

Current functionality:
- faster averages from spatial intersections
  - (average values z from all intersecting polygons y for x): x$z <- intersection_mean(x, y, "z")
    - note: does not currently take into variable intersecting coverage 
  - (project y$z onto 1x1 km grids, THEN intersect grids$z with x): x$z <- intersection_mean(x, y, "z", intermediary = TRUE)
- faster st filter
- faster match each x with a y polygon
  - (where y_id is the id from y to match onto x, using largest intersecting polygon): x$id_from_y <- intersection_match(x, y, "y_id", tie_breaker = "largest")
- add latitude and longitude columns
  - shapefiles <- add_lat_lon(shapefiles)
- ruggedness calculations (median absolute deviation)
  - shapefiles$rugged <- ruggedness_mad(shapefiles$geometry)

Coming soon:
- geographic circumscription 
- centroid-based polygon distances
- physical peripherality estimates

Data:

- global 1x1km grid cells

Benchmarking:

[to-do]
