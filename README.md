# fassf

! please create an issue if you encounter an error of ANY (!) kind, since I don't have the time to stress test everything

[note: currently not in package form, very early work in progress]

An R package with various improved and original functions for spatial analysis in R, as well as a repository for basic spatial data. Mostly wrappers that improve/add functionality.

Current functionality:
- faster averages from spatial intersections
  - (average values z from all intersecting polygons y for x): x$z <- intersection_mean(x, y, "z")
    - note: does not currently take into account variable intersecting coverage (i.e., if y[1], y[2], and y[3] all intersect with x[1], then value for x[1] will be the average across y[1], y[2], and y[3]. this is mostly redressed (for the time being) by:
  - (project y$z onto 1x1 km grids, THEN intersect grids$z with x): x$z <- intersection_mean(x, y, "z", intermediary = TRUE)
- faster st filter
- faster match each x with a y polygon
  - (where y_id is the id from y to match onto x, using largest intersecting polygon): x$id_from_y <- intersection_match(x, y, "y_id", tie_breaker = "largest")
- add latitude and longitude columns
  - shapefiles <- add_lat_lon(shapefiles)
- ruggedness calculations (median absolute deviation)
  - shapefiles$rugged <- ruggedness_mad(shapefiles$geometry)
- small area nightlight estimates (means)
  - nightlight_estimates(years = 1992:2018, harmonized_light_option = TRUE, shapefiles = grids, unit_names = "id", shapefile_dir = "nightlights/shapefiles/", light_download_dir = "nightlights/lights/", results_dir = "nightlights/results/")
    - where shapefiles is the name of an sf object, unit_names are the unique id values for each shapefile, and the _dir values are where to save various objects created 

Coming soon:
- geographic circumscription 
- centroid-based polygon distances
- physical peripherality estimates

Data:

- global 1x1km grid cells

Benchmarking:

[to-do]
