# fassf

! please create an issue if you encounter an error of ANY (!) kind, since I don't have the time to stress test everything; use at own risk!

installation: ``devtools::install_github("noahdasanaike/fassf/fassf")``

An R package with various improved and original functions for spatial analysis in R, as well as a repository for basic spatial data. Mostly wrappers that improve/add functionality. Greatly improves speed and efficiency of various spatial operations.

Examples:
- obtain all respective values from a global 1km-level tiff file for 40,000 sf polygons in 8 minutes (w/o exceeding 1gb of memory usage)
- calculate distance to nearest of 3,200 sf polygons for each of 32,000 sf polygons in 3 minutes
- obtain average outcome values for 100 sf polygons across 18,000 intersecting sf polygons in <1 second

Current functionality:
* faster averages (or measure functions) from spatial intersections
    * ``intersection_fun(need_df, have_df, "var", fun = "mean")`` or ``intersection_mean(want_df, have_df, "var")``
- obtain values from raster for polygons
    * ``need_raster_means_sf <- raster_polygon_values("values.tif", need_raster_means_sf, fun = "mean")``
* faster match each x with a y polygon
    * ``need_labels_df$labels <- intersection_match(need_labels_df, labels_df, "labels")``
- faster st distance
    * ``need_distance$distance <- faster_st_distance(need_distance_df, have_distance_sf)
- add latitude and longitude columns
    * ``need_coords_sf <- add_lat_lon(need_coords_sf)``
- ruggedness calculations (median absolute deviation)
    * ``need_ruggedness_sf$ruggedness <- ruggedness_mad(need_ruggedness_sf)
- small area nightlight estimates
    * ``need_nightlight_sf <- nightlight_estimates(1992:2021, need_nightlight_sf)
- generate voronoi polygons for coordinates bounded by a polygon
    * ``need_voronoi_sf$geometry <- better_voronoi_polygons(need_voronoi_sf, bounding_polygon_sf)``
- geocode query with geocode.xyz and openstreetmap
    * ``addresses <- osm_query_sf(c("1737 Cambridge St, Cambridge, MA", "20 Bradston St, Boston, MA"))
* faster st filter (see below)
- query openstreetmap, get polygons
- drive time calculation between points with openstreetmap
- split polygon by line
- rotate polygon

Examples:

- Obtain all "highway:residential" values from OpenStreetMap, drop values with missing names, and save to "roads_usa.RDS" by census-designated place geometries, adding a column for the name of the latter: 

```
osm_data_sf(usa_localities,
            shape_names = usa_localities$placens,
            quiet = FALSE,
            key = "highway",
            value = "residential",
            drop_na = "name",
            save = "road_names/roads_usa.RDS")
```
