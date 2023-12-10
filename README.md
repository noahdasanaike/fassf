# fassf

! please create an issue if you encounter an error of ANY (!) kind, since I don't have the time to stress test everything; use at own risk!

An R package with various improved and original functions for spatial analysis in R, as well as a repository for basic spatial data. Mostly wrappers that improve/add functionality. Greatly improves speed and efficiency of various spatial operations.

Examples:
- obtain all respective values from a global 1km-level tiff file for 40,000 sf polygons in 8 minutes (w/o exceeding 1gb of memory usage)
- calculate distance to nearest of 3,200 sf polygons for each of 32,000 sf polygons in 3 minutes
- obtain average outcome values for 100 sf polygons across 18,000 intersecting sf polygons in <1 second

Current functionality:
- faster averages from spatial intersections
- faster st filter
- faster match each x with a y polygon
- faster st distance
- add latitude and longitude columns
- ruggedness calculations (median absolute deviation)
- small area nightlight estimates
- generate voronoi polygons for coordinates bounded by a polygon
- geocode query with geocode.xyz and openstreetmap
- query openstreetmap, get polygons
- drive time calculation between points with openstreetmap
- obtain values from raster for polygons
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
