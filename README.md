# fassf

! please create an issue if you encounter an error of ANY (!) kind, since I don't have the time to stress test everything

[note: currently not in package form, very early work in progress]

An R package with various improved and original functions for spatial analysis in R, as well as a repository for basic spatial data. Mostly wrappers that improve/add functionality.

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
