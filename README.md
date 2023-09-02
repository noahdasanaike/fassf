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

obtain North Carolina counties from OpenStreetMap, filtering out counties from neighboring states:

```
states <- tigris::states() %>% 
  st_as_sf()

state_sf <- states %>% filter(NAME == "North Carolina")

state <- osm_data_sf(shapes = state_sf, key = "admin_level", value = "6", quiet = FALSE, 
                  additional = tibble(key = "border_type", value = "county"),
                  filter_type = "multipolygons",
                  additional_type = "and",
                  filter_percentage = 80)
```

Benchmarking:

[to-do]
