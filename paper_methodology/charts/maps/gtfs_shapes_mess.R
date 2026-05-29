gtfs = tidytransit::read_gtfs("https://backend.tcbarreiro.pt/download-gtfs")
summary(gtfs)

library(sf)
xmin <- -9.06816
xmax <- -9.06505
ymin <- 38.65307
ymax <- 38.65451
bbox <- st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), 
                crs = st_crs(4326))
mapview::mapview(bbox)

library(dplyr)
library(osmdata)
set_overpass_url("https://overpass-api.de/api/interpreter")
get_overpass_url() # You will see the default API instance
set_overpass_url("https://overpass.private.coffee/api/interpreter") # 4 servers with 20 cores, 256GB RAM, SSD each 
road_osm <- opq(bbox) |> # uses osmdata package, to extract only with BB
  add_osm_feature(key = "highway") |>
  osmdata_sf() |>
  osm_poly2line() # makes roundabouts into lines
road_osm <- road_osm$osm_lines

road_osm_filtered = road_osm |>
  filter(highway %in% c("secondary","terciary","residential"))
table(road_osm_filtered$highway)


gtfs_bbox = tidytransit::filter_feed_by_area(gtfs, bbox)
gtfs_bbox_sf = tidytransit::shapes_as_sf(gtfs_bbox$shapes)
gtfs$agency

mapview::mapview(road_osm_filtered, layer.name="OSM road network", color="black", homebutton=FALSE) +
  mapview::mapview(gtfs_bbox_sf, zcol="shape_id", layer.name="TCBarreiro Shapes", lwd=4, homebutton=FALSE) 
