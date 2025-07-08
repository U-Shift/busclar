library(GTFShift)
library(dplyr)
library(sf)
library(mapview)

## load 2 gtfs
gtfs_carris1 = load_feed("data/gtfs/mdb-1032-202505170214.zip")
gtfs_carris2 = load_feed("data/gtfs/mdb-2027-202505160213.zip")
# gtfs_carris2_oficial = load_feed("https://api.carrismetropolitana.pt/gtfs")

# filter by date
gtfs_carris2$calendar = create_calendar(gtfs_carris2)
date = "2025-05-21"
gtfs_carris1_filtered = tidytransit::filter_feed_by_date(gtfs_carris1, date)
gtfs_carris2_filtered = tidytransit::filter_feed_by_date(gtfs_carris2, date)
gtfs_carris2_filtered$shapes = gtfs_carris2_filtered$shapes |> # remove last 6 digits of shape id of CM
  mutate(shape_id = substring(shape_id, 1, nchar(shape_id)-6))
gtfs_carris2_filtered$trips = gtfs_carris2_filtered$trips |>
  mutate(shape_id = substring(shape_id, 1, nchar(shape_id)-6))

# unify feeds
gtfs_carris_merged = unify(gtfs_carris1_filtered, gtfs_carris2_filtered, create_transfers = FALSE)
# tidytransit::write_gtfs(gtfs_carris_merged, "data/gtfs/gtfs_carriseCM_merged.zip")

# get hourly frequency by shape
frequencies_route = GTFShift::get_route_frequency_hourly(gtfs_carris_merged, date = date)

# remove the prefix .x and .y from the variable shape_id
frequencies_route = frequencies_route |> 
  mutate(shape_id = gsub("\\.x|\\.y", "", shape_id))

# # Build OSM query
# library(osmdata)
# q = opq("Lisbon")  |>
#   add_osm_feature(key = "route", value = c("bus", "tram")) |>
#   add_osm_feature(key = "network", value = c("Carris", "Carris Metropolitana"), key_exact = TRUE)
# 
# shapes_geometry_osm = GTFShift::osm_shapes_to_routes(gtfs_bus_lisbon, q)

# match new osm shapes instead of gtfs
shapes_geometry_osm_cm = st_read("data/shapes_match_aml.gpkg") # são MULTILIESTRING!!
shapes_geometry_osm_cm = shapes_geometry_osm_cm |> 
  select(shape_id, route_short_name, route_long_name, osm_id, geom)
shapes_geometry_osm_cm = shapes_geometry_osm_cm |> 
  mutate(shape_id = substring(shape_id, 1, nchar(shape_id)-6)) # remove the laste 6 digits
shapes_geometry_osm_carris = st_read("data/shapes_match_carris_frequency_hourly.gpkg") 
shapes_geometry_osm_carris = shapes_geometry_osm_carris |> 
  select(shape_id, route_short_name.x, route_long_name, osm_id, geom) |> 
  rename(route_short_name = route_short_name.x) |> 
  distinct()
shapes_geometry_osm_all = bind_rows(shapes_geometry_osm_cm, shapes_geometry_osm_carris)


frequencies_route_osm = frequencies_route |> 
  select(shape_id, hour, frequency) |> 
  st_drop_geometry() |> 
  left_join(shapes_geometry_osm_all, by = "shape_id") |> 
  st_as_sf()
frequencies_route_osm = frequencies_route_osm |> 
  filter(!is.na(route_short_name)) # remove shapes not found in OSM






## overline
routes_freq_all_osm = data.frame()
library(stplanr)
for (h in 0:23) { # hours of the day
  routes_freq_h = frequencies_route_osm %>% 
    filter(hour == h) %>% 
    overline2(attrib = "frequency") %>% 
    arrange(frequency) %>% 
    mutate(hour = h)
  
  routes_freq_all_osm = rbind(routes_freq_all_osm, routes_freq_h)
}



# for a given hour
h = 8 # test
routes_freq_hour_osm = routes_freq_all_osm %>% 
  filter(hour == h) 

summary(routes_freq_hour_osm$freq)

## mapas
# with all
mapview(
  routes_freq_hour_osm,
  zcol = "frequency",
  lwd = "frequency",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)


st_write(routes_freq_all_osm, "data/carris_cm_frequency_osm.gpkg")
piggyback::pb_upload("data/carris_cm_frequency_osm_052025.gpkg")
