# test with sequential stops

route_test = gtfs$stop_times %>%
  filter(trip_id == "11022_20250401_77_0_10") |> #teste qualquer
  arrange(stop_sequence) |> 
  select(trip_id, stop_id, stop_sequence) |> 
  left_join(gtfs$stops, by = "stop_id")

route_test_geo = route_test %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

mapview(route_test_geo)


# routing with OSM by sequential stop_id, by car
library(openrouteservice)
locations = data.frame(lng = route_test$stop_lon,
                       lat = route_test$stop_lat)

ors_route_test = openrouteservice::ors_directions(
  locations,
  profile = "driving-car",
  output = "sf",
  api_key = ors_api_key(),
) |> 
  st_as_sf()

mapview(ors_route_test$geometry) + mapview(route_test_geo)





# stop sequences by shape_id ----------------------------------------------

stop_times = gtfs$stop_times
stops = gtfs$stops
trips = gtfs$trips
routes = gtfs$routes

stop_seq <- stop_times %>% 
  left_join(trips) %>% 
  left_join(routes) %>% 
  select(shape_id, stop_sequence, stop_id) |> 
  arrange(shape_id, stop_sequence) |> 
  distinct() |> 
  left_join(stops |> select(stop_id, stop_lat, stop_lon), by = "stop_id") |> 
  select(-stop_id)

routes_list = unique(stop_seq$shape_id)



ors_routes_carris = ors_route_test |> mutate(shape_id = "test_route") |> 
  select(shape_id, geometry)
locations_carris = locations |> 
  mutate(shape_id = "test_route") |> 
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

mapview(ors_routes_carris) +
  mapview(locations_carris) +

# for (i in 1:length(routes_list)) {
for (i in 1:2) {
  route_i = stop_seq %>% 
    filter(shape_id == routes_list[i])
  
  # routing with OSM by sequential stop_id, by car
  locations_i = data.frame(lng = route_i$stop_lon,
                         lat = route_i$stop_lat)
  
  ors_route_i = openrouteservice::ors_directions(
    locations_i,
    profile = "driving-car",
    output = "sf",
    api_key = ors_api_key(),
  ) |> 
    st_as_sf()
  
  ors_route_i = ors_route_i |> 
    mutate(shape_id = routes_list[i]) |> 
    select(shape_id, geometry)
  
  ors_routes_carris = ors_routes_carris |> bind_rows(ors_route_i)
  
  locations_i = locations_i |> 
    mutate(shape_id = routes_list[i]) |> 
    st_as_sf(coords = c("lng", "lat"), crs = 4326)
  
 locations_carris = locations_carris |> bind_rows(locations_i) 
 
}

mapview(ors_routes_carris) + mapview(locations_carris)

# também não funciona
