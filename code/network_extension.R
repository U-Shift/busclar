# network extension
library(GTFShift)
library(dplyr)
library(sf)
library(mapview)

network = gtfs_carris1
network = network |> get_route_frequency_hourly(date = "2025-05-21")
shapes_unique = network |>
  select(shape_id) |>
  distinct()
network_redux = network |>
  st_drop_geometry() |> 
  group_by(route_short_name, route_id, direction_id, shape_id) |> 
  summarise(frequency_day = sum(frequency)) |> 
  ungroup()
network_redux_max = network_redux |>
  group_by(route_short_name, direction_id) |>
  summarise(frequency_max = max(frequency_day)) |> 
  ungroup()
network_redux_shapes = network_redux_max |> 
  left_join(network_redux |> select(-route_id),
            by = c("route_short_name", "direction_id", "frequency_max" = "frequency_day")) |> 
  select(-frequency_max) |> 
  left_join(shapes_unique, by = "shape_id") |>
  st_as_sf() |> 
  mutate(length = st_length(geometry))

sum(network_redux_shapes$length) #1.937 km

mapview(network_redux_shapes)

## discard direction
network_redux_shapes_simple = network_redux_shapes |> 
  filter(direction_id == 0) # incudes the circular ones

sum(network_redux_shapes_simple$length) #1.066 km



## all variants
shapes_all = network |>
  select(shape_id) |>
  mutate(length = st_length(geometry))

sum(shapes_all$length) #32.272 km
