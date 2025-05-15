# overline_sigle
# 
# do the rnet_join for sigle service_ids
# and then the overline


library(dplyr)
library(sf)
library(stplanr)
library(mapview)



# single tests ------------------------------------------------------------


# OSM data filtered
road_osm = opq(BBOX) |> # uses osmdata package, to extract only with BB
  add_osm_feature(key = "highway") |> 
  osmdata_sf() |> 
  osm_poly2line() # makes roundabouts into lines

road_osm = road_osm$osm_lines |>
  select(osm_id, name, highway, geometry)

road_osm = road_osm |>
  dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                               'secondary',"secondary_link", "trunk", 'trunk_link',
                               "tertiary", "tertiary_link", 
                               "residential", "living_street", "unclassified", "service"))

# GTFS shapes by service_id
shape_i = shapes[1,] |> st_transform(3857)

# test with rnet_join

osm_shape_i = rnet_join(rnet_x = shape_i,
                        rnet_y = road_osm |> select(osm_id) |> st_transform(3857),
                             length_y = FALSE,
                        # dist_subset = 10,
                             # contains = FALSE, # error
                             # max_angle_diff = 80,
                             # dist_subset = 3,
                             # subset_x = TRUE, # muito lento
                             key_column = "shape_id",
                             dist = 7
)

mapview(osm_shape_i) + mapview(shape_i, color = "red")

osm_shape_i_line = road_osm |> 
  filter(osm_id %in% osm_shape_i$osm_id) 

mapview(osm_shape_i) + mapview(shape_i, color = "red") + mapview(osm_shape_i_line, color = "green")

osm_shape_i_line_correct = osm_shape_i_line |>
  group_by(name) |>
  summarise(geometry = st_union(geometry)) |>
  mutate(length = st_length(geometry) |> as.numeric()) |>
  filter(length > 10) |>   # remove small lines |
  st_combine() |> 
  st_sf()



  st_cast("MULTILINESTRING") |> 
  st_line_merge(directed = TRUE)
mapview(osm_shape_i_line_correct)

mapview(osm_shape_i_line_correct) + mapview(shape_i, color = "red") + mapview(osm_shape_i_line, color = "green")




# for all carris routes ---------------------------------------------------


## loop for all
shapes_osm = shapes[1,]

for (i in 1:nrow(shapes)) {
  shape_i = shapes[i,] |> st_transform(3857)
  
  osm_shape_i = rnet_join(rnet_x = shape_i,
                          rnet_y = road_osm |> select(osm_id) |> st_transform(3857),
                          length_y = FALSE,
                          # dist_subset = 10,
                          # contains = FALSE, # error
                          # max_angle_diff = 80,
                          # dist_subset = 3,
                          # subset_x = TRUE, # muito lento
                          key_column = "shape_id",
                          dist = 7
  )
  
  osm_shape_i_line = road_osm |> 
    filter(osm_id %in% osm_shape_i$osm_id) 
  
  if(nrow(osm_shape_i_line) == 0) next # not run for lifts
  
  osm_shape_i_line_correct = osm_shape_i_line |>
    group_by(name) |>
    summarise(geometry = st_union(geometry)) |>
    mutate(length = st_length(geometry) |> as.numeric()) |>
    filter(length > 10) |>   # remove small lines |
    st_cast("MULTILINESTRING") |>
    st_line_merge() |>
    summarise()
  
  shapes_osm_i = osm_shape_i_line_correct  |> 
    st_transform(4326) |> 
    mutate(shape_id = shapes$shape_id[i])
  
  shapes_osm = rbind(shapes_osm, shapes_osm_i)

}
shapes_osm = shapes_osm[-1,]

# mapview(shapes_osm[200,])


routes_freq =
  freq_data %>%
  left_join(trips %>%
              select(route_id, direction_id, shape_id) %>%
              distinct()) %>%
  as.data.frame() |> 
  # filter(!shape_id %in% shapes_osm$shape_id) |>  
  right_join(shapes_osm, by = "shape_id") %>% # left_join does not work, but right does!
  st_as_sf() |> 
  st_cast("MULTILINESTRING") |>
  st_cast("LINESTRING") 

mapview(routes_freq[1,])


## overline
routes_freq_all_osm = data.frame()
for (h in 0:23) { # hours of the day
  routes_freq_h = routes_freq %>% 
    filter(arrival_hour == h) %>% 
    overline2(attrib = "freq") %>% 
    arrange(freq) %>% 
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
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)





# from qgis ---------------------------------------------------------------

shapes_osm_simplifiessnap = st_read("data/carris_routes_snapped6_simplified5_v2.gpkg")

routes_freq_simplify = freq_data %>%
  left_join(trips %>%
              select(route_id, direction_id, shape_id) %>%
              distinct()) %>%
  as.data.frame()  |>
  left_join(shapes_osm_simplifiessnap) |> 
  st_as_sf()

## overline
routes_freq_simplify_all = data.frame()
for (h in 0:23) { # hours of the day
  routes_freq_h = routes_freq_simplify %>% 
    filter(arrival_hour == h) %>% 
    overline2(attrib = "freq") %>% 
    arrange(freq) %>% 
    mutate(hour = h)
  
  routes_freq_simplify_all = rbind(routes_freq_simplify_all, routes_freq_h)
}


# for a given hour
h = 8 # test
routes_freq_simplify_hour = routes_freq_simplify_all %>% 
  filter(hour == h) 
summary(routes_freq_simplify_hour$freq)


## mapas
# with all
mapview(
  routes_freq_simplify_hour,
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)

mapview(
  routes_freq_simplify_hour %>% filter(freq > 2),
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequency (hour)",
  lwd.multiplier = 200 # acho que não faz nada
)
