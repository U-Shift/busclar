# overline tests

mapview(routes_freq)

routes_742 = routes_freq %>% 
  filter(route_short_name == "742") |> 
  filter(arrival_hour == 8)

routes_706 = routes_freq %>% 
  filter(route_short_name == "706")|> 
  filter(arrival_hour == 8)

routes_730 = routes_freq %>% 
  filter(route_short_name == "730")|> 
  filter(arrival_hour == 8)

routes_718 = routes_freq %>% 
  filter(route_short_name == "718")|> 
  filter(arrival_hour == 8)

mapview::mapview(routes_742) +
  mapview::mapview(routes_730, color = "red") +
  mapview::mapview(routes_718, color = "blue") +
  mapview::mapview(routes_706, color = "green")

routes_moraissoares = rbind(routes_742, routes_730, routes_718, routes_706)
routes_moraissoares_overline = routes_moraissoares %>% 
  filter(arrival_hour == 8) |> 
  overline2(attrib = "freq") %>% 
  arrange(freq) %>% 
  mutate(hour = 8)

mapview(
  routes_moraissoares_overline,
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequência",
  lwd.multiplier = 2 
)







# só 706
routes_706_overline = routes_706 %>% 
  # st_simplify(dTolerance = 1, preserveTopology = TRUE) |> # engraçado brincar com isto, mas não resolve
  filter(arrival_hour == 8) |> 
  overline2(attrib = "freq") %>% 
  arrange(freq) %>% 
  mutate(hour = 8) |> 
  overline_intersection(attrib = "freq") # sugested here https://github.com/ropensci/stplanr/issues/420 -> does not solve!

mapview(
  routes_706_overline,
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequência",
  lwd.multiplier = 2 #
)





# match com OSM


# create unique sequential id
routes_706 = routes_706 |> 
  mutate(
    match_id = row_number()
  )


osm_706 = rnet_join(rnet_x = routes_706,
          rnet_y = road_osm |> select(osm_id),
          length_y = FALSE,
          # contains = FALSE, # error
          # max_angle_diff = 25,
          # dist_subset = 3,
          # subset_x = TRUE, # muito lento
          key_column = "match_id"
          # dist = 1
            )

mapview(
  osm_706,
  lwd.multiplier = 2 # acho que não faz nada
)


osm_706_line = road_osm |> 
  select(osm_id) |> 
  filter(osm_id %in% osm_706$osm_id) |>
  left_join(osm_706 |> st_drop_geometry()) |>
  left_join(routes_706 |>
              st_drop_geometry() |>
              select(freq, match_id)) |>
  group_by(osm_id) |> 
  summarise(freq = sum(freq))

mapview(
  osm_706_line,
  zcol = "freq",
  lwd.multiplier = 2 # acho que não faz nada
)

# não resolve. fica com "buracos"  e apanha partes que não devia.




st_write(routes_moraissoares_overline, paste0("data/", bus_operator, "_routes_moraissoares_freq.gpkg"))
st_write(routes_706, paste0("data/", bus_operator, "_routes_706.gpkg"))
st_write(routes_718, paste0("data/", bus_operator, "_routes_718.gpkg"))
st_write(routes_730, paste0("data/", bus_operator, "_routes_730.gpkg"))
st_write(routes_742, paste0("data/", bus_operator, "_routes_742.gpkg"))

# inspect in qgis


