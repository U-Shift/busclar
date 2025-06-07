library(osmdata)
library(sf)
library(mapview)
library(dplyr)

available_features()
available_tags("route")

carris_osm = opq("Lisbon")  |> 
  add_osm_feature(key = "route",
                  value = c("bus", "tram")) |> # exclude "funicular"
  osmdata_sf()

mapview(carris_osm$osm_lines) ## alll

# carris_osm_lines = st_crop(carris_osm$osm_lines, lisboa) |> 
#   select_if(~!all(is.na(.)))
# mapview(carris_osm_lines) 
carris_osm_multilines = carris_osm$osm_multilines |> 
  # st_crop(stplanr::geo_buffer(Lisbon_limit, dist = 500)) |> 
  filter(operator == "Carris") |> # juntar a Carris Metropolitana noutra altura
  select_if(~!all(is.na(.)))
names(carris_osm_multilines)
mapview(carris_osm_multilines, zcol = "colour") 

carris_osm_multilines_redux = carris_osm_multilines |> 
  select(osm_id, ref, from, to, via, name, roundtrip)
  
# filter with pattern: all values include "Carris" in text
carris_osm_points = carris_osm$osm_points |>
  filter(grepl("Carris", network)) |> # Carris, Carris Metropolitana
  st_crop(st_bbox(carris_osm_multilines_redux))
mapview(carris_osm_points) 

carris_osm_stoppositions = carris_osm_points |> filter(public_transport == "stop_position") |> 
  select_if(~!all(is.na(.)))
mapview(carris_osm_stoppositions, zcol = "ref") 
carris_osm_platform = carris_osm_points |> filter(public_transport == "platform") |> 
  select_if(~!all(is.na(.)))
mapview(carris_osm_platform, zcol = "ref")

# st_write(carris_osm_multilines_redux, "data/carris_osm_multilines_redux.gpkg", delete_dsn = TRUE)


# retrieve info from gtfs

# gtfs_carris = GTFShift::load_feed(data_sources$producer_url[1])
gtfs_carris = tidytransit::read_gtfs("https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS")
next_wednesday = calendar_nextBusinessWednesday(country_code="PT")
last_wednesday = as.Date(next_wednesday) - 7 # next_wednesday
gtfs_carris = tidytransit::filter_feed_by_date(gtfs_carris,
                                               extract_date = last_wednesday
                                               # extract_date = next_wednesday
                                               )
# gtfs_carris = gtfs_carris |> filter_by_modes(modes = list(0, 3))  # filter by mode = tram and bus
routes_freq_lisbon_hour_no_overline = GTFShift::get_route_frequency_hourly(gtfs = gtfs_carris,
                                                                           # date = next_wednesday, # affraid for santos
                                                                           date = last_wednesday,
                                                                           overline = FALSE)


# test with 1 route -------------------------------------------------------

carris_osm_735 = carris_osm_multilines_redux |> 
  filter(ref == "735")
nrow(carris_osm_735) # 4
# mapview(carris_osm_735)


carris_osm_735_geom = carris_osm_735 |> st_line_merge() # from MUTLILINESTRING to LINESTRING
carris_osm_735_geom = carris_osm_735_geom |> 
  mutate(route_dist = st_length(carris_osm_735_geom) |> units::drop_units()) |> 
  mutate(initial = lwgeom::st_startpoint(carris_osm_735_geom) |> 
           st_as_sf(),
         final = lwgeom::st_endpoint(carris_osm_735_geom) |> 
           st_as_sf()) |> 
  mutate(idseq = row_number()) |> 
  select(idseq, initial, final, route_dist, geometry) |> 
  arrange(route_dist, initial, final)


# 1. filter by ref / short_name
carris_gtfs_735 = routes_freq_lisbon_hour_no_overline |> 
  filter(route_short_name == "735") |> 
  select(route_id, shape_id, route_short_name, direction_id, geometry) |>
  distinct()
nrow(carris_gtfs_735) 


# 2. Compare distances
carris_gtfs_735 = carris_gtfs_735 |> 
  mutate(route_dist = st_length(carris_gtfs_735) |> units::drop_units()) |> 
  mutate(initial = lwgeom::st_startpoint(carris_gtfs_735) |> 
           st_as_sf(),
         final = lwgeom::st_endpoint(carris_gtfs_735) |> 
           st_as_sf()) |> 
  arrange(route_dist, initial, final)

init = st_distance(carris_osm_735_geom$initial, carris_gtfs_735$initial)
fin = st_distance(carris_osm_735_geom$final, carris_gtfs_735$final)

min(init[,1])
which.min(fin[,4]) # 1

conjunto = abs(init+fin)
carris_gtfs_735_geom_minimos = carris_gtfs_735 |> 
  mutate(osm_idseq = NA)

for (i in 1:nrow(carris_gtfs_735_geom_minimos)) {
  carris_gtfs_735_geom_minimos[i,]$osm_idseq = carris_osm_735_geom[which.min(conjunto[i,]),]$idseq
}

carris_gtfs_735_result = carris_gtfs_735_geom_minimos |>
  st_drop_geometry() |>  
  left_join(carris_osm_735_geom |> select(idseq, route_dist, geometry),
            by = c("osm_idseq" = "idseq")) |> 
  mutate(distance_diff = route_dist.x - route_dist.y)
max(carris_gtfs_735_result$distance_diff)
 
carris_gtfs_735_result = carris_gtfs_735_result |> 
  select(shape_id, direction_id, route_short_name, geometry) |> 
  st_as_sf()

mapview(carris_gtfs_735_result, zcol = "shape_id")



# mapview(carris_osm_735_geom$initial) + mapview(carris_gtfs_735$initial, col.regions = "red")
# mapview(carris_osm_735_geom$final) + mapview(carris_gtfs_735$final, col.regions = "red")





# Procedure ---------------------------------------------------------------

# 1. Filter by ref / short_name. Is it unique?
#    - If yes, then proceed to step 6.
# Is it 0? Print the message "No results found for the given ref/short_name." and continue to next iteration.
# Save info in the df with the short_name routes list.
# 
# 2. Is circular and only 1 result?
#    - If yes, then proceed to step 6.
#    - If no, then proceed to step 3.
# 
# 3. Compare the route distances and find the closest one.
#    - If only 1 result, then proceed to step 6.
# 
# 4. Compare the start point. Select the route with the minimum distance between starting points.
#   - If only 1 result, then proceed to step 6.
# 
# 5. Compare the end point. Select the route with the minimum distance between end points.
#   - If only 1 result, then proceed to step 6.
#   - If still more than 1 result, assume the first filtered one.
# 
# 6. From the unique identified result that can be assumed as the same between gtfs and osm, assign the geometry to (a new) gtfs shapes.
# Proceed with the left_joins and overlines to get the sum of frequency.




# test with 1 route and 6 variants! -------------------------------------------------------
nrow(carris_osm_multilines_redux) # 287
length(unique(carris_osm_multilines_redux$osm_id)) # 287 !

carreira = "736"
carris_osm_carreira = carris_osm_multilines_redux |> 
  filter(ref == carreira)
nrow(carris_osm_carreira) # 6
# mapview(carris_osm_carreira)


carris_osm_carreira_geom = carris_osm_carreira |> st_line_merge(directed = TRUE) # from MUTLILINESTRING to LINESTRING
# mapview(carris_osm_carreira)
carris_osm_carreira_geom = carris_osm_carreira_geom |> 
  mutate(route_dist = st_length(carris_osm_carreira_geom) |> units::drop_units()) |> 
  mutate(initial = lwgeom::st_startpoint(carris_osm_carreira_geom) |> 
           st_as_sf(),
         final = lwgeom::st_endpoint(carris_osm_carreira_geom) |> 
           st_as_sf()) |> 
  # mutate(idseq = row_number()) |> 
  select(osm_id, initial, final, route_dist, geometry) |> 
  arrange(route_dist, initial, final)


# 1. filter by ref / short_name
carris_gtfs_carreira = routes_freq_lisbon_hour_no_overline |> 
  filter(route_short_name == carreira) |> 
  select(route_id, shape_id, route_short_name, direction_id, geometry) |>
  distinct()
nrow(carris_gtfs_carreira) # 6


# 2. Compare distances
carris_gtfs_carreira = carris_gtfs_carreira |> 
  mutate(route_dist = st_length(carris_gtfs_carreira) |> units::drop_units()) |> 
  mutate(initial = lwgeom::st_startpoint(carris_gtfs_carreira) |> 
           st_as_sf(),
         final = lwgeom::st_endpoint(carris_gtfs_carreira) |> 
           st_as_sf()) |> 
  arrange(route_dist, initial, final)

## TO-DO compare first the distances (we are skipping that!!)

init = st_distance(carris_osm_carreira_geom$initial, carris_gtfs_carreira$initial)
fin = st_distance(carris_osm_carreira_geom$final, carris_gtfs_carreira$final)


conjunto = abs(init+fin)
carris_gtfs_carreira_minimos = carris_gtfs_carreira |>  # TO-DO: incorporate without _minimos
  mutate(osm_id = NA)

for (i in 1:nrow(carris_gtfs_carreira_minimos)) {
  carris_gtfs_carreira_minimos[i,]$osm_id = carris_osm_carreira_geom[which.min(conjunto[i,]),]$osm_id
}

carris_gtfs_carreira_result = carris_gtfs_carreira_minimos |>
  st_drop_geometry() |>  
  left_join(carris_osm_carreira_geom |> select(osm_id, route_dist, geometry),
            by = "osm_id") |> 
  mutate(distance_diff = abs(route_dist.x - route_dist.y)) # absolute difference
max(carris_gtfs_carreira_result$distance_diff) # 3869.512 !!! SOMETHING IS WRONG HERE

carris_gtfs_carreira_result = carris_gtfs_carreira_result |> 
  select(shape_id, direction_id, route_short_name, osm_id, geometry) |> 
  st_as_sf()

mapview(carris_gtfs_carreira_result, zcol = "shape_id")
length(unique(carris_gtfs_carreira_result$osm_id)) # 4

nrow(carris_osm_carreira) # 6
nrow(carris_gtfs_carreira) # 6
length(unique(carris_gtfs_carreira_result$osm_id)) # 4

# THEY ARE DIFFERENT UNIQUE


# after getting the osm shapes for all unique shape_id (gtfs), bring back the geometry to the
routes_freq_lisbon_hour_no_overline_OSM |>
  st_drop_geometry() |> 
  left_join(carris_gtfs_carreira_result |> 
              select(shape_id, geometry),
            by = "shape_id") |>
  st_as_sf()
# test

# test with a loop route -------------------------------------------------------
teste_loop = carris_osm_multilines_redux |> 
  filter(ref == "31B") |> # interestingly, it is not classifies as roundtrip in osm.
  # But as the result is a single line, proceed to final step
  st_line_merge() # this is relevant to end up with a LINESTRING only in the end.
mapview(teste_loop)



# test with stop names ----------------------------------------------------

carreira = "736"
# carris_osm_multilines_redux_linestrings = carris_osm_multilines_redux |> 
#   rowwise() |>
#   mutate( # from MUTLILINESTRING to LINESTRING
#     # Apply your function to each MULTILINESTRING
#     geometry = multiline_to_sorted_linestring(geometry),
#     # Convert the list column to proper sf geometry
#     geometry = st_as_sfc(geometry, crs = st_crs(carris_osm_carreira))
#   ) |> st_set_geometry("geometry") |>
#   ungroup()
carris_osm_multilines_redux_linestrings = carris_osm_multilines_redux |>
  st_line_merge(directed = FALSE)
carris_osm_carreira = carris_osm_multilines_redux_linestrings %>%
  filter(ref == carreira)

carris_osm_multilines_redux_linestrings_directed = carris_osm_multilines_redux |>
  st_line_merge(directed = TRUE)

carris_gtfs_carreira = routes_freq_lisbon_hour_no_overline |> 
  filter(route_short_name == carreira) |> 
  select(route_id, shape_id, route_short_name, direction_id, geometry) |>
  distinct()

# stop test


# only rely on distances --------------------------------------------------

carris_gtfs_shapes_winfo = routes_freq_lisbon_hour_no_overline |> 
  select(route_id, shape_id, route_short_name, direction_id) |>
  distinct()
carris_gtfs_shapes_winfo = carris_gtfs_shapes_winfo |> 
  left_join(gtfs_carris$routes |>  #get back their names (headways?)
              select(route_id, route_long_name)) |>
  mutate(shapedist_gtfs = st_length(geometry) |> units::drop_units() |> round())
nrow(carris_gtfs_shapes_winfo) # 284 (but they were more one day - May?)

carris_osm_shapes_winfo = carris_osm_multilines_redux_linestrings |> 
  mutate(shapedist_osm = st_length(geometry) |> units::drop_units() |> round())
nrow(carris_osm_shapes_winfo) # 298

# filter the ones that are in common
carris_gtfs_patterns = carris_gtfs_shapes_winfo |> 
  st_drop_geometry() |> 
  group_by(route_short_name) |>
  summarise(patterns_carris = n())

carris_osm_patterns = carris_osm_shapes_winfo |>
  st_drop_geometry() |> 
  group_by(ref) |>
  summarise(patterns_osm = n())

carris_shapes_comparison = carris_gtfs_patterns |>
  full_join(carris_osm_patterns, 
            by = c("route_short_name" = "ref")) |> 
  mutate_if(is.integer, ~replace(., is.na(.), 0)) |> 
  mutate(diff= patterns_carris - patterns_osm)

# Lets start with the 0 ones - only perfect matches
commonroutes = carris_shapes_comparison |> 
  filter(diff == 0) |> 
  select(route_short_name)

carris_gfts_shapes_winfo_common = carris_gtfs_shapes_winfo |> 
  filter(route_short_name %in% commonroutes$route_short_name) |>
  st_drop_geometry()
carris_osm_shapes_winfo_common = carris_osm_shapes_winfo |>
  filter(ref %in% commonroutes$route_short_name)


carreiras = unique(carris_gfts_shapes_winfo_common$route_short_name) # test with first carreira
carris_gtfs_osm_common0 = data.frame()
for (i in carreiras) {
  carris_osm_i = carris_osm_shapes_winfo_common |> 
    filter(ref == i) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common |> 
    filter(route_short_name == i) |> 
    arrange(shapedist_gtfs) |> 
    mutate(shapedist_osm = carris_osm_i$shapedist_osm,
           name = carris_osm_i$name,
           osm_id = carris_osm_i$osm_id) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  avgdifferences = mean(carris_gtfs_i$shape_differences)
  carris_gtfs_osm_common0 = bind_rows(carris_gtfs_osm_common0, carris_gtfs_i)
  
  print(paste0(i, ", ", avgdifferences))
}

# now the ones only in GTFS
commonroutes_1 = carris_shapes_comparison |> 
  filter(patterns_osm == 0) |> # funiculars - excluded from the osm query
  select(route_short_name)
# these are funiculars, I don't need them as they belong to a diferent catedory


# ow the ones with differences > 1 ----------------------------------------


# now the ones with differences > 1
commonroutes_2 = carris_shapes_comparison |> 
  filter(patterns_osm != 0 & diff > 0) |> 
  select(route_short_name)

carris_gfts_shapes_winfo_common2 = carris_gtfs_shapes_winfo |> 
  filter(route_short_name %in% commonroutes_2$route_short_name) |>
  st_drop_geometry()
carris_osm_shapes_winfo_common2 = carris_osm_shapes_winfo |>
  filter(ref %in% commonroutes_2$route_short_name)
# assume the full trip?
carris_gtfs_osm_common2 = carris_gfts_shapes_winfo_common2 |> 
  mutate(shapedist_osm = rep(carris_osm_shapes_winfo_common2$shapedist_osm, 3),
         name = rep(carris_osm_shapes_winfo_common2$name, 3),
         osm_id = rep(carris_osm_shapes_winfo_common2$osm_id, 3)) |> 
  mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))

# now the ones with differences < 1
commonroutes_3 = carris_shapes_comparison |> 
  filter(diff < 0) |> 
  select(route_short_name)

carris_gfts_shapes_winfo_common3 = carris_gtfs_shapes_winfo |> 
  filter(route_short_name %in% commonroutes_3$route_short_name) |>
  st_drop_geometry()
carris_osm_shapes_winfo_common3 = carris_osm_shapes_winfo |>
  filter(ref %in% commonroutes_3$route_short_name)

carreiras3 = unique(carris_gfts_shapes_winfo_common3$route_short_name) # test with first carreira
carris_gtfs_osm_common3 = data.frame()

# à pata 1
carreira3 = carreiras3[1]
carreira3 # 727
    carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(1,2,3,4,8,9,10)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)
  
# à pata 2
carreira3 = carreiras3[2]
carreira3 # 732
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(3,4)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 3  
carreira3 = carreiras3[3]
carreira3 # 768
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(1,2)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 4
carreira3 = carreiras3[4]
carreira3 # 723
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(1,2,3,5,6)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 5
carreira3 = carreiras3[5]
carreira3 # 764
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(3,4)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 6
carreira3 = carreiras3[6]
carreira3 # 760
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(4,4,5,6,7,8) # este foi difícil
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 7
carreira3 = carreiras3[7]
carreira3 # 712
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(2,3,4)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences) # este tem uma diferença muito maior pq o gtfs está adaptado às obras e o osm é o mais estável
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)
  
# à pata 8
carreira3 = carreiras3[8]
carreira3 # 776
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(2,3)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 9
carreira3 = carreiras3[9]
carreira3 # 725
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(1,2)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 10
carreira3 = carreiras3[10]  
carreira3 # 754
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(3,4)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 11
carreira3 = carreiras3[11]
carreira3 # 783
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(3,4,5,6)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 12  
carreira3 = carreiras3[12]
carreira3 # 26B
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = c(3,4)
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)

# à pata 13
carreira3 = carreiras3[13]
carreira3 # 46B
  carris_osm_i = carris_osm_shapes_winfo_common3 |> 
    filter(ref == carreira3) |>
    arrange(shapedist_osm)
  
  carris_gtfs_i = carris_gfts_shapes_winfo_common3 |> 
    filter(route_short_name == carreira3) |> 
    arrange(shapedist_gtfs)
  
  conjuntos_iguais = 2
  
  carris_gtfs_i = carris_gtfs_i |>
    mutate(shapedist_osm = carris_osm_i$shapedist_osm[conjuntos_iguais],
           name = carris_osm_i$name[conjuntos_iguais],
           osm_id = carris_osm_i$osm_id[conjuntos_iguais]) |> 
    mutate(shape_differences = abs(shapedist_gtfs - shapedist_osm))
  mean(carris_gtfs_i$shape_differences)
  
  carris_gtfs_osm_common3 = bind_rows(carris_gtfs_osm_common3, carris_gtfs_i)


# final result ------------------------------------------------------------

# combined
carris_gtfs_osm_common_all = bind_rows(carris_gtfs_osm_common0, 
                                              carris_gtfs_osm_common2,
                                              carris_gtfs_osm_common3)
length(unique(carris_gtfs_osm_common_all$route_short_name)) # 107
length(unique(carris_gtfs_osm_common_all$shape_id)) # 276

# some are linestring and some are multilinestring
# convert the ones that are multilinestring to linestring

carris_gtfs_osm_common_all_geo = carris_gtfs_osm_common_all |> 
  left_join(carris_osm_multilines_redux_linestrings |> select(osm_id, geometry)) |> 
  st_as_sf()
carris_gtfs_osm_common_all_linestrings = carris_gtfs_osm_common_all_geo |>
  filter(st_geometry_type(carris_gtfs_osm_common_all_geo) == "LINESTRING")
carris_gtfs_osm_common_all_linestrings
carris_gtfs_osm_common_all_multilinestrings = carris_gtfs_osm_common_all_geo |>
  filter(st_geometry_type(carris_gtfs_osm_common_all_geo) == "MULTILINESTRING") 
  # st_cast("LINESTRING", do_split = FALSE) # this one only keeps the first linestring
carris_gtfs_osm_common_all_multilinestrings = stplanr::line_cast(carris_gtfs_osm_common_all_multilinestrings)
# carris_gtfs_osm_common_all_multilinestrings = carris_gtfs_osm_common_all_geo |>
#   filter(st_geometry_type(carris_gtfs_osm_common_all_geo) == "MULTILINESTRING") |> 
#     rowwise() |>
#     mutate(geometry = multiline_to_sorted_linestring(geometry),
#       geometry = st_as_sfc(geometry, crs = st_crs(carris_osm_carreira))) |>
#   st_set_geometry("geometry") |>
#   ungroup()
carris_gtfs_osm_common_all_multilinestrings # for this purpose is ok, but do not use for others, as the segments are splited
mapview(carris_gtfs_osm_common_all_multilinestrings, zcol = "shape_id")
carris_gtfs_osm_common_all_geo = bind_rows(carris_gtfs_osm_common_all_linestrings, 
                                              carris_gtfs_osm_common_all_multilinestrings)

# combine with frequencies
carris_gtfs_osm_match = carris_gtfs_osm_common_all_geo |> 
  select(shape_id, osm_id) |>
  left_join(routes_freq_lisbon_hour_no_overline |> st_drop_geometry() |> 
              select(-route_id, -direction_id))


st_write(carris_gtfs_osm_match, "data/carris_gtfs_osm_match.gpkg", delete_dsn = TRUE)
piggyback::pb_upload("data/carris_gtfs_osm_match.gpkg")


# viz ---------------------------------------------------------------------

## overline
library(stplanr)
carris_gtfs_osm_match_overline = data.frame()
for (h in 0:23) { # hours of the day
  routes_freq_h = carris_gtfs_osm_match |> 
    filter(hour == h) |> 
    overline2(attrib = "frequency")  |>  
    arrange(frequency) |> 
    mutate(hour = h)
  
  carris_gtfs_osm_match_overline = rbind(carris_gtfs_osm_match_overline, routes_freq_h)
}

st_write(carris_gtfs_osm_match_overline, "data/carris_overline.gpkg", delete_dsn = TRUE)
piggyback::pb_upload("data/carris_overline.gpkg")

# for a given hour
h = 8 # test
routes_freq_simplify_hour = carris_gtfs_osm_match_overline |> 
  filter(hour == h) 
summary(routes_freq_simplify_hour$freq)
# 7h - max 94
# 8h - max 99
# 9h - max 91
# 10h - max 74
# 17h - max 88
# 18h - max 88

## mapas
# with all
mapview(
  routes_freq_simplify_hour,
  zcol = "frequency",
  lwd = "frequency",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)

mapview(
  routes_freq_simplify_hour |> filter(frequency > 6),
  zcol = "frequency",
  lwd = "frequency",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)

mapview(
  routes_freq_simplify_hour |> filter(frequency > 20),
  zcol = "frequency",
  lwd = "frequency",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)
