library(osmdata)
library(sf)
library(mapview)
library(dplyr)

available_features()
available_tags("route")

carris_osm = opq("Lisbon")  |> 
  add_osm_feature(key = "route",
                  value = "bus") |> 
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

# st_write(carris_osm_lines, "data/carris_osm_lines.gpkg", delete_dsn = TRUE)


# retrieve info from gtfs

# gtfs_carris = GTFShift::load_feed(data_sources$producer_url[1])
gtfs_carris = tidytransit::read_gtfs("https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS")
next_wednesday = calendar_nextBusinessWednesday(country_code="PT")
gtfs_carris = tidytransit::filter_feed_by_date(gtfs_carris, extract_date = next_wednesday)
# gtfs_carris = gtfs_carris |> filter_by_modes(modes = list(0, 3))  # filter by mode = tram and bus
routes_freq_lisbon_hour_no_overline = GTFShift::get_route_frequency_hourly(gtfs = gtfs_carris,
                                                                           date = next_wednesday,
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
carris_osm_multilines_redux_linestrings = carris_osm_multilines_redux |> 
  rowwise() |>
  mutate( # from MUTLILINESTRING to LINESTRING
    # Apply your function to each MULTILINESTRING
    geometry = multiline_to_sorted_linestring(geometry),
    # Convert the list column to proper sf geometry
    geometry = st_as_sfc(geometry, crs = st_crs(carris_osm_carreira))
  ) |> st_set_geometry("geometry") |>
  ungroup()
carris_osm_carreira = carris_osm_multilines_redux_linestrings %>%
  filter(ref == carreira)

carris_osm_multilines_redux_linestrings_directed = carris_osm_multilines_redux |>
  st_line_merge(directed = TRUE)

carris_gtfs_carreira = routes_freq_lisbon_hour_no_overline |> 
  filter(route_short_name == carreira) |> 
  select(route_id, shape_id, route_short_name, direction_id, geometry) |>
  distinct()
