library(osmdata)
library(sf)
library(mapview)

available_features()
available_tags("route")

carris_osm = opq(bbox = lisboa)  |> 
  add_osm_feature(key = "route",
                  value = "bus") |> 
  osmdata_sf()

mapview(carris_osm$osm_lines) ## alll

# carris_osm_lines = st_crop(carris_osm$osm_lines, lisboa) |> 
#   select_if(~!all(is.na(.)))
# mapview(carris_osm_lines) 
carris_osm_multilines = st_crop(carris_osm$osm_multilines, stplanr::geo_buffer(Lisbon_limit, dist = 500)) |> 
  filter(operator == "Carris") |> # juntar a Carris Metropolitana noutra altura
  select_if(~!all(is.na(.)))
names(carris_osm_multilines)
mapview(carris_osm_multilines, zcol = "colour") 

carris_osm_multilines_redux = carris_osm_multilines |> 
  select(osm_id, ref, from, to, via, name, roundtrip) |> 
  


carris_osm_points = st_crop(carris_osm$osm_points, lisboa)
mapview(carris_osm_points) 
carris_osm_stoppositions = carris_osm_points |> filter(public_transport == "stop_position") |> 
  select_if(~!all(is.na(.)))
mapview(carris_osm_stoppositions, zcol = "ref") 
carris_osm_platform = carris_osm_points |> filter(public_transport == "platform") |> 
  select_if(~!all(is.na(.)))
mapview(carris_osm_platform, zcol = "ref")

# st_write(carris_osm_lines, "data/carris_osm_lines.gpkg", delete_dsn = TRUE)


# test with 1 route -------------------------------------------------------

carris_osm_735 = carris_osm_multilines_redux |> 
  filter(ref == "735")
nrow(carris_osm_735) # 4
carris_osm_735 = carris_osm_735 |>
  mutate(initial = lwgeom::st_startpoint(carris_osm_735) |> 
           st_as_sf(),
         final = lwgeom::st_endpoint(carris_osm_735) |> 
           st_as_sf())
  


# let's find which one corresponds to the shape_id

gtfs_carris = load_feed(data_sources$producer_url[1])
next_wednesday = calendar_nextBusinessWednesday(country_code="PT")
gtfs_carris = tidytransit::filter_feed_by_date(gtfs_carris, extract_date = next_wednesday)
# gtfs_carris = gtfs_carris |> filter_by_modes(modes = list(0, 3))  # filter by mode = tram and bus
routes_freq_lisbon_hour_no_overline = GTFShift::get_route_frequency_hourly(gtfs = gtfs_carris,
                                                                           date = next_wednesday,
                                                                           overline = FALSE)

carris_gtfs_735 = routes_freq_lisbon_hour_no_overline |> 
  filter(route_short_name == "735")
nrow(carris_gtfs_735)

# try with the first shape
carris_gtfs_735_i = carris_gtfs_735 |> slice(1)
carris_gtfs_735_i = carris_gtfs_735_i |> 
  mutate(initial = lwgeom::st_startpoint(carris_gtfs_735_i) |> 
           st_as_sf(),
         final = lwgeom::st_endpoint(carris_gtfs_735_i) |> 
           st_as_sf())

carris_gtfs_735_i_pointinit = carris_gtfs_735_i$initial |> 
  st_as_sf()
carris_osm_735_pointinit = carris_osm_735$initial |> 
  st_as_sf()
mapview(carris_gtfs_735_i_pointinit) + mapview(carris_osm_735_pointinit, col.regions = "red")

carris_osm_735_i_pontinit = carris_osm_735_pointinit |> 
  mutate(distance = st_distance(carris_osm_735_pointinit, carris_gtfs_735_i_pointinit)[,1] |>  # in meters
           units::drop_units()) # remove units
#ver mqat!
# https://github.com/U-Shift/EITcourse/blob/fc8a3424b1c3e8ddb8c46f522d122d708875380c/distances.qmd#L96
# filter(min(distance))
# encontrar!
  


