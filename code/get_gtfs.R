# GTFS for a given region

# devtools::install_github("U-Shift/GTFShift", force = TRUE)
library(GTFShift)
library(sf)
library(dplyr)


aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
lisboa = aml |> dplyr::filter(Concelho == "Lisboa") |> sf::st_bbox()


data_sources = GTFShift::query_mobilitydatabase(refresh_token = Sys.getenv("MOBILITY_DATABASE_REFRESH"),
                       # bounding_filter_method = "partially_enclosed",
                       # bbox = lisboa
                       # country_code = "PT",
                       subdivision_name = "Lisbon" # better results than "Lisboa"
                       # is_official = TRUE
                      )
data_sources = data_sources |> filter(status == "active")
# 7 results

# I want to keep Carris and Carris Metropolitana

gtfs_carris = load_feed(data_sources$producer_url[1])
gtfs_carris_metropolitana = load_feed(data_sources$producer_url[2])

# Filter by the next Wednesday working day 
next_wednesday = calendar_nextBusinessWednesday(country_code="PT")
gtfs_carris = tidytransit::filter_feed_by_date(gtfs_carris, extract_date = next_wednesday)
gtfs_carris_metropolitana = tidytransit::filter_feed_by_date(gtfs_carris_metropolitana, extract_date = next_wednesday)



# Filter by mode (on-street only)
# 0 = Tram, Streetcar, Light rail. 
# 3 = Bus. Used for short- and long-distance bus routes.

gtfs_carris = gtfs_carris |>
  filter_by_modes(modes = list(0, 3))  # filter by mode = tram and bus
gtfs_carris_metropolitana = gtfs_carris_metropolitana |>
  filter_by_modes(modes = list(0, 3))  # filter by mode = tram and bus

# it is recommended to filter by date before merging, otherwise it is very time consuming

# Merge both gtfs
gtfs_carris_metropolitana$calendar = create_calendar(gtfs_carris_metropolitana) # otherwise next step produces error
gtfs_bus_lisbon = unify(list(gtfs_carris, gtfs_carris_metropolitana), 
                        create_transfers = FALSE) # maybe not necessary for the paper!
gtfs_bus_lisbon = tidytransit::as_tidygtfs(gtfs_bus_lisbon) # necessary?


# For each route, the number of departures aggregated per hour and overline
routes_freq_lisbon_hour = GTFShift::get_route_frequency_hourly(gtfs = gtfs_bus_lisbon,
                                                               date = next_wednesday,
                                                               overline = TRUE)

routes_freq_lisbon_hour_clip = routes_freq_lisbon_hour |> st_crop(lisboa) # clip to Lisbon bounding box

routes_freq_lisbon_hour_8 = routes_freq_lisbon_hour |> filter(hour == 8)
max(routes_freq_lisbon_hour_8$frequency, na.rm = TRUE) # 115
routes_freq_lisbon_hour_9 = routes_freq_lisbon_hour |> filter(hour == 9)
max(routes_freq_lisbon_hour_9$frequency, na.rm = TRUE) # 102
routes_freq_lisbon_hour_clip_8 = routes_freq_lisbon_hour_clip |> filter(hour == 8)


mapview::mapview(routes_freq_lisbon_hour_8 |> filter(frequency > 3),
                 zcol = "frequency", 
                 lwd = "frequency",
            layer.name = "Routes frequency")

mapview::mapview(routes_freq_lisbon_hour_clip_8 |> filter(frequency > 3),
                 zcol = "frequency", 
                 lwd = "frequency",
                 layer.name = "Routes frequency")


# # Save GTFS with all carris and metropolitana (not filtered)
# tidytransit::write_gtfs(gtfs_bus_lisbon, "data/gtfs/gtfs_bus_lisbon.zip")
# piggyback::pb_upload("data/gtfs/gtfs_bus_lisbon.zip")

# Save GTFS with filtered no transfers date carris and metropolitana
# tidytransit::write_gtfs(gtfs_bus_lisbon, "data/gtfs/gtfs_bus_lisbon_filtered.zip")
# piggyback::pb_upload("data/gtfs/gtfs_bus_lisbon_filtered.zip")




# with other funcion using rnet_join --------------------------------------

road_osm_simple = road_osm |>
  dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                               'secondary', "trunk", 'trunk_link',
                               "tertiary",  "service", # otherwise the bus lanes won't show
                               "residential", "unclassified"))

routes_freq_lisbon_hour_no_overline = GTFShift::get_route_frequency_hourly(gtfs = gtfs_bus_lisbon,
                                                               date = next_wednesday,
                                                               overline = FALSE)
routes_freq_lisbon_hour_no_overline_8 = routes_freq_lisbon_hour_no_overline |> filter(hour == 8)
routes_freq_lisbon_hour_osm_overline_8 = GTFShift::network_overline(
  road_osm_simple,
  routes_freq_lisbon_hour_no_overline_8,
  attr = "frequency",
  network_segment_length = 100,
  fun = sum,
  join_dist = 8
)
mapview::mapview(routes_freq_lisbon_hour_osm_overline_8 |> filter(frequency > 2),
                 zcol = "frequency", 
                 lwd = "frequency",
                 layer.name = "Routes frequency OSM overline")



# not good results
# routes_freq_lisbon_hour_osm_overline_8_filtered = routes_freq_lisbon_hour_osm_overline_8 |> 
#   mutate(length = sf::st_length(geometry)) |> units::drop_units() |>
#   filter(highway != "residential" & length >15)
# mapview::mapview(routes_freq_lisbon_hour_osm_overline_8_filtered |> filter(frequency > 0),
#                  zcol = "frequency", 
#                  lwd = "frequency",
#                  layer.name = "Routes frequency OSM overline filtered")
