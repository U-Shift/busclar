# To reproduce the paper results
library(GTFShift)

# # load GTFS feeds from original sources
# gtfs_carris = load_feed("https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS") # 114 MB
# gtfs_carrismetro = load_feed("https://api.carrismetropolitana.pt/gtfs") # 638 MB
# # save them and upload files to releases
# tidytransit::write_gtfs(gtfs_carris, "data/gtfs/gtfs_carris_20250521.zip")
# tidytransit::write_gtfs(gtfs_carrismetro, "data/gtfs/gtfs_carrismetro_20250521.zip")
# piggyback::pb_upload("data/gtfs/gtfs_carris_20250521.zip", repo = "U-Shift/busclar", tag = "0.8")
# piggyback::pb_upload("data/gtfs/gtfs_carrismetro_20250521.zip", repo = "U-Shift/busclar", tag = "0.8")

# load the GTFS feeds from repo
gtfs_carris = load_feed("https://github.com/U-Shift/busclar/releases/download/0.8/gtfs_carris_20250521.zip") # 114 MB
gtfs_carrismetro = load_feed("https://github.com/U-Shift/busclar/releases/download/0.8/gtfs_carrismetro_20250521.zip") # 638 MB

# unify the feeds in a single GTFS feed
gtfs_lisbon = unify(gtfs_carris, gtfs_carrismetro)

# OSM query for bus networks in Lisbon
library(osmdata)
query = opq("Lisbon")  |>
  add_osm_feature(key = "route",
                  value = "bus") |>
  add_osm_feature(key = "network",
                  value = c("Carris", "Carris Metropolitana")) # operators

# get hourly frequency by shape
frequencies_segment = get_route_frequency_hourly(gtfs = gtfs_lisbon, 
                                                 date = "2025-05-21", # we used this wednesday
                                                 overline = TRUE, 
                                                 use_osm_routes = query)

# # save the result to a file and make available at repo assets
# sf::st_write(frequencies_segment, "data/carris_cm_frequency_osm_052025.gpkg", quiet = TRUE)
# piggyback::pb_upload("data/carris_cm_frequency_osm_052025.gpkg", 
#                      repo = "U-Shift/busclar",
#                      tag = "0.8",
#                      overwrite = TRUE)

# filter for the peak hour 8-9am
frequencies_segment_8 = frequencies_segment |> 
  dplyr::filter(hour == 8) # the hour with the highest frequency

# # save the light table to a file and make available at repo assets
# saveRDS(frequencies_segment_8, "data/carris_cm_frequency_osm_052025_8h.rds")
# piggyback::pb_upload("data/carris_cm_frequency_osm_052025_8h.rds", 
#                      repo = "U-Shift/busclar",
#                      tag = "0.8",
#                      overwrite = TRUE)

# show results in a map, by frequency
mapview::mapview(
  frequencies_segment_8,
  zcol = "frequency",
  lwd = "frequency",
  layer.name = "Bus frequency"
)

tail(frequencies_segment_8) # show the last rows of this result
