library(dplyr)

gtfs_original_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_lisboa_20260519.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_AML_2026-05-27_manipulated.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_cascais_20260507.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_lisboa_20260519.zip"
  # OLD VERSION "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_AML_20260506.zip"
match_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs20260527_run20260626.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_cascais_gtfs20260527_run20260626.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs20260527_run20260626.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs20260527_run20260626.csv"
  # Versions before CRS fix
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs2026-05-27_run20260619.csv" 
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs2026-05-27_run20260619.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_cascais_gtfs2026-05-27_run20260619.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260619.csv"
  # Versions previous to first/last stop fix
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs2026-05-27_run20260618.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs2026-05-27_run20260618.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_cascais_gtfs2026-05-27_run20260618.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260618.csv"
  # Versions previous to stops heuristic fix
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_cascais_gtfs20260507_run20260507.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs20260518_run20260518.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs20260519_run20260519.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260615.csv"
  # OLD VERSION "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs20260506_run20260506.csv"


gtfs_original <- tidytransit::read_gtfs(gtfs_original_url)
summary(gtfs_original)

# CARRIS ONLY
# Filter tram routes (route_short_name contains "E")
routes_bus <- gtfs_original$routes |>
  filter(!stringr::str_detect(route_short_name, "E"))
trips_routes_bus <- gtfs_original$trips |>
  filter(route_id %in% routes_bus$route_id)
gtfs_original <- tidytransit::filter_feed_by_trips(gtfs_original, trips_routes_bus$trip_id)

summary(gtfs_original)
length(unique(gtfs_original$shapes$shape_id))

# Filter for working day 
gtfs_original <- tidytransit::filter_feed_by_date(gtfs_original, extract_date = "2026-05-27")
summary(gtfs_original)
length(unique(gtfs_original$shapes$shape_id))
length(unique(gtfs_original$trips$trip_id))

gtfs_original_shapes <- tidytransit::shapes_as_sf(gtfs_original$shapes) |>
  sf::st_transform(3763) |> # To get meters (for PT CRS)
  dplyr::mutate(length_m = sf::st_length(geometry)) |>
  dplyr::mutate(length_km = as.numeric(length_m) / 1000)
mean(gtfs_original_shapes$length_km)
sd(gtfs_original_shapes$length_km)

gtfs_original_stops_per_trip <- gtfs_original$trips |>
  left_join(gtfs_original$stop_times , by="trip_id") |> 
  group_by(trip_id) |> summarise(n_stops = n())
mean(gtfs_original_stops_per_trip$n_stops)
sd(gtfs_original_stops_per_trip$n_stops)

match <- read.csv(match_url) |> filter(shape_id %in% gtfs_original$shapes$shape_id)
nrow(match)
length(unique(match$shape_id))
length(unique(match$shape_id)) / length(unique(gtfs_original$shapes$shape_id)) * 100
# length(unique(match$route_id)) / length(unique(gtfs_original$routes$route_id)) * 100
# length(unique(match$trip_id)) / length(unique(gtfs_original$trips$trip_id)) * 100

shapes_day_match = gtfs_original$shapes |>
  distinct(shape_id) |>
  left_join(match |> select(shape_id, osm_id), by="shape_id")
shapes_day_match
nrow(shapes_day_match)
nrow(shapes_day_match|>filter(!is.na(osm_id)))
nrow(shapes_day_match|>filter(!is.na(osm_id))) / nrow(shapes_day_match) * 100

trips_day_match <- gtfs_original$trips |> left_join(shapes_day_match, by="shape_id")
nrow(trips_day_match)
nrow(trips_day_match |> filter(!is.na(osm_id)))
nrow(trips_day_match |> filter(!is.na(osm_id))) / nrow(trips_day_match) * 100

# Get mean and SD for distance_diff, points_diff and stops_diff dispersion in whole dataset
mean(match$distance_diff)
mean(match$points_diff)
mean(match$stops_diff)
# Get SD (summary doesn't provide)
sd(match$distance_diff)
sd(match$points_diff)
sd(match$stops_diff)

# Get data for reference day, at peak
gtfs_day_peak <- tidytransit::filter_feed_by_date(gtfs_original, extract_date = "2026-05-27", min_departure_time = "08:00:00", max_arrival_time = "09:00:00")
summary(gtfs_day_peak)
length(unique(gtfs_day_peak$shapes$shape_id))
length(unique(gtfs_day_peak$trips$trip_id))

shapes_day_peak_match = gtfs_day_peak$shapes |>
  distinct(shape_id) |>
  left_join(match |> select(shape_id, osm_id), by="shape_id")
shapes_day_peak_match
nrow(shapes_day_peak_match)
nrow(shapes_day_peak_match|>filter(!is.na(osm_id)))
nrow(shapes_day_peak_match|>filter(!is.na(osm_id))) / nrow(shapes_day_peak_match) * 100

trips_day_peak_match <- gtfs_day_peak$trips |> left_join(shapes_day_peak_match, by="shape_id")
nrow(trips_day_peak_match)
nrow(trips_day_peak_match |> filter(!is.na(osm_id)))
nrow(trips_day_peak_match |> filter(!is.na(osm_id))) / nrow(trips_day_peak_match) * 100

# Match <1000m length and <500m start/end points
match_filter <- match |> filter(distance_diff<1000 & points_diff<500)
nrow(match_filter)

shapes_day_peak_match_filter = gtfs_day_peak$shapes |>
  distinct(shape_id) |>
  left_join(match_filter |> select(shape_id, osm_id), by="shape_id")
shapes_day_peak_match_filter
nrow(shapes_day_peak_match_filter)
nrow(shapes_day_peak_match_filter|>filter(!is.na(osm_id)))
nrow(shapes_day_peak_match_filter|>filter(!is.na(osm_id))) / nrow(shapes_day_peak_match_filter) * 100

trips_day_peak_match_filter <- gtfs_day_peak$trips |> left_join(shapes_day_peak_match_filter, by="shape_id")
nrow(trips_day_peak_match_filter)
nrow(trips_day_peak_match_filter |> filter(!is.na(osm_id)))
nrow(trips_day_peak_match_filter |> filter(!is.na(osm_id))) / nrow(trips_day_peak_match_filter) * 100
