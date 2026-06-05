library(dplyr)

gtfs_original_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_AML_20260506.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_cascais_20260507.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_lisboa_20260519.zip"
match_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs20260518_run20260518.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs20260506_run20260506.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_cascais_gtfs20260507_run20260507.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs20260518_run20260518.csv"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs20260519_run20260519.csv"

gtfs_original <- tidytransit::read_gtfs(gtfs_original_url)
summary(gtfs_original)

# CARRIS ONLY
# Filter tram routes (route_short_name contains "E")
routes_bus <- gtfs_original$routes |>
  filter(!stringr::str_detect(route_short_name, "E"))
trips_routes_bus <- gtfs_original$trips |>
  filter(route_id %in% routes_bus$route_id)
gtfs_original <- tidytransit::filter_feed_by_trips(gtfs_original, trips_routes_bus$trip_id)

# CARRIS METROPOLITANA ONLY
# Rename all shapes that start with [.*], remove that part
gtfs_original$shapes$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs_original$shapes$shape_id)
gtfs_original$trips$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs_original$trips$shape_id)

summary(gtfs_original)
length(unique(gtfs_original$shapes$shape_id))

match <- read.csv(match_url)
nrow(match)
length(unique(match$shape_id))
length(unique(match$shape_id)) / length(unique(gtfs_original$shapes$shape_id)) * 100
# length(unique(match$route_id)) / length(unique(gtfs_original$routes$route_id)) * 100
# length(unique(match$trip_id)) / length(unique(gtfs_original$trips$trip_id)) * 100

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
