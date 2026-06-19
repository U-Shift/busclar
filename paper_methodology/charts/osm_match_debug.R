gtfs_original_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_cascais_20260507.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_lisboa_20260519.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_lisboa_20260519.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_AML_2026-05-27_manipulated.zip"
match_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_cascais_gtfs2026-05-27_run20260619.gpkg"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260618.gpkg" 
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs20260519_run20260519.gpkg"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_lisboa_gtfs20260519_run20260519.gpkg"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260615.gpkg"

gtfs_original <- tidytransit::read_gtfs(gtfs_original_url)
summary(gtfs_original)
gtfs_original_shapes <- tidytransit::shapes_as_sf(gtfs_original$shapes)

match <- sf::read_sf(match_url)
# FIlter empty geometries
nrow(match)
match_filter_empty <- match |> filter(sf::st_is_empty(geom))
nrow(match_filter_empty)
match_filter_empty

View(match |> sf::st_drop_geometry())

library(mapview)

shape_debug = "olum"
  # MobiCascais "3i4g"
  # Carris "64_0_DESC_shp" "108_3_ASC_shp"
  # Carris Metropolitana: "shp_4516_0_2" # "shp_4512_0_2"  "3120_0_2" "2805_0_1" "412"
gtfs_original_shapes |> filter(shape_id == shape_debug)
match |> filter(shape_id == shape_debug)
(match |> filter(shape_id == shape_debug))$geom

mapview(gtfs_original_shapes |> filter(shape_id == shape_debug), layer.name="GTFS shape", color="blue") +
  mapview(match |> filter(shape_id == shape_debug), layer.name="OSM route relation", color="red")

mapview(
  gtfs_original$shapes |> 
    filter(shape_id == shape_debug) |>
    sf::st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326, remove = FALSE),
  zcol="shape_pt_sequence"
)
