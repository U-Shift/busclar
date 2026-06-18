gtfs_original_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_AML_2026-05-27_manipulated.zip"
match_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260615.gpkg"

gtfs_original <- tidytransit::read_gtfs(gtfs_original_url)
summary(gtfs_original)

match <- sf::read_sf(match_url)

View(match |> sf::st_drop_geometry())

library(mapview)

shape_debug = "shp_4516_0_2" # "shp_4512_0_2" 
gtfs_original_shapes |> filter(shape_id == shape_debug)
match |> filter(shape_id == shape_debug)
(match |> filter(shape_id == shape_debug))$geom

mapview(gtfs_original_shapes |> filter(shape_id == shape_debug), layer.name="GTFS shape") +
  mapview(match |> filter(shape_id == shape_debug), layer.name="OSM route relation")
