gtfs_original <- tidytransit::read_gtfs("https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518.zip")
gtfs_original$shapes
names(gtfs_original$shapes)
nrow(gtfs_original$shapes)

write.csv(gtfs_original$shapes |> select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence), "gtfs_barreiro_20260518_shapes.csv", row.names = FALSE)

gtfs_osm <- tidytransit::read_gtfs("https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518_osm.zip")
gtfs_osm$shapes
names(gtfs_osm$shapes)
nrow(gtfs_osm$shapes)

write.csv(gtfs_osm$shapes |> select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence), "gtfs_barreiro_20260518_osm_shapes.csv", row.names = FALSE)

gtfs_original_shapes_osm <- gtfs_original$shapes |> filter(shape_id %in% gtfs_osm$shapes$shape_id)
write.csv(gtfs_original_shapes_osm |> select(shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence), "gtfs_barreiro_20260518_shapes_osm.csv", row.names = FALSE)
