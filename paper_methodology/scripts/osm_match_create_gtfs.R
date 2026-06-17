library(mapview)
library(sf)
library(dplyr)

# Run with: $ Rscript 03_create_gtfs/osm_match_create_gtfs.R

# Parameters
output_root <- "osm_gtfs"
data <- read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))
gtfs_regions <- data.frame(
    name = character(),
    gtfs_url = character(),
    osm_match = character()
)

manipulate_carris_met <- function(gtfs) {
    # Rename all shapes that start with [.*], remove that part
    gtfs$shapes$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs$shapes$shape_id)
    gtfs$trips$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs$trips$shape_id)
    return(gtfs)
}
manipulate_carris_met_area_1 <- function(gtfs) {
    gtfs <- GTFShift::filter_by_agency(gtfs, id = "41")
    gtfs <- manipulate_carris_met(gtfs)
    return(gtfs)
}

manipulate_carris_met_area_2 <- function(gtfs) {
    gtfs <- GTFShift::filter_by_agency(gtfs, id = 42)
    gtfs <- manipulate_carris_met(gtfs)
  return(gtfs)
}

manipulate_carris_met_area_3 <- function(gtfs) {
    gtfs <- GTFShift::filter_by_agency(gtfs, id = 43)
    gtfs <- manipulate_carris_met(gtfs)
  return(gtfs)
}

manipulate_carris_met_area_4 <- function(gtfs) {
    gtfs <- GTFShift::filter_by_agency(gtfs, id = 44)
    gtfs <- manipulate_carris_met(gtfs)
  return(gtfs)
}

gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "aml_1",
        gtfs_url = data$URL[data$ID == "AML"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        gtfs_manipulate = "manipulate_carris_met_area_1",
        osm_match = "osm_match/aml/gtfs_20260506/run_20260506_102712/shapes_match_AML_gtfs20260506_run20260506.gpkg"
    )
)
gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "aml_2",
        gtfs_url = data$URL[data$ID == "AML"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        gtfs_manipulate = "manipulate_carris_met_area_2",
        osm_match = "osm_match/aml/gtfs_20260506/run_20260506_102712/shapes_match_AML_gtfs20260506_run20260506.gpkg"
    )
)
gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "aml_3",
        gtfs_url = data$URL[data$ID == "AML"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        gtfs_manipulate = "manipulate_carris_met_area_3",
        osm_match = "osm_match/aml/gtfs_20260506/run_20260506_102712/shapes_match_AML_gtfs20260506_run20260506.gpkg"
    )
)
gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "aml_4",
        gtfs_url = data$URL[data$ID == "AML"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        gtfs_manipulate = "manipulate_carris_met_area_4",
        osm_match = "osm_match/aml/gtfs_20260506/run_20260506_102712/shapes_match_AML_gtfs20260506_run20260506.gpkg"
    )
)
gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "barreiro",
        gtfs_url = data$URL[data$ID == "barreiro"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        osm_match = "osm_match/barreiro/gtfs_20260518/run_20260518_123051/shapes_match_barreiro_gtfs20260518_run20260518.gpkg"
    )
)
gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "cascais",
        gtfs_url = data$URL[data$ID == "cascais"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        osm_match = "osm_match/cascais/gtfs_20260507/run_20260507_113820/shapes_match_cascais_gtfs20260507_run20260507.gpkg"
    )
)
gtfs_regions <- bind_rows(
    gtfs_regions,
    data.frame(
        name = "lisboa",
        gtfs_url = data$URL[data$ID == "lisboa"],
        gtfs_day = GTFShift::calendar_nextBusinessWednesday(),
        gtfs_day_filter = TRUE,
        osm_match = "osm_match/lisboa/gtfs_20260505/run_20260505_090741/shapes_match_lisboa_gtfs2026-05-05_run20260505.gpkg"
    )
)


# main()
output_dir <- sprintf("%s/%s/run_%s", output_root, tolower(paste(gtfs_regions$name, collapse = "_")), format(Sys.time(), "%Y%m%d_%H%M%S"))
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

unified_gtfs <- c()
for (i in 1:nrow(gtfs_regions)) {
    region <- gtfs_regions[i, ]
    message(sprintf("Running for %s...", region$name))

    # Load GTFS
    gtfs <- tidytransit::read_gtfs(region$gtfs_url)
    tidytransit::write_gtfs(gtfs, sprintf("%s/gtfs_%s.zip", output_dir, tolower(region$name)))
    summary(gtfs)

    if (!is.null(region$gtfs_manipulate) && !is.na(region$gtfs_manipulate) || !is.na(region$gtfs_day_filter) && !is.null(region$gtfs_day_filter)) {
        if (!is.na(region$gtfs_day_filter) && !is.null(region$gtfs_day_filter)) {
            message(sprintf("Filter gtfs for %s...", region$gtfs_day))
            gtfs = tidytransit::filter_feed_by_date(gtfs, extract_date = region$gtfs_day)
        } 
        if (!is.null(region$gtfs_manipulate) && !is.na(region$gtfs_manipulate)) {
            message("Manipulating gtfs...")
            gtfs <- get(region$gtfs_manipulate)(gtfs)
        }
        gtfs_file_manipulated <- sprintf("%s/gtfs_%s_manipulated.zip", output_dir, tolower(region$name))
        tidytransit::write_gtfs(gtfs, gtfs_file_manipulated)
    }

    shapes_gtfs <- tidytransit::shapes_as_sf(gtfs$shapes)
    message(sprintf("> GTFS has %s shapes", nrow(shapes_gtfs)))

    # Load OSM shapes
    osm_shapes <- sf::st_read(region$osm_match) |> distinct(shape_id, .keep_all=TRUE) # Prevent duplicated matches
    message(sprintf("> OSM has %s shapes", nrow(osm_shapes)))
    osm_shapes <- osm_shapes |> filter(distance_diff < 1000 & points_diff < 500)
    message(sprintf("> Of those, %d meet distance criteria", nrow(osm_shapes)))

    # Filter GTFS for shapes
    osm_shape_ids <- unique(osm_shapes$shape_id)
    gtfs_trips_with_osm_shapes <- gtfs$trips |>
        filter(shape_id %in% osm_shape_ids) |>
        pull(trip_id) |>
        unique()
    gtfs_filtered <- tidytransit::filter_feed_by_trips(gtfs, trip_ids = gtfs_trips_with_osm_shapes)
    message(sprintf("> GTFS has %s trips", nrow(gtfs$trips)))
    message(sprintf("> Reduced to %s trips meeting distance criteria", nrow(gtfs_filtered$trips)))

    # Replace shapes on GTFS
    message("> Starting conversion of OSM shapes from MULTILINESTRING to LINESTRING (takes some time...)")
    osm_shapes_txt <- GTFShift::create_shapes_from_sf(osm_shapes, gtfs_filtered)
    message(sprintf("> OSM MULTILINESTRING → LINESTRING → shapes.txt conversion generated %d points", nrow(osm_shapes_txt)))

    message("> Replacing GTFS shapes with OSM shapes and saving to new GTFS file")
    gtfs_filtered$shapes <- osm_shapes_txt
    summary(gtfs_filtered)

    # Append _osm to gtfs_file
    gtfs_file_osm <- sprintf("%s/gtfs_%s_osm.zip", output_dir, tolower(region$name))
    tidytransit::write_gtfs(gtfs_filtered, gtfs_file_osm)
    message(sprintf("> Saved to %s", gtfs_file_osm))
    unified_gtfs <- append(unified_gtfs, gtfs_file_osm)
}
