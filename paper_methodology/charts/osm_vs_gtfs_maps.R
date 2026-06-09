library(mapview)
library(dplyr)

gtfs_original_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_1_manipulated.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_cascais.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_lisboa_osm.zip"
gtfs_original_url_2 <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_2_manipulated.zip"
gtfs_original_url_3 <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_3_manipulated.zip"
gtfs_original_url_4 <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_4_manipulated.zip"

gtfs_osm_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_bus_unified_osm.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_1_osm.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_lisboa_osm.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_cascais_osm.zip"
  # "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_barreiro_20260518_osm.zip"
gtfs_osm_url_2 <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_2_osm.zip"
gtfs_osm_url_3 <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_3_osm.zip"
gtfs_osm_url_4 <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_aml_4_osm.zip"

gtfs_original <- tidytransit::read_gtfs(gtfs_original_url)
summary(gtfs_original)
gtfs_osm <- tidytransit::read_gtfs(gtfs_osm_url)
summary(gtfs_osm)

shapes_original <- tidytransit::shapes_as_sf(gtfs_original$shapes)
shapes_original_2 <- tidytransit::shapes_as_sf(tidytransit::read_gtfs(gtfs_original_url_2)$shapes)
shapes_original_3 <- tidytransit::shapes_as_sf(tidytransit::read_gtfs(gtfs_original_url_3)$shapes)
shapes_original_4 <- tidytransit::shapes_as_sf(tidytransit::read_gtfs(gtfs_original_url_4)$shapes)
shapes_osm <- tidytransit::shapes_as_sf(gtfs_osm$shapes)
shapes_osm_2 <- tidytransit::shapes_as_sf(tidytransit::read_gtfs(gtfs_osm_url_2)$shapes)
shapes_osm_3 <- tidytransit::shapes_as_sf(tidytransit::read_gtfs(gtfs_osm_url_3)$shapes)
shapes_osm_4 <- tidytransit::shapes_as_sf(tidytransit::read_gtfs(gtfs_osm_url_4)$shapes)


mapview(shapes_original, color="#440154", layer.name="GTFS with original shapes", legend=FALSE, homebutton=FALSE)+ 
  mapview(shapes_osm, color="#70cf57", layer.name="GTFS with OSM geometries", legend=FALSE, homebutton=FALSE)
  
mapview(shapes_original, color="#440154", layer.name="GTFS with original shapes", legend=FALSE, homebutton=FALSE)+ 
  mapview(shapes_osm, color="#70cf57", layer.name="GTFS with OSM geometries", legend=FALSE, homebutton=FALSE) +
  mapview(shapes_original_2, color="#440154", layer.name="GTFS with original shapes (2)", legend=FALSE, homebutton=FALSE)+ 
  mapview(shapes_osm_2, color="#70cf57", layer.name="GTFS with OSM geometries (2)", legend=FALSE, homebutton=FALSE) +
  mapview(shapes_original_3, color="#440154", layer.name="GTFS with original shapes (3)", legend=FALSE, homebutton=FALSE)+ 
  mapview(shapes_osm_3, color="#70cf57", layer.name="GTFS with OSM geometries (3)", legend=FALSE, homebutton=FALSE) +
  mapview(shapes_original_4, color="#440154", layer.name="GTFS with original shapes (4)", legend=FALSE, homebutton=FALSE)+ 
  mapview(shapes_osm_4, color="#70cf57", layer.name="GTFS with OSM geometries (4)", legend=FALSE, homebutton=FALSE)

# Match OSM 
go_gtfs_conversion <- read.csv("paper_methodology/charts/go_gtfs_conversion.csv")

shapes_osm_with_agency <- shapes_osm |>
    mutate(
        # Split shape_id by _ and get first value
        agency = sapply(strsplit(shape_id, "_"), `[`, 1)
    ) |>
  mutate(agency = case_when(
    agency == "41" ~ "49",
    agency == "42" ~ "49",
    agency == "43" ~ "49",
    agency == "44" ~ "49",
    TRUE ~ agency
  )) |>
  left_join(go_gtfs_conversion, by=c("agency"="gtfs_agency_id"))

# Replace NA values in agency_name with "Unknown"
shapes_osm_with_agency <- shapes_osm_with_agency |>
  mutate(agency_name = ifelse(is.na(agency_name), "Carris Metropolitana", agency_name))

mapview(shapes_osm_with_agency, zcol="agency_name", layer.name="GTFS with OSM geometries", legend=TRUE, homebutton=FALSE)
