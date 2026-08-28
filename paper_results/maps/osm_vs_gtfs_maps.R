library(mapview)
library(dplyr)

gtfs_osm_url <- "https://github.com/U-Shift/GTFShift/releases/download/v0.9/gtfs_2026-05-27_bus_unified_osm.zip"
gtfs_osm <- tidytransit::read_gtfs(gtfs_osm_url)
summary(gtfs_osm)

shapes_osm <- tidytransit::shapes_as_sf(gtfs_osm$shapes)


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
  left_join(go_gtfs_conversion, by = c("agency" = "gtfs_agency_id"))

# Replace NA values in agency_name with "Unknown"
shapes_osm_with_agency <- shapes_osm_with_agency |>
  mutate(agency_name = ifelse(is.na(agency_name), "Carris Metropolitana", agency_name))

# Sort by agency, with order: 49, 8, 1, 21
shapes_osm_with_agency <- shapes_osm_with_agency[order(match(shapes_osm_with_agency$agency, c("49", "8", "1", "21", "Unknown"))), ]
View(shapes_osm_with_agency)

# Get municipalities limits 
municipalities <- sf::st_read("https://github.com/U-Shift/busclar/releases/download/0.9/municipalities.gpkg")

mapview(municipalities, color = "#1cabce", alpha = 1, alpha.regions = 0, col.regions = "#1cabce", layer.name = "LMA municipalities", lwd = 1.5) +
mapview(
  shapes_osm_with_agency |> filter(agency_name == "Carris Metropolitana"),
  zcol = "agency_name", layer.name = "OSM route geometries",
  legend = TRUE, homebutton = FALSE,
  # color = "#363636"
  color = "#363636"
)
