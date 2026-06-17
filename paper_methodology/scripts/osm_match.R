library(GTFShift)
library(dplyr)
library(stringr)
library(mapview)
library(osmdata)

# Run with: $ Rscript 01_osm_match/osm_match.R

# get_overpass_url()
# set_overpass_url("https://maps.mail.ru/osm/tools/overpass/api/interpreter")
# set_overpass_url("https://overpass.private.coffee/api/interpreter") # 4 servers with 20 cores, 256GB RAM, SSD each
# set_overpass_url("https://overpass-api.de/api/interpreter")
# get_overpass_url()

# Refer to osm_match_parameters.R to define parameters before running this script!
output_root <- "osm_match"

# Define regions to analyse
regions <- data.frame(
  name = character(),
  gtfs_url = character(),
  query = I(list())
)
data <- read.csv(system.file("extdata", "gtfs_sources_pt.csv", package = "GTFShift"))

regions <- bind_rows( # Lisboa
  regions,
  data.frame(
    name = "lisboa",
    gtfs_url = data$URL[data$ID == "lisboa"],
    gtfs_day_filter = TRUE,
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Carris", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/portugal",
    osm_stop_order_relaxed = TRUE
  )
)
regions <- bind_rows( # Barreiro
  regions,
  data.frame(
    name = "barreiro",
    gtfs_url = data$URL[data$ID == "barreiro"],
    gtfs_day_filter = TRUE,
    gtfs_day = gsub("-", "", Sys.Date()),
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = c("TCB", "Transportes Coletivos do Barreiro"), key_exact = TRUE)
    ))),
    geofabrik_region = "europe/portugal"
  )
)
regions <- bind_rows( # Cascais
  regions,
  data.frame(
    name = "cascais",
    gtfs_url = data$URL[data$ID == "cascais"],
    gtfs_day_filter = TRUE,
    gtfs_day = gsub("-", "", Sys.Date()),
    gtfs_manipulate = "manipulate_gtfs_cascais",
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "MobiCascais", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/portugal"
  )
)
manipulate_gtfs_cascais <- function(gtfs) {
  # Change route_short_name from numeric to M%02d format (1 should become M01) to assure OSM ref match
  gtfs$routes$route_short_name <- sprintf("M%02d", as.numeric(gtfs$routes$route_short_name))
  return(gtfs)
}
regions <- bind_rows( # AML
  regions,
  data.frame(
    name = "AML",
    # For historical versions, refer to https://mobilitydatabase.org/feeds/gtfs/mdb-2027
    gtfs_url = data$URL[data$ID == "AML"],
    gtfs_day = gsub("-", "", Sys.Date()),
    gtfs_day_filter = TRUE,
    gtfs_manipulate = "manipulate_gtfs_aml",
    query = I(list(list(
      list(key = "route", value = c("bus"), key_exact = TRUE),
      list(key = "network", value = "Carris Metropolitana", key_exact = TRUE)
    ))),
    geofabrik_region = "europe/portugal",
    osm_stop_order_relaxed = TRUE
  )
)
manipulate_gtfs_aml <- function(gtfs) {
  # Rename all shapes that start with [.*], remove that part
  gtfs$shapes$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs$shapes$shape_id)
  gtfs$trips$shape_id <- gsub("^\\[[^]]*\\]\\s*", "", gtfs$trips$shape_id)
  return(gtfs)
}

# main()
for (i in 1:nrow(regions)) {
  region <- regions[i, ]
  output_region <- sprintf("%s/%s/gtfs_%s", output_root, tolower(region$name), gsub("-", "", region$gtfs_day))
  output <- sprintf("%s/run_%s", output_region, format(Sys.time(), "%Y%m%d_%H%M%S"))
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE)
  }
  message(sprintf("\n\nRunning for %s (%s)...", region$name, region$gtfs_day))

  gtfs_file <- sprintf("%s/gtfs_%s_%s.zip", output_region, region$name, region$gtfs_day)
  if (file.exists(gtfs_file)) {
    message("Loading gtfs from file...")
    gtfs <- GTFShift::load_feed(gtfs_file, create_transfers = FALSE)
  } else {
    message("Downloading gtfs...")
    gtfs <- GTFShift::load_feed(region$gtfs_url, headers = if (!is.null(region$gtfs_url_headers)) unlist(region$gtfs_url_headers[[1]]) else NULL, create_transfers = FALSE)
    tidytransit::write_gtfs(gtfs, gtfs_file)
  }
  summary(gtfs)
  # assign(sprintf("gtfs_%s_%s", region$name, region$gtfs_day), gtfs)

  if (!is.null(region$gtfs_manipulate) && !is.na(region$gtfs_manipulate) || !is.na(region$gtfs_day_filter) && !is.null(region$gtfs_day_filter)) {
    if (!is.na(region$gtfs_day_filter) && !is.null(region$gtfs_day_filter)) {
      message(sprintf("Filter gtfs for %s...", region$gtfs_day))
      gtfs = tidytransit::filter_feed_by_date(gtfs, extract_date = region$gtfs_day)
    } 
    if (!is.null(region$gtfs_manipulate) && !is.na(region$gtfs_manipulate)) {
      message("Manipulating gtfs...")
      gtfs <- get(region$gtfs_manipulate)(gtfs)
    }
    gtfs_file_manipulated <- sprintf("%s/gtfs_%s_%s_manipulated.zip", output_region, region$name, region$gtfs_day)
    if (!file.exists(gtfs_file_manipulated)) {
      tidytransit::write_gtfs(gtfs, gtfs_file_manipulated)
    }
  }

  gtfs_shapes <- tidytransit::shapes_as_sf(gtfs$shapes)
  bbox <- sf::st_bbox(gtfs_shapes)

  # Build OSM query
  q <- opq(bbox = bbox)
  for (feat in region$query[[1]]) {
    q <- add_osm_feature(
      q,
      key = feat$key,
      value = feat$value,
      key_exact = if (!is.null(feat$key_exact)) feat$key_exact else FALSE
    )
  }
  # assign(sprintf("q_%s_gtfs%s", region$name, region$gtfs_day), q)

  # Get OSM extract to avoid API call
  # osmextract::oe_download_directory()
  if (is.null(region$geofabrik_region)) {
    stop("Please define the geofabrik_region for this region in osm_match_parameters.R")
  }
  osm_file <- osmextract::oe_download(
    sprintf("https://download.geofabrik.de/%s-latest.osm.pbf", region$geofabrik_region),
    file_basename = sprintf("%s_%s.osm.pbf", str_replace_all(region$geofabrik_region, "/", "_"), format(Sys.Date(), "%Y%m%d"))
  )

  # Match shapes geometry
  message("Matching shapes...")
  shapes_match_routes <- GTFShift::osm_shapes_match_routes(
    gtfs, q,
    gtfs_match = if (!is.null(region$gtfs_match)) region$gtfs_match else "route_short_name",
    osm_match = if (!is.null(region$osm_match)) region$osm_match else "ref",
    gtfs_osm_match_exact = if (!is.null(region$gtfs_osm_match_exact)) region$gtfs_osm_match_exact else TRUE,
    log_file = sprintf("%s/shapes_match_%s_gtfs%s_run%s.r.log", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())),
    osm_file = osm_file,
    num_cores = max(1, floor(parallel::detectCores() / 2)),
    osm_stop_order_relaxed = if (!is.null(region$osm_stop_order_relaxed)) region$osm_stop_order_relaxed else FALSE,
    osm_route_type = if (!is.null(region$osm_route_type)) region$osm_route_type else "bus"
  )
  # assign(sprintf("shapes_match_routes_%s_gtfs%s", region$name, region$gtfs_day), shapes_match_routes)

  write.csv(shapes_match_routes |> sf::st_drop_geometry() |> mutate(
    distance_diff = round(distance_diff),
    points_diff = round(points_diff)
  ), sprintf("%s/shapes_match_%s_gtfs%s_run%s.csv", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), row.names = FALSE)
  sf::st_write(shapes_match_routes, sprintf("%s/shapes_match_%s_gtfs%s_run%s.gpkg", output, region$name, region$gtfs_day, gsub("-", "", Sys.Date())), append = FALSE)
  message("Done! :)")
}
