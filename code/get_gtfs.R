# GTFS for a given region

library(GTFShift)
library(sf)
library(dplyr)
library(httr)

aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
lisboa = aml |> dplyr::filter(Concelho == "Lisboa") |> sf::st_bbox()

query_mobilitydatabase <- function(
    token,
    bounding_filter_method = "partially_enclosed",
    limit=10,
    offset=0,
    country_code=NA,
    subdivision_name=NA,
    municipality=NA,
    dataset_latitudes=NA,
    dataset_longitudes=NA,
    is_official=NA
) {
  
  url <- "https://api.mobilitydatabase.org/v1/gtfs_feeds"
  
  params <- list(
    bounding_filter_method=bounding_filter_method,
    limit = limit,
    offset = offset
  )
  # if (!is.na(country_code)) params["country_code"] = country_code
  # if (!is.na(subdivision_name)) params["subdivision_name"] = subdivision_name
  # if (!is.na(municipality)) params["municipality"] = municipality
  # if (!is.na(dataset_latitudes)) params["dataset_latitudes"] = dataset_latitudes
  # if (!is.na(dataset_longitudes)) params["dataset_longitudes"] = dataset_longitudes
  # if (!is.na(is_official)) params["is_official"] = is_official
  
  response <- GET(
    url,
    query = params,
    add_headers(
      `accept` = "application/json",
      `Authorization` = sprintf("Bearer %s", token)
    )
  )
  
  # Convert response to data.frame
  content <- content(response, as = "parsed")
  
  
  safe_extract <- function(x) if (is.null(x)) NA else x
  
  feeds_df <- bind_rows(lapply(content, function(feed) {
    data.frame(
      id = safe_extract(feed$id),
      data_type = safe_extract(feed$data_type),
      created_at = safe_extract(feed$created_at),
      provider = safe_extract(feed$provider),
      feed_contact_email = safe_extract(feed$feed_contact_email),
      status = safe_extract(feed$status),
      official = safe_extract(feed$official),
      official_updated_at = safe_extract(feed$official_updated_at),
      feed_name = safe_extract(feed$feed_name),
      note = safe_extract(feed$note),
      
      # Flatten nested 'source_info'
      producer_url = safe_extract(feed$source_info$producer_url),
      license_url = safe_extract(feed$source_info$license_url),
      
      # First location (if exists)
      country_code = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$country_code) else NA,
      country = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$country) else NA,
      subdivision_name = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$subdivision_name) else NA,
      municipality = if (length(feed$locations) >= 1) safe_extract(feed$locations[[1]]$municipality) else NA,
      
      # Flatten latest_dataset (may be NULL)
      latest_dataset_id = safe_extract(feed$latest_dataset$id),
      latest_dataset_url = safe_extract(feed$latest_dataset$hosted_url),
      min_lat = safe_extract(feed$latest_dataset$bounding_box$minimum_latitude),
      max_lat = safe_extract(feed$latest_dataset$bounding_box$maximum_latitude),
      min_lon = safe_extract(feed$latest_dataset$bounding_box$minimum_longitude),
      max_lon = safe_extract(feed$latest_dataset$bounding_box$maximum_longitude),
      downloaded_at = safe_extract(feed$latest_dataset$downloaded_at),
      service_start = safe_extract(feed$latest_dataset$service_date_range_start),
      service_end = safe_extract(feed$latest_dataset$service_date_range_end),
      agency_timezone = safe_extract(feed$latest_dataset$agency_timezone),
      
      # Validation summary
      validation_errors = safe_extract(feed$latest_dataset$validation_report$total_error),
      validation_warnings = safe_extract(feed$latest_dataset$validation_report$total_warning),
      stringsAsFactors = FALSE
    )
  }))
  
  return(feeds_df)
}


query_mobilitydatabase(token = Sys.getenv("MOBILITY_DATABASE"),
                       is_official = FALSE,
                       dataset_latitudes = c(lisboa[[2]], lisboa[[4]]),
                       dataset_longitudes = c(lisboa[[1]], lisboa[[3]]))
