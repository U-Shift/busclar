# GTFS for a given region

library(GTFShift)
library(sf)
library(dplyr)


aml = sf::st_read("https://github.com/U-Shift/MQAT/raw/refs/heads/main/geo/MUNICIPIOSgeo.gpkg", quiet = TRUE)
lisboa = aml |> dplyr::filter(Concelho == "Lisboa") |> sf::st_bbox()


data_sources = query_mobilitydatabase(token = Sys.getenv("MOBILITY_DATABASE"),
                       # bounding_filter_method = "partially_enclosed",
                       bbox = lisboa,
                       country_code = "PT",
                       subdivision_name = "Lisbon" # better results than "Lisboa"
                       # is_official = TRUE
                      )|>
  filter(status == "active")
# 7 results

# I want to keep and merge Carris and Carris Metropolitana

gtfs_carris = load_feed(data_sources$producer_url[1])
gtfs_carris_metropolitana = load_feed(data_sources$producer_url[2])
gtfs_carris_metropolitana$calendar = create_calendar(gtfs_carris_metropolitana) # otherwise next step produces error

# Filter by the next working day and by mode
# 0 = Tram, Streetcar, Light rail. 
# 3 = Bus. Used for short- and long-distance bus routes.

calendar_nextBusinessWednesday <- function(start_date = Sys.Date()) {
  year <- lubridate::year(start_date)
  holidays <- calendar_get_pt_holidays(year)
  
  # Find the next Wednesday
  next_wed <- start_date + (4 - lubridate::wday(start_date) + 7) %% 7
  
  # If next Wednesday is a holiday, keep searching
  while (next_wed %in% holidays) {
    next_wed <- next_wed + 7  # Move to the next Wednesday
    
    # If we cross into a new year, update holidays
    if (year(next_wed) != year) {
      year <- lubridate::year(next_wed)
      holidays <- calendar_get_pt_holidays(year)
    }
  }
  
  return(next_wed)
}
calendar_get_pt_holidays <- function(year) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/PT")
  response <- httr::GET(url)
  
  if (status_code(response) == 200) {
    holidays <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))
    return(as.Date(holidays$date))
  } else {
    stop("Failed to retrieve holidays. Please check your internet connection or API availability.")
  }
}
if (is.null(date)) {
  date = calendar_nextBusinessWednesday()
  message(sprintf("> Reference date not provided, considering next business wednesday: %s...", date))
}
date


gtfs_carris = gtfs_carris |>
  filter_by_modes(modes = list(0, 3)) |>  # filter by mode = tram and bus
  



# filter by mode = bus



# it is recommended tho filter by date before merging, otherwise it is very time consuming

gtfs_bus_lisbon = unify(list(gtfs_carris, gtfs_carris_metropolitana), generateTransfers = TRUE)

tidytransit::write_gtfs(gtfs_bus_lisbon, "data/gtfs/gtfs_bus_lisbon.zip")
piggyback::pb_upload("data/gtfs/gtfs_bus_lisbon.zip")
