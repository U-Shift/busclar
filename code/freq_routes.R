# route frequency, based on https://github.com/Bondify/GTFS_in_R/

library(tidyverse)
library(tidytransit)
library(sf)
library(stplanr)
library(mapview)
mapviewOptions(vector.palette = hcl.colors(palette = "viridis", n = 80, rev = TRUE),
               legend.pos = "bottomright")

# Get GTFS data for a given region
bus_operator = "carris_lisboa"
lisbon_gtfs_url = "https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS"
gtfs_location = paste0("data/gtfs/", bus_operator, ".zip")
download.file(lisbon_gtfs_url, destfile = gtfs_location, mode = "wb") # 24MB
gtfs = tidytransit::read_gtfs(gtfs_location)

# gtfs filtered
# service id for the selected date
date = "2025-04-02"

gtfs_date <- tidytransit::filter_feed_by_date(
  gtfs, extract_date = date)
gtfs_date = tidytransit::gtfs_as_sf(gtfs_date)

trips = gtfs_date$trip
stops = gtfs_date$stops
shapes = gtfs_date$shapes
routes = gtfs_date$routes
stop_times = gtfs_date$stop_times

stop_times <- stop_times %>% 
  left_join(trips) %>% 
  left_join(routes) %>% 
  select(route_id, route_short_name, trip_id, stop_id, service_id, arrival_time, departure_time, direction_id, shape_id, stop_sequence)

stop_times <- stop_times %>% # só partidas
  filter(
    stop_sequence == 1)

stop_times <- stop_times %>% 
  mutate(
    arrival_hour = lubridate::hour(arrival_time)
  )

freq_data <- stop_times %>% 
  group_by(route_id, route_short_name, direction_id, arrival_hour) %>% 
  summarize(freq = n()) %>%
  ungroup()

routes_freq =
  freq_data %>%
  left_join(trips %>%
              select(route_id, direction_id, shape_id) %>%
              distinct()) %>%
  as.data.frame() %>%
  left_join(shapes) %>%
  st_as_sf()

# e fico com a frequência horária de cada linha (atenção que é hora de partida!)
class(routes_freq)


## overline
routes_freq_all = data.frame()
for (h in 0:23) { # hours of the day
  routes_freq_h = routes_freq %>% 
    filter(arrival_hour == h) %>% 
    overline2(attrib = "freq") %>% 
    arrange(freq) %>% 
    mutate(hour = h)
  
  routes_freq_all = rbind(routes_freq_all, routes_freq_h)
}

st_write(routes_freq_all, paste0("data/", bus_operator, "_routes_freq.gpkg"))
# piggyback::pb_upload(paste0("data/", gtfs_location, "routes_freq.gpkg"))


# for a given hour
h = 8 # test
routes_freq_hour = routes_freq_all %>% 
  filter(hour == h) 

summary(routes_freq_hour$freq)

## mapas
# with all
mapview(
  routes_freq_hour,
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequência",
  lwd.multiplier = 2 # acho que não faz nada
)
# above 2 per hour
mapview(
  routes_freq_hour %>% filter(freq > 2),
  zcol = "freq",
  lwd = "freq",
  layer.name = "Frequency (hour)"
)




# 
# library(tmap)
# tmap_mode("view")
# tm_shape(routes_freq_8 %>% arrange(freq)) +
#   tm_lines(id = "freq",
#           title = "Frequência de autocarros por via (8-9h)",
#           lwd = "freq",
#            lwd.legend = tm_legend_hide(),
#            col_alpha = 0.9,
#            scale = 15,
#                  # lwd.multiplier = 18,
#            col = "freq",
#            palette = cols4all::c4a(palette = "-greek"),
#              title.col = "Frequência / hora",
#   )
#   


