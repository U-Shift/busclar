# Get bus lanes from osm and compare

# remotes::install_github("nptscot/osmactive")
library(osmdata)
library(sf)
library(dplyr)

Lisbon_limit = st_read("data/Lisboa_limite.gpkg")
Lisbon_limit = st_transform(Lisbon_limit, 4326)
BBOX = st_bbox(Lisbon_limit)

# Get bus lanes from OSM
road_osm = opq(BBOX) |> # uses osmdata package, to extract only with BB
  add_osm_feature(key = "highway") |> 
  osmdata_sf() |> 
  osm_poly2line() # makes roundabouts into lines

road_osm = road_osm$osm_lines |>
  select(osm_id, name, highway, geometry)

road_osm = road_osm |>
  dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                               'secondary',"secondary_link", "trunk", 'trunk_link',
                               "tertiary", "tertiary_link", 
                               "residential", "living_street", "unclassified", "service"))

osm_lanes = road_osm |> select(contains("psv"))
osm_lanes = osm_lanes |> filter(psv == "designated" | 
                                        `lanes:psv` == 1 |
                                        `lanes:psv:forward` == 1 |
                                        `lanes:psv:backward` == 1 |
                                        `psv:lanes:backward` == "designated" |
                                        `psv:lanes:forward` == "designated" |
                                        !is.na(`psv:lanes`)
                                  )
mapview(osm_lanes)
