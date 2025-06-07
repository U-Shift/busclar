# osm_bus_extract for Carris Metropolitana
library(osmdata)
library(sf)
library(mapview)
library(dplyr)
library(stplanr)

# available_features()
# available_tags("route")

carrismet_osm = opq("Lisbon")  |>
  add_osm_feature(key = "route",
                  value = c("bus", "tram")) |> # exclude "funicular"
  osmdata_sf()
# mapview(carrismet_osm$osm_lines) ## alll

table(carrismet_osm$osm_multilines$operator)
# Alsa Todi          Barraqueiro Oeste                 Boa Viagem                     Carris 
# 30                          2                          1                        298 
# CarrisTur                    Flixbus                     Gipsyy Rede Nacional de Expressos 
# 1                         19                          1                         10 
# Rodoviaria de Lisboa       Rodoviária de Lisboa    Transportes Sul do Tejo            Viação Alvorada 
# 2                        279                         31                        182 

