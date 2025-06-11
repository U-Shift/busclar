# clean carris netowrk overline
library(sf)
library(dplyr)
library(mapview)
library(qgisprocess)
library(sfnetworks)
library(tidygraph)


carris_overline = st_read("https://github.com/U-Shift/busclar/releases/download/latest/carris_overline.gpkg")

carris_sample = carris_overline |>
  filter(hour == 8) |>
  mutate(FID = as.integer(1)) |>
  st_transform(3857)

carris_dissolve = carris_overline |> 
  summarise(geom = st_union(geom))
carris_dissolve = carris_dissolve |> 
  mutate(FID = as.integer(1)) |> 
  st_transform(3857)
  # stplanr::line_cast()

clean_osm = function(road_network) {
  
    # I had to leave empy the Pyhton env in global options for this to work!!!!!!
    # options(qgisprocess.path="/usr/bin/qgis_process.bin") # if not defined
    
    # qgis_configure() # to enable plugguins. we only need to use Grass
    # qgis_plugins() #não tem o disconnected islands
    
  # algorithms = qgis_algorithms()
  # algorithms |> filter(grepl(pattern = "clean", x = algorithm, ignore.case = TRUE))
  # qgis_show_help("grass:v.clean")
    
    input = road_network |> 
      # mutate(fid_2 = as.integer(1:nrow(road_network))) |> 
      st_write(paste0("other/road_network_clean.shp"), delete_dsn = TRUE, delete_layer = TRUE, quiet = TRUE)
    
    input = st_read(paste0("other/road_network_clean.shp"), quiet = TRUE) #because of the fid column
    
    # # delete existing outputs
    # if (file.exists(paste0("outputdata/", CITY, "/road_network.shp"))){
    #   file.remove(paste0("outputdata/", CITY, "/road_network.shp"))
    # }
    # 
    # input = input |> 
    #   stplanr::line_cast() # to avoid the error: Geometries are not all of type LINESTRING, or all of type POINT
    # 
    output_path = paste0("other/road_network_clean.shp")
    
    output = qgis_run_algorithm(
      algorithm = "grass:v.clean", # the new name removes the 7!
      input = input, 
      type = c(0, 1, 2, 3, 4, 5, 6), 
      tool = c(0, 1, 2, 6, 8), #break, snap, rmdangle, rmdupl, bpol
      threshold = c("0", "0.00000100", "0.00000100", "0", "0"), 
      output = output_path, # need to be defined otherwise it saves in tmp.gpkg and makes the error with fid (# ERROR 1: failed to execute insert : UNIQUE constraint failed: outpute935bd152d284569afb314c88e8fce09.fid)
      error = qgis_tmp_vector(),
      GRASS_OUTPUT_TYPE_PARAMETER = "auto",
      # 'GRASS_REGION_PARAMETER':None, 
      # 'GRASS_SNAP_TOLERANCE_PARAMETER':-1, 
      # 'GRASS_MIN_AREA_PARAMETER':0.0001, 
      # 'GRASS_VECTOR_DSCO':'', 
      # 'GRASS_VECTOR_LCO':'', 
      # 'GRASS_VECTOR_EXPORT_NOCAT':False
      .quiet = TRUE
    )
    
    road_network_clean = sf::st_read(output[["output"]][1], quiet = TRUE)
    # |> select(-fid_2)
    # 
    st_write(road_network_clean, output_path, delete_dsn = TRUE, quiet = TRUE)
    # # cleaning the unnecessary nodes, using tidygraph and sfnetworks
    # 
    road_network_clean = st_read("other/carris_clean_linestring.shp", quiet = TRUE)
    road_network_clean = as_sfnetwork(road_network_clean) # ERROR HERE!
    # 
    road_network_clean = convert(road_network_clean, to_spatial_smooth) |>
      activate(edges) |>
      as_tibble() |>
      # select(cat, osm_id, highway, geometry) |>
      mutate(edgeID = c(1:n())) |>
      st_as_sf()
    # 
    # st_write(road_network_clean, output_path, delete_dsn = TRUE, quiet = TRUE)
    # 
    # see trafficcalmr::osm_consolidate as an option!
    # https://saferactive.github.io/trafficalmr/reference/osm_consolidate.html
    # remotes::install_github("saferactive/trafficalmr")
    # road_network_clean_consolidate = road_network_clean |> st_transform(3857) |> trafficalmr::osm_consolidate(200)
    # osm_tags missing here, not working!
    
    st_write(road_network_clean, "other/carris_clean_clean.shp", delete_dsn = TRUE, quiet = TRUE)
}

# st_write(carris_dissolve, "other/carris_dissolve.shp", delete_dsn = TRUE, quiet = TRUE)

carris_clean = clean_osm(carris_dissolve) # not all LINESTRING. NOW THE ERROR IS WRITING THE LAYER. TRY TO REBOOT
carris_clean = clean_osm(carris_sample)





# carris metro shapes -----------------------------------------------------

carris_metrop_shapes = tidytransit::gtfs_as_sf(gtfs_carris_metropolitana)
carris_metrop_shapes = carris_metrop_shapes$shapes |> 
  st_as_sf()

carris_metrop_shapes_lx = carris_metrop_shapes |>
  st_transform(3857) |> 
  st_crop(stplanr::geo_buffer(road_network_clean, dist = 500))
st_write(carris_metrop_shapes_lx, "other/carris_metrop_shapes_lx.shp", delete_dsn = TRUE, quiet = TRUE)
