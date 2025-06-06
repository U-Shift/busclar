library(sf)
library(dplyr)

# multilinestring = carris_osm_carreira[c(1), ] |> select(geometry) |> st_as_sf()
# mapview(multilinestring)

multiline_to_sorted_linestring <- function (multilinestring) { 
  
  # browser()

  # 1. Extract all individual LINESTRING components
  linestrings <- st_cast(multilinestring, "LINESTRING") %>% st_as_sf() %>% st_set_geometry("geometry")
  
  # 2. Find the correct order by connecting endpoints
  #    - Get all start and end points
  linestrings <- linestrings %>%
    mutate(
      start = lwgeom::st_startpoint(geometry),
      end = lwgeom::st_endpoint(geometry)
    )
  
  # 3. Reorder the linestrings by finding the best sequence
  
  # Find the best path (simplified approach)
  # (This part may need adjustment based on your data)
  ordered_lines <- list()
  current_line <- linestrings[1, ] # Start with the first line
  ordered_lines[[1]] <- current_line$geometry
  remaining_lines <- linestrings[-1, ]
  
  while (nrow(remaining_lines) > 0) {
    last_point <- lwgeom::st_endpoint(current_line$geometry)
    
    # Find the closest line segment to continue the route
    nearest_idx <- st_nearest_feature(last_point, remaining_lines)
    next_line <- remaining_lines[nearest_idx, ]
    
    # Check if we need to reverse the next line to connect properly
    next_start <- lwgeom::st_startpoint(next_line$geometry)
    next_end <- lwgeom::st_endpoint(next_line$geometry)
    
    if (st_distance(last_point, next_start) > st_distance(last_point, next_end)) {
      next_line$geometry <- st_reverse(next_line$geometry)
    }
    
    ordered_lines[[length(ordered_lines) + 1]] <- next_line$geometry
    remaining_lines <- remaining_lines[-nearest_idx, ]
    current_line <- next_line
  }
  
  # 4. Extract ALL coordinates in order
  all_coords <- lapply(ordered_lines, st_coordinates) %>% 
    do.call(rbind, .) %>%  # Combine all coordinate matrices
    .[, 1:2]  # Keep only X/Y columns (drop L1 if present)
  
  # 5. Create new LINESTRING from the combined coordinates
  final_linestring <- st_linestring(all_coords)
  
  # 6. Convert to sf object (optional)
  final_linestring_sf <- st_sf(
    geometry = st_sfc(final_linestring, crs = st_crs(carris_osm_carreira))
  )
  
}

# 5. Verify start and endpoints
# final_linestring = multiline_to_linestring(final_linestring)
# start <- lwgeom::st_startpoint(final_linestring_sf)
# end <- lwgeom::st_endpoint(final_linestring_sf)
# 
# mapview(final_linestring_sf, color="gray") + mapview(start, col.regions="#ffd400") + mapview(end, col.regions="#003f8f")
