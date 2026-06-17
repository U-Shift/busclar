aml_match = sf::st_read("https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_AML_gtfs2026-05-27_run20260615.gpkg")

aml_match
View(aml_match)

barreiro_match = sf::st_read("https://github.com/U-Shift/GTFShift/releases/download/v0.9/shapes_match_barreiro_gtfs20260518_run20260518.gpkg")
barreiro_match
View(barreiro_match)

names(barreiro_match)
barreiro_match |>
  mutate(
    distance_diff = round(distance_diff, 2),
    points_diff = round(points_diff, 2),
    stops_diff = round(stops_diff, 2)
  ) |> 
  select(shape_id, osm_id, distance_diff, points_diff, stops_diff)
