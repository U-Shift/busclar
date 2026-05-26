# OSM route=bus, train, light_rail, subway, tram, ferry
# Fetch taginfo chronology for these route tags and parse data to data.frame
# API Response structure:
# total:	INT	Total number of results.
# url:	STRING	URL of the request.
# data_until:	STRING	All changes in the source until this date are reflected in this taginfo result.
# data:	ARRAY OF HASHES	Array with results.
#   > date:	TEXT	Date in format YYYY-MM-DD.
#   > nodes:	INT	Difference of number of nodes with this key relative to previous entry.
#   > ways:	INT	Difference of number of ways with this key relative to previous entry.
#   > relations:	INT	Difference of number of relations with this key relative to previous entry.

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# User-Agent string to comply with OSM API policies
user_agent_str <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"

# Route types: https://wiki.openstreetmap.org/wiki/Key:route?uselang=pt-PT
route_types <- c("bus", "trolleybus", "minibus", "share_taxi", "train", "light_rail", "subway", "tram", "monorail", "ferry", "funicular")
df_list <- list()
min_date <- as.Date("2100-01-01")
max_date <- as.Date("1970-01-01")

# Fetch and calculate cumulative relations for each route type
for (type in route_types) {
    cat(sprintf("Fetching OSM route=%s chronology data from taginfo...\n", type))
    url <- paste0("https://taginfo.openstreetmap.org/api/4/tag/chronology?key=route&value=", type)
    res <- GET(url, user_agent(user_agent_str))
    if (status_code(res) == 200) {
        df <- fromJSON(content(res, as = "text", encoding = "UTF-8"))$data
        if (is.data.frame(df) && nrow(df) > 0) {
            df$date <- as.Date(df$date)
            df <- df %>%
                arrange(date) %>%
                mutate(relations_total = cumsum(relations)) %>%
                select(date, relations_total)

            colnames(df)[2] <- paste0("route_", type)
            df_list[[type]] <- df

            if (min(df$date) < min_date) min_date <- min(df$date)
            if (max(df$date) > max_date) max_date <- max(df$date)
        } else {
            cat(sprintf("No chronology data for route=%s, skipping.\n", type))
        }
    } else {
        warning(sprintf("Failed to fetch chronology for route=%s. Status: %d", type, status_code(res)))
    }
}

# Create a master date sequence
all_dates <- data.frame(date = seq(min_date, max_date, by = "day"))

# Join all route datasets
df_combined <- all_dates
for (type in route_types) {
    if (type %in% names(df_list)) {
        df_combined <- df_combined %>%
            left_join(df_list[[type]], by = "date")
    }
}

# Fill downward and replace NAs with 0
df_combined <- df_combined %>%
    fill(starts_with("route_"), .direction = "down") %>%
    mutate(across(starts_with("route_"), ~ coalesce(., 0)))

# Compute total public transit relations
df_combined <- df_combined %>%
    mutate(
        total_transit = rowSums(across(starts_with("route_")))
    )

# Fetch GTFS shape_id and route_id chronologies
cat("Fetching OSM gtfs:shape_id chronology data from taginfo...\n")
url_shape <- "https://taginfo.openstreetmap.org/api/4/key/chronology?key=gtfs:shape_id"
res_shape <- GET(url_shape, user_agent(user_agent_str))
df_shape <- fromJSON(content(res_shape, as = "text", encoding = "UTF-8"))$data
df_shape$date <- as.Date(df_shape$date)
df_shape <- df_shape %>%
    arrange(date) %>%
    mutate(gtfs_shapes_total = cumsum(relations)) %>%
    select(date, gtfs_shapes = gtfs_shapes_total)

cat("Fetching OSM gtfs:route_id chronology data from taginfo...\n")
url_route <- "https://taginfo.openstreetmap.org/api/4/key/chronology?key=gtfs:route_id"
res_route <- GET(url_route, user_agent(user_agent_str))
df_route <- fromJSON(content(res_route, as = "text", encoding = "UTF-8"))$data
df_route$date <- as.Date(df_route$date)
df_route <- df_route %>%
    arrange(date) %>%
    mutate(gtfs_routes_total = cumsum(relations)) %>%
    select(date, gtfs_routes = gtfs_routes_total)

# Merge everything into final dataset
df_final <- df_combined %>%
    left_join(df_shape, by = "date") %>%
    left_join(df_route, by = "date") %>%
    fill(gtfs_shapes, gtfs_routes, .direction = "down") %>%
    mutate(
        gtfs_shapes = coalesce(gtfs_shapes, 0),
        gtfs_routes = coalesce(gtfs_routes, 0)
    )

# ----------------- PLOT 1: Combined public transit vs GTFS tags -----------------
# Scale factor to align primary and secondary Y-axes
scale_factor <- 8

p_combined <- ggplot(df_final, aes(x = date)) +
    geom_line(aes(y = total_transit, color = "OSM Public Transit (Relations)"), linewidth = 1) +
    geom_line(aes(y = gtfs_routes * scale_factor, color = "OSM relations with gtfs:route_id"), linewidth = 0.8, linetype = "dashed") +
    geom_line(aes(y = gtfs_shapes * scale_factor, color = "OSM relations with gtfs:shape_id"), linewidth = 0.8, linetype = "dotdash") +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(
        name = "Total OSM Public Transit Relations (Solid)",
        labels = comma,
        sec.axis = sec_axis(~ . / scale_factor, name = "GTFS Tagged Relations (Dashed/Dotdash)", labels = comma)
    ) +
    scale_color_manual(
        name = "Metrics",
        values = c(
            "OSM Public Transit (Relations)" = "#0374b4",
            "OSM relations with gtfs:route_id" = "#AF7E04",
            "OSM relations with gtfs:shape_id" = "#2c8150"
        )
    ) +
    labs(
        title = "Growth of Public Transit Routes and GTFS Integration in OpenStreetMap",
        subtitle = "Comparing total transit relations (bus, train, light_rail, subway, tram, ferry) with GTFS tagging over time",
        x = "Year"
        # caption = paste("Data source: OpenStreetMap Taginfo API | Date until:", max(df_final$date))
    ) +
    theme_minimal(base_family = "sans") +
    theme(
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 8)),
        plot.subtitle = element_text(size = 9, color = "gray30", margin = margin(b = 12)),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 9),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(color = "#0374b4", face = "bold", size = 10),
        axis.title.y.right = element_text(color = "#AF7E04", face = "bold", size = 10)
        # plot.caption = element_text(size = 7, color = "gray50", margin = margin(t = 12))
    )

# Dynamically resolve output path depending on current working directory
out_dir <- if (dir.exists("paper_methodology")) "paper_methodology/charts/osm_gtfs" else "."
out_path_combined <- file.path(out_dir, "osm_bus_growth.png") # keeps same filename for markdown reference compatibility
ggsave(out_path_combined, plot = p_combined, width = 8, height = 5, dpi = 300)
cat("Combined plot successfully saved to", out_path_combined, "\n")


# ----------------- PLOT 2: Growth by public transit mode -----------------
# Reshape for long format plotting


df_long <- df_combined %>%
    select(date, starts_with("route_")) %>%
    pivot_longer(cols = starts_with("route_"), names_to = "route_type", values_to = "relations") %>%
    mutate(route_type = sub("route_", "", route_type)) %>%
    filter(relations > 0) # filter out 0 values for log scale compatibility

# Custom color palette for route types
custom_colors <- c(
  "bus" = "#002B41",
  "trolleybus" = "#50ab77",
  # "minibus" = "#AF7E04",# No data
  "share_taxi" = "#bd2416",
  "train" = "#009de0",
  "light_rail" = "#b01980",
  "subway" = "#0374b4",
  "tram" = "#46555f",
  #"monorail" = "#86135a", # no data
  "ferry" = "#952511"
  # "funicular" = "#AF7E04" # No data
)

p_modes <- ggplot(df_long, aes(x = date, y = relations, color = route_type)) +
    geom_line(linewidth = 1) +
    scale_y_log10(labels = comma, breaks = 10^(0:6)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = custom_colors) +
    labs(
        title = "Growth of Public Transit Route Relations in OpenStreetMap by Mode",
        subtitle = "Historical growth of mapped transit routes (logarithmic scale)",
        x = "Year",
        y = "Total Relations (Log Scale)",
        color = "Route Type"
        # caption = paste("Data source: OpenStreetMap Taginfo API | Date until:", max(df_combined$date))
    ) +
    theme_minimal(base_family = "sans") +
    theme(
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 8)),
        plot.subtitle = element_text(size = 9, color = "gray30", margin = margin(b = 12)),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
        # plot.caption = element_text(size = 7, color = "gray50", margin = margin(t = 12))
    )


out_path_modes <- file.path(out_dir, "osm_transit_modes_growth.png")
ggsave(out_path_modes, plot = p_modes, width = 8, height = 5, dpi = 300)
cat("Breakdown by mode plot successfully saved to", out_path_modes, "\n")


# ----------------- PLOT 3: Growth by public transit mode (linear scale) -----------------
p_modes_linear <- ggplot(df_long, aes(x = date, y = relations, color = route_type)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_color_manual(values = custom_colors) +
    labs(
        title = "Growth of Public Transit Route Relations in OpenStreetMap by Mode",
        subtitle = "Historical growth of mapped transit routes (linear scale)",
        x = "Year",
        y = "Total Relations",
        color = "Route Type"
        # caption = paste("Data source: OpenStreetMap Taginfo API | Date until:", max(df_combined$date))
    ) +
    theme_minimal(base_family = "sans") +
    theme(
        plot.title = element_text(face = "bold", size = 12, margin = margin(b = 8)),
        plot.subtitle = element_text(size = 9, color = "gray30", margin = margin(b = 12)),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
        # plot.caption = element_text(size = 7, color = "gray50", margin = margin(t = 12))
    )

out_path_modes_linear <- file.path(out_dir, "osm_transit_modes_growth_linear.png")
ggsave(out_path_modes_linear, plot = p_modes_linear, width = 8, height = 5, dpi = 300)
cat("Breakdown by mode (linear) plot successfully saved to", out_path_modes_linear, "\n")


# ----------------- CSV: Yearly summary of relations and weights -----------------
# Filter for the last day of each year
df_yearly <- df_combined %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>%
    filter(date == max(date)) %>%
    ungroup()

# Reshape and calculate percentage weight per year
df_yearly_summary <- df_yearly %>%
    select(year, starts_with("route_")) %>%
    pivot_longer(cols = starts_with("route_"), names_to = "mode", values_to = "relations") %>%
    mutate(mode = sub("route_", "", mode)) %>%
    group_by(year) %>%
    mutate(
        total_relations = sum(relations),
        percentage = if_else(total_relations > 0, round((relations / total_relations) * 100, 2), 0)
    ) %>%
    ungroup() %>%
    select(year, mode, relations, percentage)

out_path_csv <- file.path(out_dir, "osm_transit_yearly_summary.csv")
write.csv(df_yearly_summary, out_path_csv, row.names = FALSE)
cat("Yearly summary CSV successfully saved to", out_path_csv, "\n")
