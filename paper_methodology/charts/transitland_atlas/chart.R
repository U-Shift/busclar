library(ggplot2)
library(dplyr)
library(scales)

# Dynamically resolve input/output path depending on current working directory
out_dir <- if (dir.exists("paper_methodology")) "paper_methodology/charts/transitland_atlas" else "."
csv_path <- file.path(out_dir, "feed_counts_by_year.csv")

if (!file.exists(csv_path)) {
  stop(paste("CSV file not found at:", csv_path))
}

# Read data
df <- read.csv(csv_path)

# Filter for years with actual feeds (Transitland Atlas starts having feeds from 2020 onwards)
df_filtered <- df %>%
  filter(total_feeds > 0) %>%
  arrange(year)

# Create the plot with professional styling matching the paper's figures
p <- ggplot(df_filtered, aes(x = year, y = total_feeds)) +
  # Area fill under the line for a premium, clean aesthetic
  geom_area(fill = "#0374b4", alpha = 0.15) +
  # Main line
  geom_line(color = "#0374b4", linewidth = 1.2) +
  # Points for each year
  geom_point(color = "#002B41", size = 3) +
  # Value labels above points
  geom_text(aes(label = comma(total_feeds)),
    vjust = -0.8,
    size = 3.5,
    fontface = "bold",
    color = "#002B41"
  ) +
  # Axis configuration
  scale_x_continuous(breaks = df_filtered$year) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.15))
  ) + # Extra space at top for labels
  labs(
    # title = "Growth of Transitland Atlas Feeds",
    # subtitle = "Total number of open transit feeds indexed by year (2020-2026)",
    x = "Year",
    y = "Total Feeds"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    # plot.title = element_text(face = "bold", size = 12, color = "#002B41", margin = margin(b = 6)),
    # plot.subtitle = element_text(size = 9.5, color = "gray30", margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", size = 10, color = "#002B41", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 10, color = "#002B41", margin = margin(r = 10)),
    axis.text = element_text(size = 9, color = "gray20"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(15, 15, 15, 15)
  )

# Save the plot
out_path <- file.path(out_dir, "feed_counts_by_year.png")
ggsave(out_path, plot = p, width = 7, height = 4.5, dpi = 300)
cat("Plot successfully saved to:", out_path, "\n")
