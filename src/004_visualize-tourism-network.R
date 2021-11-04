# Load library
library(tidyverse)
library(ggmap)

# Load Edges Data
edges <- read_rds("data/edges.rds") %>%
  rename(source = Source, target = Target) %>%
  group_by(source, target) %>%
  count() %>%
  ungroup() %>%
  rename(weight = n)

# Load Node Data
nodes <- read_rds("data/nodes.rds")

# Add Coordinate
edges <- merge(edges, nodes, by.x = "source", by.y = "label")
edges <- merge(edges, nodes, by.x = "target", by.y = "label")

# Open Bali Map
map_bali <- read_rds("data/map-bali.rds")

# Add Tourist Movement
network_kunjungan <- ggmap(map_bali) +
  geom_curve(
    data = edges, aes(
      x = lon.x,
      y = lat.x,
      xend = lon.y,
      yend = lat.y,
      color = place_type.y
    ),
    size = 0.3,
    alpha = 0.7,
    curvature = 0.4,
    na.rm = TRUE,
    arrow = arrow(
      length = unit(0.01, "npc"),
      type = "closed"
    )
  ) +
  geom_point(
    data = nodes,
    aes(
      x = lon,
      y = lat,
      color = place_type
    ),
    size = 1.5,
    alpha = 0.8,
    na.rm = TRUE
  ) +
  theme_void() +
  labs(color = "Type") +
  coord_equal()
network_kunjungan

# Save Plot
ggsave(network_kunjungan,
       filename = "plot/network_kunjungan.png",
       width = 15,
       height = 15,
       dpi = 300,
       type = "cairo",
       units = "cm",
       limitsize = FALSE
)
