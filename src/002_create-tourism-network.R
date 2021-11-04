# Install Package
install.packages(ggmap)

# Load packages
library(tidyverse)
library(ggmap)

# Load data
df <- read_csv("data/sample-tripadvisor.csv")

# Fix Data
df_fix <- df %>%
  select(reviewer_link, place_name) %>%
  drop_na()

# Get Edge list
edgelist <- df_fix %>%
  group_by(reviewer_link) %>%
  filter(n() > 1) %>%
  split(.$reviewer_link) %>%
  map(., 2) %>%
  map(~ combn(.x, m = 2)) %>%
  map(~ t(.x)) %>%
  map_dfr(as_tibble) %>%
  filter(V1 != V2) %>%
  rename(Source = V1, Target = V2)

# Simpan Edgelist Data
write_rds(edgelist, "data/edges.rds")

# Get nodes
nodes <- df %>%
  select(place_name, place_type) %>%
  unique()

# Connect to Gmaps API
register_google(key = "___")

# Get Place Coordinate
nodes_coordinate <- mutate_geocode(nodes, place_name)

# Save as RDS
nodes_coordinate %>%
  rename(label = place_name) %>%
  write_rds("data/nodes.rds")
