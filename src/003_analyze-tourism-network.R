# Load library
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggmap)

# Load Edges Data
edgelist <- read_rds("data/edges.rds") %>%
  rename(source = Source, target = Target) %>%
  group_by(source, target)

# Membuat network
g <- graph_from_data_frame(edgelist, directed = FALSE)

# Pengukuran network secara keseluruhan
metrik_network <- tibble(jumlah_node = vcount(g), 
                         jumlah_edge = ecount(g), 
                         average_path_length = average.path.length(g), 
                         diameter = diameter(g), 
                         density = edge_density(g), 
                         modularity = modularity(cluster_louvain(g)))
metrik_network

# Mengukur metrik sentralitas
g_tidy <- as_tbl_graph(g) %>%
  mutate(degree_centrality = centrality_degree()) %>%
  mutate(betweenness_centrality = centrality_betweenness()) %>%
  mutate(eigen_centrality = centrality_eigen()) %>%
  mutate(modularity = group_louvain())

# Menampilkan hasil pengukuran sentralitas
g_tidy %>%
  arrange(desc(degree_centrality)) %>%
  as_tibble()