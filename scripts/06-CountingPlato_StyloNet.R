library(stylo)
library(igraph)
library(ggraph)
library(paletteer)
library(tidyverse)
library(graphlayouts)
load("./data/dtms.Rdata")

# dtm
dtm600 <- dtms[[11]]

# stylo network
stylo(network = TRUE,
      frequencies = dtm600,
      mfw.min = 100,
      mfw.max = 600,
      mfw.incr = 50,
      network.type="undirected",
      network.tables="both",
      linked.neighbors=3,
      edge.weights="linear",
      distance.measure = "wurzburg",
      gui=FALSE)

my_csv <- list.files(pattern = "csv")
my_csv

corpus_edges <- read_csv(my_csv[2])
corpus_edges

corpus_nodes <- read_csv(my_csv[3])
corpus_nodes

net_data <- corpus_edges |> 
  left_join(corpus_nodes, 
            by = join_by(Source == Id)) |> 
  dplyr::select(-Source) |> 
  rename(Source = Label) |> 
  relocate(Source, .before = Target) |> 
  left_join(corpus_nodes, 
            by = join_by(Target == Id)) |> 
  dplyr::select(-Target) |> 
  rename(Target = Label) |> 
  relocate(Target, .after = Source) |> 
  dplyr::select(Source, Target, Weight) |> 
  filter(Weight > 6) |> 
  rename(weight = Weight)

net_data 

# plot graph
corpus_graph <- graph_from_data_frame(net_data, directed = TRUE)
corpus_graph <- as_undirected(corpus_graph, mode = "collapse",
                              edge.attr.comb = list(weight = "mean"))


# центральности
V(corpus_graph)$weighted_degree <- strength(corpus_graph)
V(corpus_graph)$degree <- igraph::degree(corpus_graph)
V(corpus_graph)$eigen <- eigen_centrality(corpus_graph)$vector


# кластеры
cw <- cluster_walktrap(corpus_graph)
csg <- cluster_spinglass(corpus_graph)
cev <- cluster_leading_eigen(corpus_graph)
fg <- cluster_fast_greedy(corpus_graph)


modularity(cw)
modularity(csg)
modularity(cev)
modularity(fg)

clus_tbl <- tibble(title = cw$name, 
                   cluster = cw$membership)
V(corpus_graph)$membership <- as.factor(clus_tbl$cluster)

communities(cw)  

# граф
set.seed(21122024)
ggraph(corpus_graph, layout = "nicely") +
  geom_edge_arc(aes(linewidth = weight),
                alpha = 0.5,
                curvature = 0.2,
                show.legend = FALSE,
                color = "grey80") +
  geom_node_point(aes(shape = membership,
                      fill = membership,
                      size = degree),
                  #alpha = 0.5,
                  show.legend = FALSE) +
  geom_node_label(aes(label = name),
                  vjust = -0.75,
                  cex = 2,
                  #alpha = 0.8,
                  show.legend = FALSE,
                  #fontface = "bold"
                  ) +
  labs(x = NULL, y = NULL) + 
  scale_shape_manual("", values = c(21, 24, 21, 23, 23, 22, 25)) +
  scale_fill_manual("", values = c("grey50", "grey50", "white", "grey50", "white", "white", "white")) +
  scale_size_continuous(range = c(3,9)) +
  theme_void() 

ggsave(filename = "img6.png",
       width = 10, 
       height = 7,
       units = "in", 
       bg = "white")


# отдельный диалог
net_data |> 
  filter(str_detect(Source, "Iust")) |> 
  arrange(-weight)

#zoom in
clus2 <- clus_tbl |>
  filter(cluster == 2) |>
  pull(title)

clus2_tbl <- net_data |> 
  filter(Source %in% clus2 & Target %in% clus2)

vert <- which(vertex_attr(corpus_graph)$name %in% clus2)

p <- induced_subgraph(corpus_graph, vids = vert)


set.seed(22122024)
ggraph(p, layout = "nicely") +
  geom_edge_arc(aes(linewidth = weight,
                    filter = edge_attr(p)$weight > 12),
                alpha = 0.5,
                curvature = 0.2,
                show.legend = FALSE,
                color = "grey80") +
  geom_node_point(aes(size = weighted_degree),
                  show.legend = FALSE,
                  shape = 21,
                  fill = "white", 
                  color = "grey30") +
  geom_node_label(aes(label = name),
                  vjust = -0.6,
                  cex = 2.5,
                  color = "grey20",
                  show.legend = FALSE) +
  labs(x = NULL, y = NULL) + 
  theme_void() 

ggsave(filename = "img7.png",
       width = 6, 
       height = 6,
       units = "in", 
       bg = "white")
