# working through demo (https://michaelgastner.com/DAVisR2021/chap-tidygraph.html)
# Fri Jul 01 09:47:50 2022 ------------------------------

library(tidyverse)


demo_nodes <-
  readxl::read_excel(here::here("input", "demo_data.xlsx"), sheet = "demo_nodes")
demo_edges <-
  readxl::read_excel(here::here("input", "demo_data.xlsx"), sheet = "demo_edges")

library(tidygraph)
demo_undirected_netw <-
  tbl_graph(nodes = demo_nodes, edges = demo_edges, directed = FALSE)

demo_undirected_netw


demo_undirected_netw %>%
  activate(edges) %>%
  as_tibble()

#---- visual
ggraph(demo_undirected_netw, layout = "kk") +
  geom_edge_link() +
  geom_node_label(aes(label = name))
