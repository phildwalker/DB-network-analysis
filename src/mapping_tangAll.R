# mapping the tangStats connection
# Fri Jul 01 09:00:56 2022 ------------------------------

library(tidyverse)
library(tidygraph)
library(ggraph)
# library(igraph)

load(file = here::here("output", "TangFull.rds"))
load(file = here::here("output", "allTangTable.rds"))

nodes <- 
  allTangTable %>% 
  select(id = rowid,
         label, TABLE_TYPE, Category) %>% 
  mutate(id = as.numeric(id),
         Category = ifelse(is.na(Category), TABLE_TYPE, Category))


edges <- TangFull %>% 
  rename(from_name = from, to_name = to) %>% 
  left_join(nodes, by = c("from_name" = "label")) %>% 
  rename(from = id) %>%
  left_join(nodes, by = c("to_name" = "label")) %>% 
  rename(to = id) %>% 
  select(from, to, weight) %>% 
  mutate(weight = as.numeric(weight)) %>% 
  filter(!is.na(to),
         !is.na(from))


#----------------


routes_tidy <- tbl_graph(nodes = nodes,
                         edges = edges,
                         directed = F)



#---- Plot graph ------

ggraph(routes_tidy, layout = "kk") +
  geom_edge_link() +
  geom_node_label(aes(label = label), size = 2)+
  theme_graph()#+theme_bw()





library(visNetwork)
# library(networkD3)


visNetwork(nodes %>% mutate(group = Category), 
           edges,
           main = "TangStats Table Network") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle") %>% 
  visLegend() %>% 
  visOptions(highlightNearest = T, selectedBy = "group")


#----Notde Analysis--------



summNodes <- routes_tidy %>% 
  mutate(NodeIso = node_is_isolated(),
         NetworkSize = node_effective_network_size(),
         LocSize = local_size()) %>% 
  activate(nodes) %>% 
  as_tibble()


summNodes %>% 
  count(NodeIso)


summNodes %>% 
  arrange(desc(LocSize)) %>% 
  head(10)
