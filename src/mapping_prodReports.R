# mapping out the links found in prod reports
# Fri Jul 01 10:14:50 2022 ------------------------------

library(tidyverse)
library(tidygraph)
library(ggraph)
# library(igraph)

load(file = here::here("output", "allTangTable.rds"))
load(file = here::here("output", "ProdReports.rds"))

nodes <- 
  allTangTable %>% 
  select(id = rowid,
         label, TABLE_TYPE, Category) %>% 
  mutate(id = as.numeric(id),
         Category = ifelse(is.na(Category), TABLE_TYPE, Category))


edges_rpt <- ProdReports %>% 
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


prod_rpts_tidy <- tbl_graph(nodes = nodes,
                            edges = edges_rpt,
                            directed = F)



#---- Plot graph ------

ggraph(prod_rpts_tidy, layout = "kk") +
  geom_edge_link() +
  geom_node_label(aes(label = label), size = 2)+
  theme_graph()#+theme_bw()





library(visNetwork)
# library(networkD3)


visNetwork(nodes %>% mutate(group = Category), 
           edges_rpt %>% rename(width = weight),
           main = "TangStats Production Reports") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle") %>% 
  visLegend() %>% 
  visOptions(highlightNearest = T)



#----- reviewing network properties ------

summNodesRPT <- prod_rpts_tidy %>% 
  mutate(NodeIso = node_is_isolated(),
         NetworkSize = node_effective_network_size(),
         LocSize = local_size()) %>% 
  activate(nodes) %>% 
  as_tibble()


summNodesRPT %>% 
  count(NodeIso)


summNodesRPT %>% 
  arrange(desc(LocSize)) %>% 
  head(10)



ProdReportsTest <- readxl::read_excel(here::here("input", "DBtables_used.xlsx"),
                                  sheet = "Sheet2") %>% 
  select(1:5) %>%
  janitor::clean_names() %>% 
  filter(!is.na(db_tables_used)) %>% 
  mutate(db_tables_used = str_remove_all(db_tables_used, "dbo."))


ProdReportsTest %>% 
  group_by(stored_procedure) %>% 
  summarise(nTbl = n()) %>% 
  ungroup() %>% 
  summarise(mean = mean(nTbl),
            median = median(nTbl)) %>% 
  ungroup()


ProdReportsTest %>% 
  count(db_tables_used, sort= T) %>% 
  ungroup()

