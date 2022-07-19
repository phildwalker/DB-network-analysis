# Building out the node tables to do network analysis on
# Mon Jun 27 14:01:26 2022 ------------------------------

library(tidyverse)


#read in tables--------
allTang <- readxl::read_excel(here::here("input", "TangStats - Database Schema Information (06-20-2022).xlsx"),
                              sheet = "KeyColumnUsage") %>% 
  select(CONSTRAINT_NAME, TABLE_NAME) %>% 
  filter(!str_detect(CONSTRAINT_NAME, "IX_|PK_|UX_")) %>% 
  mutate(ToSplit = CONSTRAINT_NAME,
         ToSplit = str_remove_all(ToSplit, "FK_|fk_")) %>%  
  separate(ToSplit, into = c("Tb1", "Tb2"), sep = "_") %>% 
  mutate(Tbl1Check = str_detect(Tb1, "tbl"),
         Tbl2Check = str_detect(Tb2, "tbl"),
         BothTBL = Tbl1Check + Tbl2Check) %>% 
  filter(BothTBL == 2)

TangFull <- allTang %>% 
  select(from = Tb1,
         to = Tb2) %>% 
  distinct() %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  group_by(from, to) %>% 
  summarise(weight = n()) %>% 
  ungroup()



save(TangFull, file = here::here("output", "TangFull.rds"))


#---------- list of all the tangstats tables ---

allTangTable <- readxl::read_excel(here::here("input", "TangStats - Database Schema Information (06-20-2022).xlsx"),
                              sheet = "TableList") %>% 
  select(1:8) %>% 
  filter(TABLE_SCHEMA != "sys") %>% 
  select(label = TABLE_NAME, TABLE_TYPE, Category) %>% 
  rowid_to_column()


save(allTangTable, file = here::here("output", "allTangTable.rds"))


 #------ tables used in reports ----
ProdReports <- readxl::read_excel(here::here("input", "DBtables_used.xlsx"),
                                  sheet = "Sheet2") %>% 
  select(1:5) %>%
  janitor::clean_names() %>% 
  filter(!is.na(db_tables_used)) %>% 
  mutate(db_tables_used = str_remove_all(db_tables_used, "dbo."),
         x3 = str_remove_all(x3, "dbo.")) %>% 
  filter(x3 != db_tables_used) %>% 
  select(to = x3, from = db_tables_used) %>% 
  group_by(to, from) %>% 
  summarise(weight = n())


save(ProdReports, file = here::here("output", "ProdReports.rds"))



