packages <- c("tidyverse","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/Functions.R")

ECOS_key <- Sys.getenv(x="ECOS_key")

# 1. Script --------------------------------------------------------------------

# ECOS STATS CODE LIST
EcosTable <- getEcosList(ECOS_key = ECOS_key,lang="en")

CODE_LIST <- EcosTable %>% filter(SRCH_YN=="Y") %>% pull(STAT_CODE)

safe_getEcosCode_kr <-  safely(.f=function(x) getEcosCode(ECOS_key = ECOS_key, STAT_CODE = x, lang="kr"))
safe_getEcosCode_en <-  safely(.f=function(x) getEcosCode(ECOS_key = ECOS_key, STAT_CODE = x, lang="en"))

EcosStatsList_kr <- map(CODE_LIST, safe_getEcosCode_kr) %>%
                      transpose() %>%
                      pluck("result") %>%
                      bind_rows() %>%
                      arrange(STAT_NAME,ITEM_CODE)

EcosStatsList_en <- map(CODE_LIST, safe_getEcosCode_en) %>%
                      transpose() %>%
                      pluck("result") %>%
                      bind_rows() %>%
                      arrange(STAT_NAME,ITEM_CODE)

key <- c('STAT_CODE','ITEM_CODE','DATA_CNT')
EcosStatsList <- union(
              select(EcosStatsList_kr, all_of(key)),
              select(EcosStatsList_en, all_of(key))) %>% 
              left_join(EcosStatsList_kr, by=key) %>% 
              left_join(EcosStatsList_en %>% 
                        select(all_of(key), STAT_NAME_EN=STAT_NAME,ITEM_NAME_EN=ITEM_NAME),
                        by=key)

saveRDS(EcosStatsList,"Rdata/EcosStatsList.rds")
write.csv(EcosStatsList,"Output/EcosStatsList.csv")


# KEY STATISTICS
KeyStats <- getKeyStats(ECOS_key=ECOS_key, lang="kr") %>% 
              bind_cols(getKeyStats(ECOS_key=ECOS_key, lang="en") %>% 
                           select(KEYSTAT_NAME_EN=KEYSTAT_NAME)) %>% 
              select(CLASS_NAME,KEYSTAT_NAME,KEYSTAT_NAME_EN,CYCLE,DATA_VALUE,UNIT_NAME)

saveRDS(KeyStats,"Rdata/KeyStats.rds")
write.csv(KeyStats,"Output/KeyStats.csv")


# ECOS MACRO DATA
data_code <- ecosSearch(c("Macro Economic Analysis")) %>% 
  filter(DATA_CNT >0) %>% select(STAT_CODE,ITEM_CODE,ITEM_NAME,ITEM_NAME_EN,CYCLE) %>% distinct()


safe_getEcosData_M <- safely(.f=function(x) getEcosData(ECOS_key = ECOS_key, 
                                                      stat_code="901Y001", 
                                                      period="MM", 
                                                      start_time="199001", 
                                                      end_time=today() %>% format(format="%Y%m"), 
                                                      item_code1=x, 
                                                      item_code2="?", 
                                                      item_code3="?"))

safe_getEcosData_Q <- safely(.f=function(x) getEcosData(ECOS_key = ECOS_key, 
                                                        stat_code="901Y001", 
                                                        period="QQ", 
                                                        start_time="19901", 
                                                        end_time=today() %>% format(format="%Y%q"), 
                                                        item_code1=x, 
                                                        item_code2="?", 
                                                        item_code3="?"))

safe_getEcosData_Y <- safely(.f=function(x) getEcosData(ECOS_key = ECOS_key, 
                                                        stat_code="901Y001", 
                                                        period="YY", 
                                                        start_time="1990", 
                                                        end_time=today() %>% format(format="%Y"), 
                                                        item_code1=x, 
                                                        item_code2="?", 
                                                        item_code3="?"))


ecos_macro_raw_M <- map(data_code$ITEM_CODE, safe_getEcosData_M) %>% 
  transpose() %>% 
  pluck("result") %>% 
  set_names(data_code$ITEM_CODE) %>% 
  compact()

ecos_macro_M <- ecos_macro_raw_M %>% 
  reduce(left_join, by="TIME") %>% 
  as_tibble() %>% 
  set_names(c("yearM",names(ecos_macro_raw_M))) %>% 
  mutate(yearM=as.yearmon(yearM,format="%Y%m")) %>% 
  mutate(across(.cols=-1,.fns=as.numeric))

ecos_macro_raw_Q <- map(data_code$ITEM_CODE, safe_getEcosData_Q) %>% 
  transpose() %>% 
  pluck("result") %>% 
  set_names(data_code$ITEM_CODE) %>% 
  compact()

ecos_macro_Q <- ecos_macro_raw_Q %>% 
  reduce(left_join, by="TIME") %>% 
  as_tibble() %>% 
  set_names(c("yearQ",names(ecos_macro_raw_Q))) %>% 
  mutate(yearM=as.yearqtr(yearQ,format="%Y%q")) %>% 
  mutate(across(.cols=-1,.fns=as.numeric))

ecos_macro_raw_Y <- map(data_code$ITEM_CODE, safe_getEcosData_Y) %>% 
  transpose() %>% 
  pluck("result") %>% 
  set_names(data_code$ITEM_CODE) %>% 
  compact()

ecos_macro_Y <- ecos_macro_raw_Y %>% 
  reduce(left_join, by="TIME") %>% 
  as_tibble() %>% 
  set_names(c("year",names(ecos_macro_raw_Y))) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(across(.cols=-1,.fns=as.numeric))


saveRDS(ecos_macro_M,"Rdata/ecos_macro_raw_M.rds")
saveRDS(ecos_macro_Q,"Rdata/ecos_macro_raw_Q.rds")
saveRDS(ecos_macro_Y,"Rdata/ecos_macro_raw_Y.rds")

