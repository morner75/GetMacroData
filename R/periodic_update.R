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






