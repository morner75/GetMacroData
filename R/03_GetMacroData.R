packages <- c("tidyverse","xts","lubridate","zoo")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/02_MacroFilesProc.R")
source("R/Functions.R")

start_date <- "1990-01-01" %>% ymd()

current_date <- as.Date(Sys.time())
ECOS_key <- Sys.getenv(x="ECOS_key")


## 1. ECOS Macro Economic Data -------------------------------------------------

EcosStatsList <- readRDS("Rdata/EcosStatsList.rds")
data_list <- readRDS("Rdata/ecos_code_list2.rds") 

safe_getEcosData_M <- safely(.f=function(.x,.y,.z) getEcosData(ECOS_key = ECOS_key, 
                                                            stat_code=.x, 
                                                            period="M", 
                                                            start_time="199001", 
                                                            end_time=today() %>% format(format="%Y%m"), 
                                                            item_code1=.y, 
                                                            item_code2=.z, 
                                                            item_code3="?"))

safe_getEcosData_Q <- safely(.f=function(.x,.y,.z) getEcosData(ECOS_key = ECOS_key, 
                                                            stat_code=.x, 
                                                            period="Q", 
                                                            start_time="1990Q1", 
                                                            end_time=as.yearqtr(today()) %>% format(format="%YQ%q"), 
                                                            item_code1=.y, 
                                                            item_code2=.z, 
                                                            item_code3="?"))

safe_getEcosData_Y <- safely(.f=function(.x,.y,.z) getEcosData(ECOS_key = ECOS_key, 
                                                            stat_code=.x, 
                                                            period="A", 
                                                            start_time="1990", 
                                                            end_time=today() %>% format(format="%Y"), 
                                                            item_code1=.y, 
                                                            item_code2=.z, 
                                                            item_code3="?"))

data_list_M <- data_list %>% filter(CYCLE=="M")

ecos_macro_raw_M <- pmap(list(data_list_M$STAT_CODE, data_list_M$ITEM_CODE, data_list_M$ITEM_CODE2), safe_getEcosData_M) %>% 
  transpose() %>% 
  pluck("result") %>% 
  set_names(data_list_M$VAR) %>% 
  compact()

ecos_macro_M <- ecos_macro_raw_M %>% 
  reduce(left_join, by="TIME") %>% 
  as_tibble() %>% 
  set_names(c("yearM",names(ecos_macro_raw_M))) %>% 
  mutate(yearM=as.yearmon(yearM,format="%Y%m")) %>% 
  mutate(across(.cols=-1,.fns=as.numeric))

data_list_Q <- data_list %>% filter(CYCLE %in% c("M","Q"))
ecos_macro_raw_Q <- pmap(list(data_list_Q$STAT_CODE, data_list_Q$ITEM_CODE, data_list_Q$ITEM_CODE2), safe_getEcosData_Q) %>% 
  transpose() %>% 
  pluck("result") %>% 
  set_names(data_list_Q$VAR) %>% 
  compact()

ecos_macro_Q <- ecos_macro_raw_Q %>% 
  reduce(left_join, by="TIME") %>% 
  as_tibble() %>% 
  set_names(c("yearQ",names(ecos_macro_raw_Q))) %>% 
  mutate(yearQ=as.yearqtr(yearQ,format="%YQ%q")) %>% 
  mutate(across(.cols=-1,.fns=as.numeric))

ecos_macro_raw_Y <- pmap(list(data_list$STAT_CODE, data_list$ITEM_CODE, data_list$ITEM_CODE2), safe_getEcosData_Y) %>% 
  transpose() %>% 
  pluck("result") %>% 
  set_names(data_list$VAR) %>% 
  compact()

ecos_macro_Y <- ecos_macro_raw_Y %>% 
  reduce(left_join, by="TIME") %>% 
  as_tibble() %>% 
  set_names(c("year",names(ecos_macro_raw_Y))) %>% 
  mutate(year=as.numeric(year)) %>% 
  mutate(across(.cols=-1,.fns=as.numeric))


monthly_data  <- ecos_macro_M %>% 
                  pivot_longer(-yearM,names_to='vars') %>% 
                  select(yearM,vars,value) %>% 
                  mutate(vars=ifelse(str_sub(vars,-2,-1)=="_G",str_replace(vars,"_G","_MG"),str_c(vars,"_M")))


quarterly_data <- ecos_macro_Q %>% 
                    pivot_longer(-yearQ,names_to='vars') %>% 
                    select(yearQ,vars,value) %>% 
                    mutate(vars=ifelse(str_sub(vars,-2,-1)=="_G",str_replace(vars,"_G","_QG"),str_c(vars,"_Q")))

annual_data <- ecos_macro_Y %>% 
                    pivot_longer(-year,names_to='vars') %>% 
                    select(year,vars,value) %>% 
                    mutate(vars=ifelse(str_sub(vars,-2,-1)=="_G",str_replace(vars,"_G","_YG"),str_c(vars,"_Y")))


description <- data_list %>%  
                  left_join(EcosStatsList %>% 
                              select(STAT_CODE, ITEM_CODE, STAT_NAME, ITEM_NAME,STAT_NAME_EN,ITEM_NAME_EN),
                            by=c("STAT_CODE","ITEM_CODE")) 

ecos_macro <- list(annual=annual_data,quarterly=quarterly_data, monthly=monthly_data,description=description)
saveRDS(ecos_macro,'Rdata/ecos_macro.rds')

monthly_dataW <- monthly_data %>% pivot_wider(names_from = vars,values_from = value)
quarterly_dataW <- quarterly_data %>% pivot_wider(names_from = vars,values_from = value)
annual_dataW <- annual_data %>% pivot_wider(names_from = vars,values_from = value)


wb <- createWorkbook()
addWorksheet(wb,"monthly")
addWorksheet(wb,"quarterly")
addWorksheet(wb,"annual")
addWorksheet(wb,"description")
writeData(wb,"monthly",monthly_dataW,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"quarterly",quarterly_dataW,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"annual",annual_dataW,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"description",description,startCol=1,startRow=1,rowNames=FALSE)
saveWorkbook(wb,"Output/ecos_macro_data.xlsx",overwrite = TRUE)


## 2. Stress Test data ---------------------------------------------------------

# Retrieving data

code_list <- readRDS("Rdata/ecos_code_list.rds") 

required_Q <- code_list %>% filter(period=="Q") %>% pull(name)
required_Y <- code_list %>% filter(period=="A") %>% pull(name)
required_M <- code_list %>% filter(period=="M") %>% pull(name)
required_D <- code_list %>% filter(period=="D") %>% pull(name)


required.data <- c(required_Y,required_Q,required_M)
DATA <- vector(mode="list",length=length(required.data)) %>% set_names(required.data)

for(i in required.data){
  code_info <- code_list %>% filter(name==i)
  DATA[[i]] <- getEcosData(ECOS_key=ECOS_key,
              stat_code=code_info[["code"]],
              period=code_info[["period"]],
              start_time=EcosTerm(start_date,code_info[["period"]]),
              end_time=EcosTerm(current_date,code_info[["period"]]),
              item_code1=code_info[["sub1"]],
              item_code2=code_info[["sub2"]],
              item_code3=code_info[["sub3"]]) %>% 
    mutate(DATA_VALUE=as.numeric(DATA_VALUE))
  Sys.sleep(0.1)
} 

for(i in required_D){
  code_info <- code_list %>% filter(name==i)
  DATA[[i]] <- getEcosData(ECOS_key=ECOS_key,
                           stat_code=code_info[["code"]],
                           period=code_info[["period"]],
                           start_time=EcosTerm("2018-01-01",code_info[["period"]]),
                           end_time=EcosTerm(current_date,code_info[["period"]]),
                           item_code1=code_info[["sub1"]],
                           item_code2=code_info[["sub2"]],
                           item_code3=code_info[["sub3"]]) %>% 
    mutate(DATA_VALUE=as.numeric(DATA_VALUE))
  Sys.sleep(0.1)
} 

saveRDS(DATA, "Rdata/ecos_data_raw.rds")
rm(list=ls())
