packages <- c("tidyverse","xts","lubridate","zoo")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/MacroFilesProc.R")
source("R/Functions.R")

start_date <- "1990-01-01" %>% ymd()

current_date <- as.Date(Sys.time())
ECOS_key <- Sys.getenv(x="ECOS_key")


## 1. ECOS Macro Economic Data -------------------------------------------------

ecos_macro_M <- readRDS("Rdata/ecos_macro_raw_M.rds")
ecos_macro_Q <- readRDS("Rdata/ecos_macro_raw_Q.rds")
ecos_macro_Y <- readRDS("Rdata/ecos_macro_raw_Y.rds")
EcosStatsList <- readRDS("Rdata/EcosStatsList.rds")
variable_names <- read.csv("InputFiles/ecos_macro_variables_list.csv") %>% 
                  filter(USE=="O") %>% 
                  select(-USE)

tidy_ecos_macro_M <- ecos_macro_M %>% 
                      pivot_longer(-yearM,names_to='ITEM_CODE') %>% 
                      inner_join(variable_names,by="ITEM_CODE") %>% 
                      mutate(yearQ=as.yearqtr(yearM)) 

monthly_data_code <- tidy_ecos_macro_M %>%
                      drop_na(value) %>% 
                      group_by(yearQ,ITEM_CODE) %>% 
                      summarise(N=n()) %>% 
                      group_by(ITEM_CODE) %>% 
                      summarise(average=mean(N)) %>% 
                      filter(average>2) %>% 
                      pull(ITEM_CODE)

monthly_data <- tidy_ecos_macro_M %>% 
                select(-yearQ) %>% 
                filter(ITEM_CODE %in% monthly_data_code) %>% 
                mutate(NAME=ifelse(str_sub(NAME,-2,-1)=="_G",str_replace(NAME,"_G","_MG"),str_c(NAME,"_M"))) %>% 
                select(yearM,NAME, value)


tidy_ecos_macro_Q <- ecos_macro_Q %>% 
                    pivot_longer(-yearQ,names_to='ITEM_CODE') %>% 
                    inner_join(variable_names,by="ITEM_CODE") %>% 
                    mutate(yearQ=as.yearqtr(yearQ,format="%Y%q")) 

quarterly_data <- tidy_ecos_macro_Q %>% 
                   mutate(NAME=ifelse(str_sub(NAME,-2,-1)=="_G",str_replace(NAME,"_G","_QG"),str_c(NAME,"_Q"))) %>% 
                   select(yearQ,NAME, value)


tidy_ecos_macro_Y <- ecos_macro_Y %>% 
                      pivot_longer(-year,names_to='ITEM_CODE') %>% 
                      inner_join(variable_names,by="ITEM_CODE")  

annual_data <- tidy_ecos_macro_Y %>% 
                  mutate(NAME=ifelse(str_sub(NAME,-2,-1)=="_G",str_replace(NAME,"_G","_YG"),str_c(NAME,"_Y"))) %>% 
                  select(year,NAME, value)

description <- tidy_ecos_macro_M %>% select(NAME,ITEM_CODE) %>% distinct() %>% 
                  left_join(EcosStatsList %>% 
                              filter(str_detect(STAT_NAME_EN,"Macro Economic Analysis")) %>% 
                              select(STAT_CODE, ITEM_CODE, STAT_NAME, ITEM_NAME,ITEM_NAME_EN),
                            by="ITEM_CODE") 

ecos_macro <- list(annual=annual_data, quarterly=quarterly_data, monthly=monthly_data,description=description)
saveRDS(ecos_macro,'Rdata/ecos_macro.rds')

monthly_dataW <- monthly_data %>% pivot_wider(names_from = NAME,values_from = value)
quarterly_dataW <- quarterly_data %>% pivot_wider(names_from = NAME,values_from = value)
annual_dataW <- annual_data %>% pivot_wider(names_from = NAME,values_from = value)

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

INT_list <- str_c('INT_',c('CALL','CD91','CP91',"BOK1Y","KTB1Y","KTB2Y","KTB3Y","KTB5Y","KTB10Y",'AAm3Y','BBBm3Y'))


required_Q <-  c('USDKRW_Q','RGDP_Q','RGDP_QA','NGDP_Q','GOV_Q','GDP_DF_Q','UNEMP_QA',
                 'EMP_Q','EMP_QA','CPI_Q','CP_DEBT1_1_Q',
                 'CP_DEBT1_2_Q','HH_DEBT1_Q','CP_DEBT2_1_Q','CP_DEBT2_2_Q',
                 'HH_DEBT2_Q','CP_DEBT3_1_Q','CP_DEBT3_21_Q','CP_DEBT3_22_Q',
                 'HH_DEBT3_Q','OIL_Q','CU_Q','NI_Q','AL_Q','US_RGDP_QG','CHN_RGDP_QG',
                 'HH_MORT_Q','CP_INTCOVERAGE_Q','CP_DEBT2EQUITY_Q','EXT_DEBT_Q',
                  str_c(INT_list,'_Q'))
                 
required_Y <-  c('USDKRW_Y','RGDP_Y','NGDP_Y','GOV_Y','GDP_DF_Y','UNEMP_Y','EMP_Y',
                 'CPI_Y','KOSPI_Y', 'HH_DI_Y','CP_DEBT1_1_Y',
                 'CP_DEBT1_2_Y','HH_DEBT1_Y','CP_DEBT2_1_Y','CP_DEBT2_2_Y',
                 'HH_DEBT2_Y','CP_DEBT3_1_Y','CP_DEBT3_21_Y','CP_DEBT3_22_Y',
                 'HH_DEBT3_Y','OIL_Y','CU_Y','NI_Y','AL_Y','US_RGDP_YG','CHN_RGDP_YG',
                 'HH_MORT_Y','HH_ASSET_NONFIN_Y','HH_ASSET_FIN_Y','CP_ASSET_Y',
                 'CP_CAPITAL_Y','EXT_DEBT_Q', str_c(INT_list,'_Y'))

required_M <-  c('USDKRW_M','HOUSE_M','KOSPI_M','OIL_M','CU_M','NI_M','ALL_M',
                 'GOV_BAL_M','RESID_PERMIT_M',
                 str_c(INT_list,'_M'))

required_D <-  c('USDKRW_D','KOSPI_D', str_c(INT_list,'_D'))


required.data <- c(required_Y,required_Q,required_M,required_D)

DATA <- vector(mode="list",length=length(required.data)) %>% set_names(required.data)

for(i in required.data){
  code_info <- code_list %>% filter(name==i)
  DATA[[i]] <- getEcosData(ECOS_key=ECOS_key,
              stat_code=code_info[["code"]],
              period=code_info[["period"]],
              start_time=EcosTerm(start_date,code_info[["period"]]),
              end_time=EcosTerm(current_date-months(3),code_info[["period"]]),
              item_code1=code_info[["sub1"]],
              item_code2=code_info[["sub2"]],
              item_code3=code_info[["sub3"]]) %>% 
    mutate(DATA_VALUE=as.numeric(DATA_VALUE))
  Sys.sleep(0.1)
} 

saveRDS(DATA, "Rdata/ecos_data_raw.rds")
rm(list=ls())
