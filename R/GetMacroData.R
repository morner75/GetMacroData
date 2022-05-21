packages <- c("tidyverse","xts","lubridate","zoo")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/MacroFilesProc.R")
source("R/Functions.R")

start_date <- "1990-01-01" %>% ymd()

current_date <- as.Date(Sys.time())
ECOS_key <- Sys.getenv(x="ECOS_key")


### Retrieving data

code_list <- readRDS("Rdata/ecos_code_list.rds")

INT_list <- str_c('INT_',c('CALL','CD91','CP91',"BOK1Y","KTB1Y","KTB2Y","KTB3Y","KTB5Y","KTB10Y",'AAm3Y','BBBm3Y'))


required_Q <-  c('USDKRW_Q','RGDP_Q','RGDP_QA','NGDP_Q','GOV_Q','GDP_DF_Q','UNEMP_QA',
                 'EMP_Q','EMP_QA','CPI_Q','CP_DEBT1_1_Q',
                 'CP_DEBT1_2_Q','HH_DEBT1_Q','CP_DEBT2_1_Q','CP_DEBT2_2_Q',
                 'HH_DEBT2_Q','CP_DEBT3_1_Q','CP_DEBT3_21_Q','CP_DEBT3_22_Q',
                 'HH_DEBT3_Q','OIL_Q','CU_Q','NI_Q','AL_Q','US_RGDP_QG','CHN_RGDP_QG',
                 'HH_MORT_Q','CP_INTCOVERAGE_Q','CP_DEBT2EQUITY_Q','EXT_DEBT_Q',
                 'CONSTRTN_QG', 'CRRNT_BAL_Q','EQUIP_INVEST_QG',  
                  str_c(INT_list,'_Q'))
                 
required_Y <-  c('USDKRW_Y','RGDP_Y','NGDP_Y','GOV_Y','GDP_DF_Y','UNEMP_Y','EMP_Y',
                 'CPI_Y','KOSPI_Y', 'HH_DI_Y','CP_DEBT1_1_Y',
                 'CP_DEBT1_2_Y','HH_DEBT1_Y','CP_DEBT2_1_Y','CP_DEBT2_2_Y',
                 'HH_DEBT2_Y','CP_DEBT3_1_Y','CP_DEBT3_21_Y','CP_DEBT3_22_Y',
                 'HH_DEBT3_Y','OIL_Y','CU_Y','NI_Y','AL_Y','US_RGDP_YG','CHN_RGDP_YG',
                 'HH_MORT_Y','HH_ASSET_NONFIN_Y','HH_ASSET_FIN_Y','CP_ASSET_Y',
                 'CP_CAPITAL_Y','EXT_DEBT_Q', str_c(INT_list,'_Y'))

required_M <-  c('USDKRW_M','HOUSE_M','KOSPI_M','OIL_M','CU_M','NI_M','ALL_M',
                 'GOV_BAL_M','RESID_PERMIT_M','FRGN_CRRNCY_RESRV_M',
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
