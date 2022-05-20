packages <- c("tidyverse","xts","lubridate","zoo","wbstats","Quandl","openxlsx")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/MacroFilesProc.R")
source("R/Functions.R")

start_date <- "1990-01-01" %>% ymd()

current_date <- as.Date(Sys.time())
ECOS_key <- Sys.getenv(x="ECOS_key")
QUANDL_key <- Sys.getenv(x="QUANDL_key")

### Retrieving data

code_list <- readRDS("Rdata/ecos_code_list.rds")

INT_list <- str_c('INT_',c('CALL','CD91','CP91',"BOK1Y","KTB1Y","KTB2Y","KTB3Y","KTB5Y","KTB10Y",'AAm3Y','BBBm3Y'))


required_Q <-  c('USDKRW_Q','RGDP_Q','RGDP_QA','NGDP_Q','GOV_Q','GDP_DF_Q','UNEMP_QA',
                 'EMP_Q','EMP_QA','CPI_Q','CP_DEBT1_1_Q',
                 'CP_DEBT1_2_Q','HH_DEBT1_Q','CP_DEBT2_1_Q','CP_DEBT2_2_Q',
                 'HH_DEBT2_Q','CP_DEBT3_1_Q','CP_DEBT3_21_Q','CP_DEBT3_22_Q',
                 'HH_DEBT3_Q','OIL_Q','CU_Q','NI_Q','AL_Q','US_RGDP_QG','CHN_RGDP_QG',
                 'HH_MORT_Q','CP_INTCOVERAGE_Q','CP_DEBT2EQUITY_Q','EXT_DEBT_Q',
                 'CONSTRTN_QG', 'CRRNT_BAL_QG','EQUIP_INVEST_QG',  
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

#required.data <- 'EXT_DEBT_Y'
                     
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





################################################################
#
# Part I. daily data
#
################################################################

# daily data from ECOS
daily.data <- str_subset(required.data,"_D$") %>% sort()
StarsDataD <- map(daily.data,~levelCleansingECOS(DATA,.x,"DD")) %>% 
               reduce(full_join,by="date") %>% 
               select(date,sort(names(.)))





################################################################
#
# Part III. monthly data
#
################################################################


# monthly data from ECOS
monthly.data <- str_subset(required.data,"_M$") %>% sort()
StarsDataM <- map(monthly.data,~levelCleansingECOS(DATA,.x,"MM")) %>% 
  reduce(full_join,by="yearM") 


# KOSPI_M
StarsDataM <- rows_patch(StarsDataM,
                         readRDS("Rdata/macro_file_data.rds")[["KOSPI_M"]] %>% 
                           filter(yearM>=1990))

# MSCIW
StarsDataM <- readRDS("Rdata/macro_file_data.rds")[["MSCIW_M"]] %>%
  filter(yearM>=as.yearmon(start_date)) %>% 
  right_join(StarsDataM,by="yearM") 


# REER 

bisDB <- getBISDB()  
bis_REER <- bisDB %>% 
  filter(name=="Effective exchange rate indices (monthly)") %>% 
  pull() %>% 
  downloadBISDB()

StarsDataM<-  bis_REER %>% 
              filter(str_detect(REF_AREA,"KR"),str_detect(Type,"Real")) %>% 
              select(Basket,contains("-")) %>% 
              mutate(Basket=recode(Basket, `Broad (60 economies)`="broad", `Narrow (27 economies)`="narrow")) %>% 
              pivot_longer(-Basket,names_to = "yearM",values_to = "REER") %>% 
              mutate(yearM=as.yearmon(yearM,format="%Y-%m")) %>% 
              drop_na() %>% 
              pivot_wider(names_from="Basket",values_from = REER) %>% 
              rename(REER_BB_M=broad,
                     REER_NB_M=narrow) %>% 
              right_join(StarsDataM,by="yearM") %>%
              filter(yearM>=as.yearmon(start_date)) %>% 
              arrange(yearM)


StarsDataM <- StarsDataM %>% 
               select(yearM,sort(names(.)))

################################################################
#
# Part III. quarterly data
#
################################################################

quarterly.data <- str_subset(required.data,"_QG*$") %>% sort()
  
StarsDataQ <- map(quarterly.data %>% 
                    str_subset(.,".*DEBT.*_Q$",negate = TRUE) %>% 
                    c(.,'EXT_DEBT_Q','CP_DEBT2EQUITY_Q'), 
                  ~levelCleansingECOS(DATA,.x,"QQ")) %>% 
              reduce(full_join,by="yearQ")


## Monthly to quarterly data

# KOSPI
StarsDataQ <- anti_join(readRDS("Rdata/macro_file_data.rds")[["KOSPI_Q"]],
                        mon2qtrECOS(DATA,"KOSPI_M") %>% rename(KOSPI_Q=KOSPI_M),by=c("yearQ")) %>% 
              bind_rows(mon2qtrECOS(DATA,"KOSPI_M")%>% rename(KOSPI_Q=KOSPI_M))  %>% 
              right_join(StarsDataQ,by="yearQ")

# HOUSE
StarsDataQ <- mon2qtrECOS(DATA,"HOUSE_M") %>%
              rename(HOUSE_Q=HOUSE_M) %>% 
              right_join(StarsDataQ,by="yearQ") %>% 
              arrange(yearQ)


# MSCI index from webpage
StarsDataQ <- readRDS("Rdata/macro_file_data.rds")[["MSCIW_M"]] %>%
              mutate(yearQ=as.yearqtr(yearM)) %>%
              group_by(yearQ) %>% 
              summarise(MSCIW_Q=mean(MSCIW_M)) %>% ungroup() %>% 
              right_join(StarsDataQ,by="yearQ") 

# REER from BIS
StarsDataQ <- StarsDataM %>% 
              mutate(yearQ=as.yearqtr(yearM)) %>% 
              group_by(yearQ) %>% 
              summarise(REER_BB_Q=mean(REER_BB_M),REER_NB_Q=mean(REER_NB_M)) %>% 
              ungroup() %>% 
              right_join(StarsDataQ,by="yearQ")

# adding HH_DEBT
work_needed_data <- str_subset(required.data,"HH_DEBT._Q$") 
StarsDataQ <- Obtain_DebtQECOS(work_needed_data,DATA) %>% 
  set_names("yearQ","HH_DEBT_Q") %>% 
  right_join(StarsDataQ,by="yearQ")

# adding CP_DEBT
work_needed_data1 <- str_subset(required.data,"CP_DEBT._1_Q$") 
work_needed_data2 <-  str_subset(required.data,"CP_DEBT._2.*_Q$")

CP_DEBT1_Q <- Obtain_DebtQECOS(work_needed_data1,DATA) %>% 
            set_names(c("yearQ","CP_DEBT1_Q"))

CP_DEBT2_Q <- map(work_needed_data2, ~levelCleansingECOS(DATA,.x,"QQ")) %>% 
            reduce(full_join,by="yearQ") %>%
            mutate(CP_DEBT3_2_Q=CP_DEBT3_21_Q+CP_DEBT3_22_Q) %>% 
            mutate(CP_DEBT3_2_Q=replace(CP_DEBT3_2_Q,CP_DEBT3_2_Q==0,NA),
                   CP_DEBT2_2_Q=replace(CP_DEBT2_2_Q,CP_DEBT2_2_Q==0,NA)) %>%
            arrange(yearQ) %>% 
            mutate(ratio1=mean(CP_DEBT1_2_Q/CP_DEBT2_2_Q,na.rm=TRUE),
                   ratio2=mean(CP_DEBT2_2_Q/CP_DEBT3_2_Q,na.rm=TRUE)) %>% 
            mutate(CP_DEBT3_2_Q=CP_DEBT3_2_Q*ratio1*ratio2,
                   CP_DEBT2_2_Q=CP_DEBT2_2_Q*ratio1,
                   CP_DEBT2_Q=case_when(is.na(CP_DEBT1_2_Q) & is.na(CP_DEBT2_2_Q) ~ CP_DEBT3_2_Q,
                                      is.na(CP_DEBT1_2_Q) ~ CP_DEBT2_2_Q,
                                      TRUE ~ CP_DEBT1_2_Q)) %>% 
            select(yearQ,CP_DEBT2_Q) 

StarsDataQ <- CP_DEBT1_Q %>% 
              left_join(CP_DEBT2_Q,by='yearQ') %>% 
              transmute(yearQ=yearQ,
                        CP_DEBT_Q=CP_DEBT1_Q+CP_DEBT2_Q) %>% 
              right_join(StarsDataQ,by="yearQ") %>% 
              arrange(yearQ)



# bond return volatility, stock return volatility (D to Q)
StarsDataQ <- StarsDataD %>% 
                mutate(yearQ=as.yearqtr(date)) %>%
                group_by(yearQ) %>% 
                summarise(INT_KTB3Y_VOL_Q=sd(INT_KTB3Y_D,na.rm = TRUE),
                          KOPSI_VOL_Q=sd(KOSPI_D/lag(KOSPI_D)*100-100,na.rm=TRUE)) %>% 
                right_join(StarsDataQ,by="yearQ")

# Residential permit, Foreing currency reserve, government balance (M to Q)
StarsDataQ <- StarsDataM %>% 
                mutate(yearQ=as.yearqtr(yearM)) %>%
                group_by(yearQ) %>% 
                summarise(FRGN_CRRNCY_RESRV_Q=sum(FRGN_CRRNCY_RESRV_M),
                          GOV_BAL_Q=sum(GOV_BAL_M),
                          RESID_PERMIT_Q=sum(RESID_PERMIT_M)) %>% 
                right_join(StarsDataQ,by="yearQ")




# Compute INT_CS_Q, INT_TS_Q, KOSPI_QG(stock return) (Q to Q)
StarsDataQ <-  StarsDataQ %>% 
                  mutate(INT_CS_Q = INT_AAm3Y_Q-INT_KTB3Y_Q,
                         INT_TS_Q = INT_KTB10Y_Q-INT_CALL_Q,
                         KOSPI_QG = makeVariable(KOSPI_Q,type="growth",terms=1),
                         INT_RKTB10Y_Q = INT_KTB10Y_Q - GDP_DF_Q,
                         HOUSE_QG= makeVariable(HOUSE_Q,type="growth",terms=1),
                         DEBT_Q= HH_DEBT_Q + CP_DEBT_Q,
                         DEBT_QG= makeVariable(DEBT_Q,type='growth',terms = 1),
                         DEBT2GDP_Q = DEBT_Q/RGDP_Q,
                         HOUSE2INCOME_Q = HOUSE_Q/NGDP_Q,
                         HH_MORT_QG=makeVariable(HH_MORT_Q,type='growth',terms = 1)) %>% 
                  arrange(yearQ) %>% 
                  select(yearQ,sort(names(.)))
    



################################################################
#
# Part II. annual data
#
################################################################


# annual data from ECOS
annual.data <- str_subset(required.data,"_YG*$") %>% sort()
StarsDataY <- map(annual.data %>% str_subset(".*DEBT.*_Y$",negate = TRUE), 
                  ~levelCleansingECOS(DATA,.x,"YY")) %>% 
              reduce(full_join,by="year") 

# REER from BIS
StarsDataY <- StarsDataM %>% 
                mutate(year=year(yearM)) %>% 
                group_by(year) %>% 
                summarise(REER_BB_Y=mean(REER_BB_M),REER_NB_Y=mean(REER_NB_M)) %>% 
                ungroup() %>% 
                right_join(StarsDataY,by="year")


# World Bank annual growth, wbstats package (Source: World Bank) 
StarsDataY <- wbstats::wb_data(indicator="NY.GDP.MKTP.KD.ZG", country = "WLD", start_date = 1960,end_date = year(Sys.Date())) %>% 
                transmute(year=date, WGDP_YG=as.vector(NY.GDP.MKTP.KD.ZG)) %>%
                right_join(StarsDataY,by="year") 


# HOUSE_Y
StarsDataY <- mon2yearECOS(DATA,"HOUSE_M") %>%
  rename(HOUSE_Y=HOUSE_M) %>% 
  right_join(StarsDataY,by="year") %>% 
  arrange(year)

# MSCIW_Y
StarsDataY <- readRDS("Rdata/macro_file_data.rds")[["MSCIW_M"]] %>%
  mutate(year=year(yearM)) %>%
  group_by(year) %>% 
  summarise(MSCIW_Y=mean(MSCIW_M)) %>% ungroup() %>%  
  right_join(StarsDataY,by="year") 

# adding HH_DEBT
work_needed_data <- str_subset(required.data,"HH_DEBT._Y$") 
StarsDataY <- Obtain_DebtYECOS(work_needed_data,DATA) %>% 
  set_names("year","HH_DEBT_Y") %>% 
  right_join(StarsDataY,by="year")

# adding CP_DEBT
work_needed_data1 <- str_subset(required.data,"CP_DEBT._1_Y$") 
work_needed_data2 <-  str_subset(required.data,"CP_DEBT._2.*_Y$")

CP_DEBT1_Y <- Obtain_DebtYECOS(work_needed_data1,DATA) %>% 
  set_names(c("year","CP_DEBT1_Y"))

CP_DEBT2_Y <- map(work_needed_data2, ~levelCleansingECOS(DATA,.x,"YY")) %>% 
  reduce(full_join,by="year") %>%
  arrange(year) %>% 
  mutate(CP_DEBT3_2_Y=CP_DEBT3_21_Y+CP_DEBT3_22_Y) %>% 
  mutate(ratio1=mean(CP_DEBT1_2_Y/CP_DEBT2_2_Y,na.rm=TRUE),
         ratio2=mean(CP_DEBT2_2_Y/CP_DEBT3_2_Y,na.rm=TRUE)) %>% 
  mutate(CP_DEBT3_2_Y=CP_DEBT3_2_Y*ratio1*ratio2,
         CP_DEBT2_2_Y=CP_DEBT2_2_Y*ratio1,
         CP_DEBT2_Y=case_when(is.na(CP_DEBT1_2_Y) & is.na(CP_DEBT2_2_Y) ~ CP_DEBT3_2_Y,
                              is.na(CP_DEBT1_2_Y) ~ CP_DEBT2_2_Y,
                              TRUE ~ CP_DEBT1_2_Y)) %>% 
  select(year,CP_DEBT2_Y) 

StarsDataY <- CP_DEBT1_Y %>% 
  left_join(CP_DEBT2_Y,by="year") %>% 
  transmute(year=year,
            CP_DEBT_Y=CP_DEBT1_Y+CP_DEBT2_Y) %>% 
  right_join(StarsDataY,by="year") 

# Interest coverage ration

StarsDataY <- StarsDataQ %>% 
                mutate(year=year(yearQ)) %>% 
                group_by(year) %>% 
                summarise(CP_INTCOVERAGE_Y=mean(CP_INTCOVERAGE_Q,na.rm=TRUE),
                          CP_DEBT2EQUITY_Y=mean(CP_DEBT2EQUITY_Q,na.rm=TRUE)) %>% 
              ungroup() %>% 
              right_join(StarsDataY,by="year") 
                          


# INT_CS_Y, INT_TS_Y
StarsDataY <-  StarsDataY %>% 
                mutate(INT_CS_Y = INT_AAm3Y_Y-INT_KTB3Y_Y,
                       INT_TS_Y = INT_KTB10Y_Y-INT_CALL_Y,
                       KOSPI_YG = makeVariable(KOSPI_Y,type="growth",terms=1),
                       INT_RKTB10Y_Y = INT_KTB10Y_Y - GDP_DF_Y,
                       HOUSE_YG= makeVariable(HOUSE_Y,type="growth",terms=1),
                       DEBT_Y= HH_DEBT_Y + CP_DEBT_Y,
                       DEBT_YG= makeVariable(DEBT_Y,type='growth',terms = 1),
                       DEBT2GDP_Y = DEBT_Y/RGDP_Y,
                       HOUSE2INCOME_Y = HOUSE_Y/NGDP_Y,
                       HH_MORT_YG=makeVariable(HH_MORT_Y,type='growth',terms = 1)) %>% 
  arrange(year) %>% 
  select(year,sort(names(.)))
                       
                       


################################################################
#
# Part V. save data
#
################################################################

list(quarterly=StarsDataQ, annual=StarsDataY, monthly=StarsDataM,daily=StarsDataD) %>% saveRDS(.,"Output/macro_data.rds")

# data explanation
StatsDetails <- readRDS('Rdata/EcosStatsList.rds')
StatsDescription <- code_list %>% 
              select(변수명=name,통계표코드=code,통계항목코드=sub1) %>% 
              mutate(주기=str_extract(변수명,"_[YQMD]G?$") %>% 
                          str_sub(2,2) %>% 
                         factor(levels=c("Y","Q","M","D")),
                     .before=변수명) %>%
              left_join(StatsDetails %>% 
                          select(통계표코드,통계항목코드,통계항목코드,통계명, 통계항목명),
                        by=c("통계표코드","통계항목코드")) %>% 
              arrange(주기,변수명)



wb <- createWorkbook()
addWorksheet(wb,"quarterly")
addWorksheet(wb,"annual")
addWorksheet(wb,"monthly")
addWorksheet(wb,"daily")
addWorksheet(wb,"description")
writeData(wb,"quarterly",StarsDataQ,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"annual",StarsDataY,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"monthly",StarsDataM,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"daily",StarsDataD,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"description",StatsDescription,startCol=1,startRow=1,rowNames=FALSE)
saveWorkbook(wb,"Output/macro_data.xlsx",overwrite = TRUE)



rm(list=ls())

################################################################

