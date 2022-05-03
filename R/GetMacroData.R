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
required.data <-  c('USDKRW_Q','USDKRW_Y','RGDP_Q','RGDP_Y','RGDP_QA','NGDP_Q',
                    'NGDP_Y','GOV_Q','GOV_Y','GDP_DF_Q','GDP_DF_Y','UNEMP_QA',
                    'UNEMP_Y','EMP_Q','EMP_QA','EMP_Y','CPI_Q','CPI_Y','HOUSE_M',
                    'KOSPI_M','KOSPI_Y','INT_CALL_Q','INT_CD91_Q','INT_KTB3Y_Q',
                    'INT_KTB10Y_Q','INT_AAm3Y_Q','INT_BOK1Y_Q','INT_CALL_Y','INT_CD91_Y',
                    'INT_KTB3Y_Y','INT_KTB10Y_Y','INT_AAm3Y_Y','INT_BOK1Y_Y',
                    'INT_CALL_M','INT_CD91_M','INT_KTB3Y_M','INT_KTB10Y_M','INT_AAm3Y_M',
                    'INT_BOK1Y_M','HH_DI_Y','CP_DEBT1_1_Q','CP_DEBT1_2_Q','HH_DEBT1_Q',
                    'CP_DEBT2_1_Q','CP_DEBT2_2_Q','HH_DEBT2_Q','CP_DEBT3_1_Q',
                    'CP_DEBT3_21_Q','CP_DEBT3_22_Q','HH_DEBT3_Q','CP_DEBT1_1_Y',
                    'CP_DEBT1_2_Y','HH_DEBT1_Y','CP_DEBT2_1_Y','CP_DEBT2_2_Y',
                    'HH_DEBT2_Y','CP_DEBT3_1_Y','CP_DEBT3_21_Y','CP_DEBT3_22_Y','HH_DEBT3_Y')


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
} 

################################################################
#
# Part I. quarterly data
#
################################################################

quarterly.data <- str_subset(required.data,"_Q$")
  
StarsDataQ <- map(quarterly.data %>% str_subset(".*DEBT.*_Q$",negate = TRUE), 
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

# Oil, Quantdl package (source: Federal Reserve Economic DATA)

StarsDataQ <- Quandl("FRED/WTISPLC", type="raw", collapse="quarterly", start_date=start_date, api_key=QUANDL_key) %>% 
              mutate(yearQ=as.yearqtr(as.character(quarter(ymd(Date),with_year=TRUE)),format="%Y.%q")) %>% 
              dplyr::select(yearQ,OIL_Q=Value)  %>% 
              right_join(StarsDataQ,by="yearQ")

# MSCI index from webpage
StarsDataQ <- readRDS("Rdata/macro_file_data.rds")[["MSCIW_M"]] %>%
              mutate(yearQ=as.yearqtr(yearM)) %>%
              group_by(yearQ) %>% 
              summarise(MSCIW_Q=mean(MSCIW_M)) %>% ungroup() %>% 
              right_join(StarsDataQ,by="yearQ") 

# REER from BIS

bisDB <- getBISDB()  
bis_REER <- bisDB %>% 
              filter(name=="Effective exchange rate indices (monthly)") %>% 
              pull() %>% 
              downloadBISDB()

REER_M <-  bis_REER %>% 
            filter(str_detect(`Reference area`,"KR"),str_detect(Type,"Real")) %>% 
            select(Basket,contains("-")) %>% 
            mutate(Basket=recode(Basket, `B:Broad (60 economies)`="broad", `N:Narrow (27 economies)`="narrow")) %>% 
            pivot_longer(-Basket,names_to = "yearM",values_to = "REER") %>% 
            mutate(yearM=as.yearmon(yearM,format="%Y-%m")) %>% 
            drop_na()

REER_BB_Q <- REER_M %>% 
              filter(Basket=="broad") %>%  
              transmute(yearQ=as.yearqtr(yearM),
                        REER_BB=REER) %>% 
              group_by(yearQ) %>% 
              summarise(REER_BB_Q=mean(REER_BB)) %>% 
              ungroup()


REER_NB_Q <- REER_M %>%   
              filter(Basket=="narrow") %>%  
              transmute(yearQ=as.yearqtr(yearM),
                        REER_NB=REER) %>% 
              group_by(yearQ) %>% 
              summarise(REER_NB_Q=mean(REER_NB)) %>% 
              ungroup()
                        

REER_BB_Y <- REER_M %>% 
              filter(Basket=="broad") %>%  
              transmute(year=year(yearM),
                        REER_BB=REER) %>% 
              group_by(year) %>% 
              summarise(REER_BB_Y=mean(REER_BB),
                        REER_BB_VOL_Y=sd(REER_BB)) %>% 
              ungroup()

REER_NB_Y <- REER_M %>%   
              filter(Basket=="narrow") %>%  
              transmute(year=year(yearM),
                        REER_NB=REER) %>% 
              group_by(year) %>% 
              summarise(REER_NB_Y=mean(REER_NB),
                        REER_NB_VOL_Y=sd(REER_NB)) %>% 
              ungroup()

REER_Y <- left_join(REER_NB_Y,REER_BB_Y,by="year")

StarsDataQ <- right_join(REER_BB_Q,REER_NB_Q, by="yearQ") %>%
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


# Compute INT_CS_Q, INT_TS_Q
StarsDataQ <-  StarsDataQ %>% 
                mutate(INT_CS_Q = INT_AAm3Y_Q-INT_KTB3Y_Q,
                       INT_TS_Q = INT_KTB10Y_Q-INT_CALL_Q) %>% 
                arrange(yearQ) 


################################################################
#
# Part II. annual data
#
################################################################


# annual data from ECOS
annual.data <- str_subset(required.data,"_Y$")
StarsDataY <- map(annual.data %>% str_subset(".*DEBT.*_Y$",negate = TRUE), 
                  ~levelCleansingECOS(DATA,.x,"YY")) %>% 
              reduce(full_join,by="year") 

# REER_BB from BIS

StarsDataY <- left_join(StarsDataY,REER_Y, by="year")


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
  right_join(StarsDataY,by="year") %>% 
  arrange(year)


# Oil, Quantdl package (source: Federal Reserve Economic DATA)

StarsDataY <- Quandl("FRED/WTISPLC", type="raw", collapse="annual", start_date=start_date, api_key=QUANDL_key) %>% 
  mutate(year=year(ymd(Date))) %>% 
  dplyr::select(year,OIL_Y=Value)  %>% 
  right_join(StarsDataY,by="year") %>% 
  arrange(year)


# INT_CS_Y, INT_TS_Y
StarsDataY <-  StarsDataY %>% 
                mutate(INT_CS_Y = INT_AAm3Y_Y-INT_KTB3Y_Y,
                       INT_TS_Y = INT_KTB10Y_Y-INT_CALL_Y) %>% 
                arrange(year) 


################################################################
#
# Part III. monthly data
#
################################################################


# monthly data from ECOS
monthly.data <- str_subset(required.data,"_M$")
StarsDataM <- map(monthly.data,~levelCleansingECOS(DATA,.x,"MM")) %>% 
  reduce(full_join,by="yearM") 


# MSCIW
StarsDataM <- readRDS("Rdata/macro_file_data.rds")[["MSCIW_M"]] %>%
              filter(yearM>=as.yearmon(start_date)) %>% 
              right_join(StarsDataM,by="yearM") 


# Oil, Quantdl package (source: Federal Reserve Economic DATA)

StarsDataM <- Quandl("FRED/WTISPLC", type="raw", collapse="monthly", start_date=start_date, api_key=QUANDL_key) %>% 
  mutate(yearM=as.yearmon(ymd(Date))) %>% 
  dplyr::select(yearM,OIL_M=Value)  %>% 
  right_join(StarsDataM,by="yearM") %>% 
  arrange(yearM)

# REER 

StarsDataM <- REER_M %>% 
                pivot_wider(names_from="Basket",values_from = REER) %>% 
                rename(REER_BB_M=broad,
                       REER_NB_M=narrow) %>% 
                right_join(StarsDataM,by="yearM") %>%
                filter(yearM>=as.yearmon(start_date)) %>% 
                arrange(yearM)
      

################################################################
#
# Part IV. save data
#
################################################################

list(quarterly=StarsDataQ, annual=StarsDataY, monthly=StarsDataM) %>% saveRDS(.,"Output/macro_data.rds")

wb <- createWorkbook()
addWorksheet(wb,"quarterly")
addWorksheet(wb,"annual")
addWorksheet(wb,"monthly")
writeData(wb,"quarterly",StarsDataQ,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"annual",StarsDataY,startCol=1,startRow=1,rowNames=FALSE)
writeData(wb,"monthly",StarsDataM,startCol=1,startRow=1,rowNames=FALSE)
saveWorkbook(wb,"Output/macro_data.xlsx",overwrite = TRUE)
rm(list=ls())

################################################################

