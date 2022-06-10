packages <- c("tidyverse","xts","lubridate","zoo","wbstats","openxlsx")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/Functions.R")

DATA <- readRDS("Rdata/ecos_data_raw.rds")

required.data <- names(DATA)


################################################################
#
# Part I. daily data
#
################################################################

# daily data from ECOS
daily.data <- str_subset(required.data,"_DG*$") %>% sort()
StarsDataD <- map(daily.data,~levelCleansingECOS(DATA,.x,"DD")) %>% 
  reduce(full_join,by="date") %>% 
  select(date,sort(names(.)))

################################################################
#
# Part III. monthly data
#
################################################################


# monthly data from ECOS
monthly.data <- str_subset(required.data,"_M[GA]*$") %>% sort()
StarsDataM <- map(monthly.data,~levelCleansingECOS(DATA,.x,"MM")) %>% 
  reduce(full_join,by="yearM") 


# KOSPI_M
StarsDataM <- rows_patch(StarsDataM,
                         readRDS("Rdata/macro_file_data.rds")[["KOSPI_M"]] %>% 
                           filter(yearM>=1990))
StarsDataM <- StarsDataD %>% 
  mutate(yearM=as.yearmon(date)) %>% 
  group_by(yearM) %>% 
  summarise(KOSPI_VOL_M=sd(KOSPI_D,na.rm=TRUE)) %>% 
  ungroup() %>% 
  right_join(StarsDataM,by="yearM")

# MSCIW
StarsDataM <- readRDS("Rdata/macro_file_data.rds")[["MSCIW_M"]] %>%
  filter(yearM>=1990) %>% 
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
  filter(year(yearM)>=1990) %>% 
  arrange(yearM)



StarsDataM <-  StarsDataM %>% 
  mutate(INT_CS_M = INT_AAm3Y_M-INT_KTB3Y_M,
         INT_TS_M = INT_KTB10Y_M-INT_CALL_M,
         KOSPI_MG = makeVariable(KOSPI_M,type="growth",terms=1),
         MSCIW_MG = makeVariable(MSCIW_M,type="growth",terms=1),
         HOUSE_MG= makeVariable(HOUSE_M,type="growth",terms=1),
         CPI_MG= makeVariable(CPI_M,type="growth",terms=12))

StarsDataM <- StarsDataM %>% 
  select(yearM,sort(names(.))) 

################################################################
#
# Part III. quarterly data
#
################################################################

quarterly.data <- str_subset(required.data,"_Q[GA)]*$") %>% sort()

StarsDataQ <- map(quarterly.data %>% 
                    str_subset(.,".*DEBT.*_Q$",negate = TRUE) %>% 
                    c(.,'EXT_DEBT_Q','CP_DEBT2EQUITY_Q'), 
                  ~levelCleansingECOS(DATA,.x,"QQ")) %>% 
  reduce(full_join,by="yearQ")


## Monthly to quarterly data

# KOSPI
StarsDataQ <- anti_join(readRDS("Rdata/macro_file_data.rds")[["KOSPI_Q"]],
                        mon2qtrECOS(DATA,"KOSPI_M") %>% rename(KOSPI_Q=KOSPI_M),
                        by=c("yearQ")) %>% 
  bind_rows(mon2qtrECOS(DATA,"KOSPI_M")%>% 
              rename(KOSPI_Q=KOSPI_M))  %>% 
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

# adding GOV_DEBT
work_needed_data <- str_subset(required.data,"GOV_DEBT._Q$") 
StarsDataQ <- Obtain_DebtQECOS(work_needed_data,DATA) %>% 
  set_names("yearQ","GOV_DEBT_Q") %>% 
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
            KOSPI_VOL_Q=sd(KOSPI_D/lag(KOSPI_D)*100-100,na.rm=TRUE)) %>% 
  right_join(StarsDataQ,by="yearQ")

# Residential permit, government balance (M to Q)
StarsDataQ <- StarsDataM %>% 
  mutate(yearQ=as.yearqtr(yearM)) %>%
  group_by(yearQ) %>% 
  summarise(GOV_BAL_Q=sum(GOV_BAL_M),
            RESID_PERMIT_Q=sum(RESID_PERMIT_M)) %>% 
  right_join(StarsDataQ,by="yearQ")




# Compute INT_CS_Q, INT_TS_Q, KOSPI_QG(stock return) (Q to Q)
StarsDataQ <-  StarsDataQ %>% 
  mutate(INT_CS_Q = INT_AAm3Y_Q-INT_KTB3Y_Q,
         INT_TS_Q = INT_KTB10Y_Q-INT_CALL_Q,
         KOSPI_QG = makeVariable(KOSPI_Q,type="growth",terms=1),
         MSCIW_QG = makeVariable(MSCIW_Q,type="growth",terms=1),
         INT_RKTB10Y_Q = INT_KTB10Y_Q - GDP_DF_Q,
         HOUSE_QG= makeVariable(HOUSE_Q,type="growth",terms=1),
         RGDP_QG= makeVariable(RGDP_Q,type="growth",terms=4),
         NGDP_QG= makeVariable(NGDP_Q,type="growth",terms=4),
         DEBT_Q= HH_DEBT_Q + CP_DEBT_Q,
         DEBT_QG= makeVariable(DEBT_Q,type='growth',terms = 1),
         CP_DEBT_QG= makeVariable(CP_DEBT_Q,type='growth',terms = 1),
         HH_DEBT_QG= makeVariable(HH_DEBT_Q,type='growth',terms = 1),
         DEBT2GDP_Q = DEBT_Q/RGDP_Q,
         HOUSE2INCOME_Q = HOUSE_Q/NGDP_Q,
         HH_MORT_QG=makeVariable(HH_MORT_Q,type='growth',terms = 1),
         CP_DEBT2GDP_Q=CP_DEBT_Q/RGDP_Q,
         HH_DEBT2GDP_Q=HH_DEBT_Q/RGDP_Q,
         CPI_QG=makeVariable(CPI_Q,type="growth",terms=4),
         EXT_DEBT_QG=makeVariable(EXT_DEBT_Q,type="growth",terms=1),
         RESID_PERMIT_QG=makeVariable(RESID_PERMIT_Q,type="growth",terms=4),
         GOV_QG=makeVariable(GOV_Q,type="growth",terms=4)
  ) %>% 
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

# KOSPI_VOL
StarsDataY <- StarsDataD %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarise(KOSPI_VOL_Y=sd(KOSPI_D,na.rm=TRUE)) %>% 
  ungroup() %>% 
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

# adding GOV_DEBT
work_needed_data <- str_subset(required.data,"GOV_DEBT._Y$") 
StarsDataY <- Obtain_DebtYECOS(work_needed_data,DATA) %>% 
  set_names("year","GOV_DEBT_Y") %>% 
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

# Interest coverage ratio

StarsDataY <- StarsDataQ %>% 
  mutate(year=year(yearQ)) %>% 
  group_by(year) %>% 
  summarise(CP_INTCOVERAGE_Y=mean(CP_INTCOVERAGE_Q,na.rm=TRUE),
            CP_DEBT2EQUITY_Y=mean(CP_DEBT2EQUITY_Q,na.rm=TRUE)) %>% 
  ungroup() %>% 
  right_join(StarsDataY,by="year") 


# bond return volatility, stock return volatility (D to Y)
StarsDataY <- StarsDataD %>% 
  mutate(year=year(date)) %>%
  group_by(year) %>% 
  summarise(INT_KTB3Y_VOL_Y=sd(INT_KTB3Y_D,na.rm = TRUE)) %>% 
  right_join(StarsDataY,by="year")

# Residential permit, government balance (M to Y)
StarsDataY <- StarsDataM %>% 
  mutate(year=year(yearM)) %>%
  group_by(year) %>% 
  summarise(GOV_BAL_Y=sum(GOV_BAL_M,na.rm=TRUE),
            REER_NB_VOL_Y=sd(REER_NB_M,na.rm=TRUE),
            REER_BB_VOL_Y=sd(REER_BB_M,na.rm=TRUE),
            MSCIW_VOL_Y=sd(MSCIW_MG,na.rm=TRUE),
            RESID_PERMIT_Y=sum(RESID_PERMIT_M,na.rm=TRUE),
            INT_BASE_Y=mean(INT_BASE_M,na.rm=TRUE)) %>% 
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
# Part V. Data augmentation with ecos_macro
#
################################################################

ecos_macro <- readRDS("Rdata/ecos_macro.rds")

macroDataM <- macroDataQ <- macroDataY <- NULL

macroDataM  <- ecos_macro %>% pluck("monthly") %>% 
  pivot_wider(names_from = vars) %>% 
  left_join(StarsDataM,by="yearM")



var_M1 <- paste0(c('AL','CORN','CU','FOREIGN_RESERVES','GOLD',"CN_HANGSENG",
                  'NET_TERMS_TRADE','NI','OIL','US_DOWJONES','US_NASDAQ'),'_M')
var_M2 <- paste0(c('US_CPI','RESID_PERMIT',"CRRNT_ACC","FIN_ACC","GOODS_BAL","SERVICES_BAL"),'_M')

macroDataM <- macroDataM %>% 
  bind_cols(
      macroDataM %>% 
        select(all_of(var_M1)) %>% 
        mutate_all(.funs=~(.x/lag(.x)-1)*100) %>% 
        set_names(paste0(var_M1,"G"))
      ) %>% 
  bind_cols(
    macroDataM %>% 
      select(all_of(var_M2)) %>% 
      mutate_all(.funs=~(.x/lag(.x,12)-1)*100) %>% 
      set_names(paste0(var_M2,"G"))
  ) %>% 
  select(yearM,all_of(names(.) %>% sort()))

macroDataQ <- ecos_macro %>% pluck("quarterly") %>%
  bind_rows(
      ecos_macro %>% pluck("monthly") %>%
      mutate(yearQ=as.yearqtr(yearM)) %>%
      filter(str_detect(vars, "APARTMENT|APARTMENT_JEONSE|HOUSE_JEONSE|M1|M2")) %>%
      group_by(yearQ,vars) %>%
      summarise(value=mean(value,na.rm=TRUE), .groups="drop") %>% 
      mutate(vars=str_replace(vars,"_M","_Q")) 
  ) %>% 
  pivot_wider(names_from = vars) %>% 
  left_join(StarsDataQ,by="yearQ") %>% 
  select(yearQ,all_of(names(.) %>% sort())) 
  
var_Q1 <- paste0(c('AL','CORN','CU','FOREIGN_RESERVES','GOLD',"CN_HANGSENG",
                   'NET_TERMS_TRADE','NI','OIL','US_DOWJONES','US_NASDAQ',
                   'GOV_DEBT','IAIP','IMPORT','EXPORT','CN_EXPORT','CN_IMPORT',
                   'US_IAIP','US_EXPORT','US_IMPORT'),'_Q')
var_Q2 <- paste0(c('CN_CPI','US_CPI','CRRNT_ACC','GOV_BAL',
                   "FIN_ACC","GOODS_BAL","SERVICES_BAL"),'_Q')

macroDataQ <- macroDataQ %>% 
  bind_cols(
    macroDataQ %>% 
      select(all_of(var_Q1)) %>% 
      mutate_all(.funs=~(.x/lag(.x)-1)*100) %>% 
      set_names(paste0(var_Q1,"G"))
  ) %>% 
  bind_cols(
    macroDataQ %>% 
      select(all_of(var_Q2)) %>% 
      mutate_all(.funs=~(.x/lag(.x,4)-1)*100) %>% 
      set_names(paste0(var_Q2,"G"))
  ) %>% 
  select(yearQ,all_of(names(.) %>% sort()))


MtoY <- macroDataM %>% 
  pivot_longer(-yearM,names_to="NAME",values_to = "value") %>% 
  mutate(year=year(yearM)) %>% 
  filter(str_detect(NAME, "CN_HANGSENG|CORN|EURIBOR3M|GOLD|US_CPI|US_DOWJONES|US_LIBOR3M|US_NASDAQ|US_TBill6M|US_TBond30Y|US_TNote10Y|US_TNote5Y"),
         str_detect(NAME, "_MG",negate=TRUE)) %>%
  group_by(year,NAME) %>%
  summarise(value=mean(value,na.rm=TRUE), .groups="drop") %>% 
  mutate(NAME=str_replace(NAME,"_M","_Y")) %>% 
  pivot_wider(names_from=NAME,values_from = value)


QtoY_sum <- macroDataQ %>% 
  pivot_longer(-yearQ,names_to="NAME",values_to = "value") %>% 
  mutate(year=year(yearQ)) %>% 
  filter(str_detect(NAME, "CN_EXPORT|CN_IMPORT|EXPORT|IMPORT|US_EXPORT|US_IMPORT")) %>%
           group_by(year,NAME) %>%
           summarise(value=sum(value,na.rm=TRUE), .groups="drop") %>% 
           mutate(NAME=str_replace(NAME,"_Q","_Y")) %>% 
           pivot_wider(names_from=NAME,values_from = value)
         
QtoY_mean <- macroDataQ %>% 
   pivot_longer(-yearQ,names_to="NAME",values_to = "value") %>% 
   mutate(year=year(yearQ)) %>% 
   filter(str_detect(NAME, "US_IAIP|IAIP|CN_CPI")) %>%
            group_by(year,NAME) %>%
            summarise(value=mean(value,na.rm=TRUE), .groups="drop") %>% 
            mutate(NAME=str_replace(NAME,"_Q","_Y")) %>% 
            pivot_wider(names_from=NAME,values_from = value)

macroDataY <- ecos_macro %>% pluck("annual") %>% 
  pivot_wider(names_from = vars) %>% 
  left_join(StarsDataY,by="year") %>% 
  left_join(MtoY, by="year") %>%
  left_join(QtoY_sum,by="year") %>%
  left_join(QtoY_mean,by="year") %>%
  mutate(CP_DEBT2GDP_Y=CP_DEBT_Y/RGDP_Y,
         HH_DEBT2GDP_Y=HH_DEBT_Y/RGDP_Y) %>% 
  select(year,all_of(names(.) %>% sort()))

var_Y <- paste0(c('AL','CN_HANGSENG','CORN' ,'CP_DEBT','CU','CPI','CRRNT_ACC',
                  'FOREIGN_RESERVES','GOV_BAL',"FIN_ACC","GOODS_BAL",'GOLD',
                   'NET_TERMS_TRADE','NI','OIL','US_DOWJONES','US_NASDAQ',
                   'HH_DI','GOV_DEBT','HH_DEBT',
                   'US_CPI',"SERVICES_BAL",'RGDP','NGDP','MSCIW','GOV'),'_Y')

macroDataY <- macroDataY %>% 
  bind_cols(
    macroDataY %>% 
      select(all_of(var_Y)) %>% 
      mutate_all(.funs=~(.x/lag(.x)-1)*100) %>% 
      set_names(paste0(var_Y,"G"))
  ) %>% 
  select(year,all_of(names(.) %>% sort()))

list(quarterly=macroDataQ, annual=macroDataY, monthly=macroDataM,daily=StarsDataD) %>% saveRDS(.,"Output/macro_data.rds")



# data explanation
code_list <- readRDS('Rdata/ecos_code_list.rds')
StatsDetails <- readRDS('Rdata/EcosStatsList.rds')
StatsDescription <- code_list %>% 
  select(NAME=name,STAT_CODE=code,ITEM_CODE=sub1) %>% 
  mutate(PERIOD=str_extract(NAME,"_[YQMD][GA]?$") %>% 
           str_sub(2,2) %>% 
           factor(levels=c("Y","Q","M","D")),
         .before=NAME) %>%
  left_join(StatsDetails %>% 
              select(STAT_CODE,ITEM_CODE,STAT_NAME, ITEM_NAME,ITEM_NAME_EN),
            by=c("STAT_CODE","ITEM_CODE")) %>% 
  bind_rows(
    map_dfr(c("M","Q","Y"), ~{ecos_macro %>% 
        pluck("description") %>% 
        mutate(PERIOD=.x,.before=VAR)}) %>% 
      mutate(NAME=ifelse(str_sub(VAR,-2,-1)=="_G",
                         str_replace(VAR,"_G",str_c("_",PERIOD,"G")),
                         str_c(VAR,"_",PERIOD)))
  ) %>% 
  arrange(PERIOD,NAME)


################################################################
#
# Part VI. save data
#
################################################################


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


################################################################
