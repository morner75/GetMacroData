packages <- c("tidyverse","xts","lubridate","zoo","BIS","wbstats","Quandl")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/working/Functions_xts.R")

start_date <- "1990-01-01" %>% ymd() 

current_date <- as.Date(Sys.time())
ECOS_key <- Sys.getenv(x="ECOS_key")
QUANDL_key <- Sys.getenv(x="QUANDL_key")


### Retrieving data

code_list <- readRDS("Rdata/code_list.rds")
required.data <-  c("USDKRW","RGDP_Q","SARGDP_Q","NGDP_Q",
                    "GOV","GDP_DF","UNEMP","EMP",
                    "CPI","HOUSE","KOSPI","INT_CALL",
                    "INT_CD91","INT_KTB3Y","INT_KTB10Y","INT_AAm3Y",
                    "INT_BOK1Y","CP_DEBT1_1","CP_DEBT1_2","HH_DEBT1",
                    "CP_DEBT2_1","CP_DEBT2_2","HH_DEBT2",
                    "CP_DEBT3_1","CP_DEBT3_21","CP_DEBT3_22","HH_DEBT3",
                    "HH_DI")


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
} 

################################################################
#
# Part I. quarterly data
#
################################################################

quarterly.data <-  c("USDKRW","RGDP_Q","SARGDP_Q","NGDP_Q",
                     "GOV","GDP_DF","UNEMP","EMP",
                     "CPI","INT_CALL","INT_CD91","INT_KTB3Y",
                     "INT_KTB10Y","INT_AAm3Y","INT_BOK1Y")
StarsDataQ <- map(quarterly.data, ~levelXTS(DATA,.x,"QQ")) %>% do.call(merge,.)



## Monthly to quarterly data

# HOUSE
StarsDataQ <- levelXTS(DATA,"HOUSE","MM") %>% 
  period.apply(INDEX=endpoints(.,"quarters"),FUN=mean) %>% 
  merge(StarsDataQ,join="right")


# KOSPI

KOSPI1 <- levelXTS(DATA,"KOSPI","MM") %>% 
            period.apply(INDEX=endpoints(.,"quarters"),FUN=mean)
KOSPI2 <- readRDS("Rdata/file_data.rds")[["KOSPI"]]  %>% 
            mutate(date=ceiling_date(as.Date(yearQ),unit="quarter")-1) %>% 
            filter(date < min(index(KOSPI1)) & date > start_date) 
KOSPI2_xts <- as.xts(KOSPI2[,"KOSPI"], order.by=KOSPI2$date)
StarsDataQ  <- rbind(KOSPI1,KOSPI2_xts) %>% merge(StarsDataQ,join="right")

# Oil, Quantdl package (source: Federal Reserve Economic DATA)

oil <-  Quandl("FRED/WTISPLC", collapse="monthly",api_key=QUANDL_key) %>% 
          filter(Date >= start_date) %>% as_tibble() %>% set_names("Date","OIL")
StarsDataQ <- as.xts(oil[,-1],order.by=as.Date(oil$Date)) %>% 
           period.apply(INDEX=endpoints(.,"quarters"),FUN=mean) %>% 
           merge(StarsDataQ,join="right")
  
# MSCI index
MSCI_W <- readRDS("Rdata/file_data.rds")[["MSCI_W"]]  %>% 
            mutate(yearQ=ceiling_date(as.Date(yearQ),unit="quarter") -1) %>% 
            filter(yearQ >= start_date) 
MSCI_W_xts <- as.xts(MSCI_W[,-1],order.by=as.Date(MSCI_W$yearQ)) 

StarsDataQ <- merge(StarsDataQ,MSCI_W_xts,join="left")


##################################################################################3

# REER_BB, BIS package (Source: BIS)
url <- BIS::get_datasets() %>% slice(13) %>% pull(url)
REER <- BIS::get_bis(url) %>% filter(eer_type=="R", ref_area=="KR") %>% 
                select(date,eer_basket,obs_value) %>% 
                pivot_wider(names_from = eer_basket, values_from = obs_value) %>% 
                transmute(date=ceiling_date(ym(date),unit="months")-1,
                         REER_BB= B,
                         REER_BB_vol = makeVariable(REER_BB,type="vol",terms=12),
                         REER_NB= N,
                         REER_NB_vol=makeVariable(REER_NB,type="vol",terms=12))
REER_xts <- as.xts(REER[,-1],order.by=REER$date) %>% 
              na.fill(fill=0) %>% 
              period.apply(INDEX=endpoints(.,"quarters"),FUN = mean)

StarsDataQ <- merge(StarsDataQ,REER_xts,join="left")


# adding HH_DEBT

HH_DEBT.data <- c("HH_DEBT3","HH_DEBT2","HH_DEBT1")
map(HH_DEBT.data, ~levelXTS(DATA,.x,"QQ")) %>% do.call(merge,.)

HH_DEBT.data <- c("HH_DEBT3","HH_DEBT2","HH_DEBT1") 
StarsDataQ <- Obtain_Debt_Q(work_needed_data,DATA) %>% 
  set_names("yearQ","HH_DEBT") %>% 
  right_join(StarsDataQ,by="yearQ")

# adding CP_DEBT
work_needed_data1 <- c("CP_DEBT3_1","CP_DEBT2_1","CP_DEBT1_1") 
work_needed_data2 <- c("CP_DEBT3_21","CP_DEBT3_22","CP_DEBT2_2","CP_DEBT1_2")

CP_DEBT1 <- Obtain_Debt_Q(work_needed_data1,DATA) %>% set_names(c("yearQ","CP_DEBT1"))
CP_DEBT2 <- map(work_needed_data2, ~levelCleansing(DATA,.x,"QQ")) %>% reduce(full_join,by="yearQ") %>%
  mutate(CP_DEBT3_2=CP_DEBT3_21+CP_DEBT3_22) %>% 
  mutate(CP_DEBT3_2=replace(CP_DEBT3_2,CP_DEBT3_2==0,NA),CP_DEBT2_2=replace(CP_DEBT2_2,CP_DEBT2_2==0,NA)) %>%
  arrange(yearQ) %>% mutate(ratio1=mean(CP_DEBT1_2/CP_DEBT2_2,na.rm=TRUE),
                           ratio2=mean(CP_DEBT2_2/CP_DEBT3_2,na.rm=TRUE)) %>% 
  mutate(CP_DEBT3_2=CP_DEBT3_2*ratio1*ratio2,CP_DEBT2_2=CP_DEBT2_2*ratio1,
         CP_DEBT2=case_when(is.na(CP_DEBT1_2) & is.na(CP_DEBT2_2) ~ CP_DEBT3_2,
                            is.na(CP_DEBT1_2) ~ CP_DEBT2_2,
                            TRUE ~ CP_DEBT1_2)) %>% select(yearQ,CP_DEBT2) 

StarsDataQ <- CP_DEBT1 %>% left_join(CP_DEBT2) %>% 
  transmute(yearQ=yearQ,CP_DEBT=CP_DEBT1+CP_DEBT2) %>% 
  right_join(StarsDataQ,by="yearQ") %>% arrange(yearQ)


# Compute RGDP_Q_G, RGDP_Y, RGDP_Y_G, NGDP_Y, NGDP_Y_G
StarsDataQ <-  StarsDataQ %>% mutate(INT_CS=INT_AAm3Y-INT_KTB3Y,
                                     INT_TS=INT_KTB10Y-INT_CALL) %>% arrange(yearQ) 
saveRDS(StarsDataQ,"Rdata/StarsDataQ.rds")






