packages <- c("tidyverse","lubridate","zoo","openxlsx","xts")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)


rm(list=ls())

### 1. ECOSE data code list
read_csv("InputFiles/data_description.csv",col_names =TRUE) %>% 
  saveRDS(.,"Rdata/ecos_code_list.rds")



### 2. csv file based data

file_data <- list()

# KOSPI index (historical data before ECOS data begins) 
file_data[["KOSPI_M"]]<- read_csv("InputFiles/KOSPI.csv",col_names=TRUE) %>% 
                        transmute(yearM=as.yearmon(as.Date(date)),val=close) %>% 
                        group_by(yearM) %>% summarize(KOSPI_M=mean(val))

file_data[["KOSPI_Q"]]<- read_csv("InputFiles/KOSPI.csv",col_names=TRUE) %>% 
                            transmute(yearQ=as.yearqtr(as.Date(date)),val=close) %>% 
                            group_by(yearQ) %>% summarize(KOSPI_Q=mean(val))

file_data[["KOSPI_Y"]]<- read_csv("InputFiles/KOSPI.csv",col_names=TRUE) %>% 
                            transmute(year=year(as.Date(date)),val=close) %>% 
                            group_by(year) %>% summarize(KOSPI_Y=mean(val))


# MSCI index 
file_data[["MSCIW_M"]] <- read_csv("InputFiles/MSCIW.csv",col_names=TRUE) %>% 
                          mutate(yearM=as.yearmon(as.Date(Date,format="%Y.%m.%d")),.keep="unused") %>%
                          group_by(yearM) %>% summarize(MSCIW_M=mean(WORLD)) %>% ungroup() 

file_data[["MSCIW_Q"]] <- read_csv("InputFiles/MSCIW.csv",col_names=TRUE) %>% 
                              mutate(yearQ=as.yearqtr(as.Date(Date,format="%Y.%m.%d")),.keep="unused") %>%
                            group_by(yearQ) %>% summarize(MSCIW_Q=mean(WORLD)) %>% ungroup() 

file_data[["MSCIW_Y"]] <- read_csv("InputFiles/MSCIW.csv",col_names=TRUE) %>% 
                              mutate(year=year(as.Date(Date,format="%Y.%m.%d")),.keep="unused") %>%
                              group_by(year) %>% summarize(MSCIW_Y=mean(WORLD)) %>% ungroup() 



saveRDS(file_data,"Rdata/macro_file_data.rds")

