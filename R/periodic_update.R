packages <- c("tidyverse","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())

# 1. Functions -----------------------------------------------------------------

# retrieving stats list from ECOS

getEcosList <- function(ECOS_key=ECOS_key){
  url <- paste0("https://ecos.bok.or.kr/api/StatisticTableList/",ECOS_key,"/json/kr/1/1000/")
  html <- httr::GET(url)
  res <- rawToChar(html$content)
  Encoding(res) <- "UTF-8"
  json_all <- fromJSON(res)
  
  if (!is.null(json_all$RESULT)){
    code <- json_all$RESULT$CODE	
    msg  <- json_all$RESULT$MESSAGE	
    stop(paste0(code, "\n ", msg))
    
  }
  json_all[[1]][[2]] %>%  tibble::as_tibble() %>% 
    dplyr::select(통계표코드=STAT_CODE, 
                       상위통계표코드=P_STAT_CODE,
                       통계명=STAT_NAME, 
                       주기=CYCLE,출처=ORG_NAME, 
                       검색가능여부=SRCH_YN)
}

# retrieving stats details from ECOS

getEcosCode <- function(ECOS_key=ECOS_key,STAT_CODE){
  url <- paste0("http://ecos.bok.or.kr/api/StatisticItemList/",ECOS_key,"/json/kr/1/1000/",STAT_CODE)
  html <- GET(url)
  res <- rawToChar(html$content)
  Encoding(res) <- "UTF-8"
  json_all <- fromJSON(res)
  
  if (!is.null(json_all$RESULT)){
    code <- json_all$RESULT$CODE	
    msg  <- json_all$RESULT$MESSAGE	
    stop(paste0(code, "\n ", msg))
    
  }
  json_all[[1]][[2]] %>% tibble::as_tibble() %>% 
    dplyr::select(통계표코드 = STAT_CODE,	
                       통계명 =	STAT_NAME	,
                       항목그룹코드 = GRP_CODE,	
                       항목그룹	= GRP_NAME,	
                       상위통계항목코드	= P_ITEM_CODE,	
                       통계항목코드	= ITEM_CODE,	
                       통계항목명	= ITEM_NAME,	
                       주기	= CYCLE,	
                       수록시작일자	= START_TIME,	
                       수록종료일자	= END_TIME,	
                       자료수	= DATA_CNT,	
                       가중치 = WEIGHT) 
}

ecosSearch <- function(x) {
  data <- readRDS("Output/EcosStatsList.rds")
  search <- data %>% transmute(search=str_c(통계명,통계항목명,sep=" ")) %>% pull()
  flag <- map(x, ~str_detect(search,.x)) %>% reduce(magrittr::multiply_by) %>% as.logical()
  data %>% filter(flag)
}


getEcosData <- function(ECOS_key, stat_code, period, start_time, end_time, item_code1, 
                        item_code2, item_code3){
  
  url <- paste("http://ecos.bok.or.kr/api/StatisticSearch",ECOS_key,"json/kr/1/500",
               stat_code,period, start_time, end_time, item_code1, item_code2, item_code3,"", sep = "/")
  html <- GET(url)
  res <- rawToChar(html$content)
  Encoding(res) <- "UTF-8"
  json_all <- fromJSON(res)
  
  if (!is.null(json_all$RESULT)){
    
    code <- json_all$RESULT$CODE	
    msg  <- json_all$RESULT$MESSAGE	
    
    stop(paste0(code, "\n ", msg))
    
  }
  json_all[[1]][[2]] %>%  dplyr::select(TIME,DATA_VALUE)
}


# 2. Script --------------------------------------------------------------------

ECOS_key <- Sys.getenv(x="ECOS_key")

# EcosTable <- getEcosList(ECOS_key = ECOS_key) %>% as_tibble()
# 
# CODE_LIST <- EcosTable %>% filter(검색가능여부=="Y") %>% pull(통계표코드)
# 
# CODE_LIST <- CODE_LIST[1:10]
# 
# safe_getEcosCode <-  safely(.f=function(x) getEcosCode(ECOS_key = ECOS_key, STAT_CODE = x))
# 
# EcosStatsList <- map(CODE_LIST, safe_getEcosCode) %>% 
#                   transpose() %>% 
#                   pluck("result") %>% 
#                   bind_rows() %>% 
#                   arrange(통계명,통계항목코드)
# saveRDS(EcosStatsList,"Output/EcosStatsList.rds")


data_code <- ecosSearch(c("거시경제분석 지표")) %>% 
  filter(자료수 >0) %>% 
  pull(통계항목코드) %>% unique()

data_name <- ecosSearch(c("거시경제분석 지표")) %>% 
  filter(자료수 >0) %>% 
  pull(통계항목명)

safe_getEcosData <- safely(.f=function(x) getEcosData(ECOS_key = ECOS_key, 
                                                      stat_code="901Y001", 
                                                      period="MM", 
                                                      start_time="19901", 
                                                      end_time=today() %>% format(format="%Y%m"), 
                                                      item_code1=x, 
                                                      item_code2="?", 
                                                      item_code3="?"))


ecos_macro_raw <- map(data_code, safe_getEcosData) %>% 
                    transpose() %>% 
                    pluck("result") 

if(detect_index(ecos_macro_raw,is.null)>0) data_name <- data_name[-detect_index(ecos_macro_raw,is.null)] 


ecos_macro <- ecos_macro_raw %>% 
                compact() %>% 
                reduce(left_join, by="TIME") %>% 
                set_names(c("yearM",data_name)) %>% 
                as_tibble() %>% 
                mutate(yearM=as.yearmon(yearM,format="%Y%m")) %>% 
                mutate(across(.cols=-1,.fns=as.numeric))


saveRDS(ecos_macro,"Output/ecos_macro.rds")

