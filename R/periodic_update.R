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



# 2. Script --------------------------------------------------------------------

ECOS_key <- Sys.getenv(x="ECOS_key")

EcosTable <- getEcosList(ECOS_key = ECOS_key) %>% as_tibble()

CODE_LIST <- EcosTable %>% filter(검색가능여부=="Y") %>% pull(통계표코드)

CODE_LIST <- CODE_LIST[1:10]

safe_getEcosCode <-  safely(.f=function(x) getEcosCode(ECOS_key = ECOS_key, STAT_CODE = x))

EcosStatsList <- map(CODE_LIST, safe_getEcosCode) %>% 
                  transpose() %>% 
                  pluck("result") %>% 
                  bind_rows() %>% 
                  arrange(통계명,통계항목코드)

saveRDS(EcosStatsList,"Output/EcosStatsList.rds")

EcosStatsList %>% filter(str_detect(통계항목명,"고용률"))
