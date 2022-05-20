##############################################################################
#
# Auxiliary Functions
#
##############################################################################

packages <- c("tidyverse","httr","jsonlite","tempdisagg","lubridate","zoo")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

##############################################################################
#
# Part 0. Basics
#
##############################################################################

## 1. Switch between quarter and date

yearQ2date <- function(yearQ) yq(yearQ) + period(3,units = "months") - days(1)
yearM2date <- function(yearM) ym(yearM) + months(1) - days(1) 
year2date <- function(year) ymd(str_c(year,"-12-31")) 


##############################################################################
#
# Part I. API functions
#
##############################################################################

## 1 ECOS API

# retrieving data from ECOS

getEcosData <- function(ECOS_key, stat_code, period, start_time, end_time, item_code1, 
                        item_code2, item_code3){
  
  url <- paste("http://ecos.bok.or.kr/api/StatisticSearch",ECOS_key,"json/kr/1/10000",
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


# retrieving stats list from ECOS

getEcosList <- function(ECOS_key=ECOS_key){
  url <- paste0("https://ecos.bok.or.kr/api/StatisticTableList/",ECOS_key,"/json/kr/1/1000/")
  html <- GET(url)
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




# ECOS API period setting
EcosTerm <- function(time,type){
  case_when(type=="MM" ~  as.yearmon(time) %>% format(.,"%Y%m"),
            type=="QQ" ~   as.yearqtr(time) %>% format(.,"%Y%q"),
            TRUE ~ as.character(year(time)))
}

getKeyStats <- function(ECOS_key=ECOS_key){
  url <- paste0("http://ecos.bok.or.kr/api/KeyStatisticList/",ECOS_key,"/json/kr/1/100/")
  html <- GET(url)
  res <- rawToChar(html$content)
  Encoding(res) <- "UTF-8"
  json_all <- fromJSON(res)

  if (!is.null(json_all$RESULT)){
    code <- json_all$RESULT$CODE
    msg  <- json_all$RESULT$MESSAGE
    stop(paste0(code, "\n ", msg))

  }
  json_all[[1]][[2]] %>%  tibble::as_tibble() %>%
    dplyr::select(통계그룹명=CLASS_NAME,
                       통계명=KEYSTAT_NAME,
                       시점=CYCLE,
                       값=DATA_VALUE,
                       단위=UNIT_NAME)
}


ecosSearch <- function(x) {
  data <- readRDS("Rdata/EcosStatsList.rds")
  search <- data %>% transmute(search=str_c(통계명,통계항목명,sep=" ")) %>% pull()
  flag <- map(x, ~str_detect(search,.x)) %>% reduce(magrittr::multiply_by) %>% as.logical()
  data %>% filter(flag) %>% distinct()
}

## 2. KSIS API

# FSIS serach: 회사코드, 통계항목 계정리스트

getFsisInfos <- function(api_key, info_name=c("companySearch","statisticsListSearch","accountListSearch"),
                         item_code="A"){
  
  if(info_name=="companySearch"){
    url <- paste0("http://fisis.fss.or.kr/openapi/",info_name,".json?lang=kr&auth=",
                  api_key,"&partDiv=",item_code)
  }else if(info_name=="statisticsListSearch"){
    url <- paste0("http://fisis.fss.or.kr/openapi/",info_name,".json?lang=kr&auth=",
                  api_key,"&lrgDiv=",item_code)
  }else{
    url <- paste0("http://fisis.fss.or.kr/openapi/",info_name,".json?lang=kr&auth=",
                  api_key,"&listNo=",item_code)
  }
  html <- GET(url)
  res <- rawToChar(html$content)
  Encoding(res) <- "UTF-8"
  json_all <- fromJSON(res)
  if (json_all$result$err_cd!="000"){
    code <- json_all$result$err_cd
    msg  <- json_all$result$err_msg	
    stop(paste0(code, "\n ", msg))
  }
  json_all[[1]][[4]]
}

# Retrieving data from FSIS

getFsisData <- function(api_key=api_key,finance_cd="0010001",list_no="SA053",account_cd="B",term="Y",
                        start_month="200801",end_month="202012"){
  url <- paste0("http://fisis.fss.or.kr/openapi/statisticsInfoSearch.json?lang=kr&auth=",api_key,"&financeCd=",finance_cd,
                "&listNo=",list_no,"&accountCd=",account_cd,"&term=",term,"&startBaseMm=",start_month,"&endBaseMm=",end_month)
  
  html <- GET(url)
  res <- rawToChar(html$content)
  Encoding(res) <- "UTF-8"
  json_all <- fromJSON(res)
  
  if (json_all$result$err_cd!="000"){
    code <- json_all$result$err_cd
    msg  <- json_all$result$err_msg	
    stop(paste0(code, "\n ", msg))
  }
  json_all[[1]][[7]]
}


## 3. BIS API

# BIS DB 생성
getBISDB <- function(){
bisDB_url <- 'https://www.bis.org/statistics/full_data_sets.htm'
bisDB_nodes <- bisDB_url %>%  
  xml2::read_html() %>% 
  rvest::html_nodes(xpath = "//a[contains(@href, 'zip')]") 
tibble(name = rvest::html_text(bisDB_nodes), url = str_c('https://www.bis.org',rvest::html_attr(bisDB_nodes,"href")))
}


downloadBISDB <- function (url, ...) {
  tmp_dir <- tempdir()
  tmp_file <- tempfile(fileext = ".zip")
  utils::download.file(url, tmp_file, mode = "wb", ...)
  filename <- utils::unzip(tmp_file, list = TRUE)
  utils::unzip(tmp_file, exdir = tmp_dir)
  path=file.path(tmp_dir, filename$Name)
  readr::read_csv(path)
}



##############################################################################
#
# Part II. Data manipulation functions
#
##############################################################################

## 0. general data transformation functions

# making variable for growth, difference, volatility, log difference, and EMWA volatility

makeVariable <- function(data, type=c("growth","diff","vol","logdiff","EMWAvol"), terms,lambda=0.94){
  n <- length(data)
  case_when(type=="growth" ~ 100*data/lag(data,n=terms)-100,
            type=="logdiff" ~ log(data)-lag(log(data),n=terms),
            type=="diff" ~ data-lag(data,n=terms),
            type=="vol"~ c(rep(NA_real_,terms-1),map_dbl(terms:n, function(i) sd(data[(i-terms+1):i]))),
            type=="EWMAvol"~ c(rep(NA_real_,terms), sqrt((1-lambda)*data[(terms+1):n]^2+
                                                     lambda*map_dbl(terms:n, function(i) sd(data[(i-terms+1):i]))[-1]^2)),
            TRUE ~ NA_real_)
}


# # disaggregation of annual data to quarterly one
# year2qtr <- function(low_frq,high_frq, conversion="sum"){
#   low_frq <- zoo(df[[target]], order.by=df[["year"]])
#   high_frq <-  zoo(df[[]], order.by=df[["year"]])
#   val=predict(td(low_frq ~ 1 , to="quarterly", conversion=conversion, method = "denton-cholette"))
#   tibble(yearQ=as.yearqtr(index(val)),val=as.numeric(val)) %>% set_names(c("yearQ",name))
# }


## 1. level data cleansing for ECOS api

# level data cleansing
levelCleansingECOS <- function(DATA,name,period=c("MM","QQ","YY")) {
  if(period=="QQ"){
      DATA[[name]]%>% transmute(time=as.yearqtr(TIME,format="%Y%q"), val=as.double(DATA_VALUE)) %>%  
      set_names(c("yearQ",name))
  }else if(period=="YY"){
      DATA[[name]]%>% transmute(year=as.numeric(TIME), val=as.double(DATA_VALUE)) %>% 
      set_names(c("year",name))
  }else if(period=="MM"){
    DATA[[name]]%>% transmute(time=as.yearmon(TIME,format="%Y%m"), val=as.double(DATA_VALUE)) %>%
      set_names(c("yearM",name))
  }else if(period=="DD"){
    DATA[[name]]%>% transmute(time=as.Date(TIME,format="%Y%m%d"), val=as.double(DATA_VALUE)) %>%
      set_names(c("date",name))  
  } else{
    print("wrong input for the period argument")}
}


## 2. quarterly data auxiliary functions for ECOS api

# annual to quarterly data
year2qtrECOS <- function(DATA,name,conversion="sum"){
  low_frq <- zoo(DATA[[name]]$DATA_VALUE, order.by=ymd(str_c(DATA[[name]]$TIME,"-12-01")))
  val=predict(td(low_frq ~ 1 , to="quarterly", conversion=conversion, method = "denton-cholette"))
  tibble(yearQ=as.yearqtr(index(val)),val=as.numeric(val)) %>% set_names(c("yearQ",name))
}

# HH_DEBT and CP_DEBT cleansing
Obtain_DebtQECOS <- function(work_needed_data,DATA) {
  map(work_needed_data, ~levelCleansingECOS(DATA,.x,"QQ")) %>% reduce(full_join,by="yearQ") %>% 
    set_names(c("yearQ","DEBT3","DEBT2","DEBT1")) %>%  
    mutate(DEBT3=replace(DEBT3,DEBT3==0,NA),DEBT2=replace(DEBT2,DEBT2==0,NA)) %>%
    arrange(yearQ) %>% mutate(ratio1=mean(DEBT1/DEBT2,na.rm=TRUE),
                              ratio2=mean(DEBT2/DEBT3,na.rm=TRUE)) %>% 
    mutate(DEBT3=DEBT3*ratio1*ratio2,DEBT2=DEBT2*ratio1,
           DEBT=case_when(is.na(DEBT1) & is.na(DEBT2) ~ DEBT3,
                          is.na(DEBT1) ~ DEBT2,
                          TRUE ~ DEBT1)) %>% select(yearQ,DEBT) 
} 


## 3. monthly data auxiliary functions for ECOS api

# monthly to quarterly data
mon2qtrECOS <- function(DATA,name) DATA[[name]] %>% transmute(yearQ=as.yearqtr(ym(TIME)),val=as.double(DATA_VALUE)) %>% 
                               group_by(yearQ) %>% summarise(mean_val=mean(val),.groups="drop") %>% set_names(c("yearQ",name))

# monthly to annual data
mon2yearECOS <- function(DATA,name) DATA[[name]] %>% transmute(year=year(ymd(paste0(TIME,"-01"))),val=as.double(DATA_VALUE)) %>% 
  group_by(year) %>% summarise(mean_val=mean(val),.groups="drop") %>% set_names(c("year",name))


## 3. annual auxiliary functions for ECOS api

# Get DEBT series
Obtain_DebtYECOS <- function(work_needed_data,DATA) {
  map(work_needed_data, ~levelCleansingECOS(DATA,.x,"YY")) %>% reduce(full_join,by="year") %>% 
    set_names(c("year","DEBT3","DEBT2","DEBT1")) %>%  
    arrange(year) %>% mutate(DEBT3=replace(DEBT3,DEBT3==0,NA),DEBT2=replace(DEBT2,DEBT2==0,NA)) %>% 
                            mutate(ratio1=mean(DEBT1/DEBT2,na.rm=TRUE),
                           ratio2=mean(DEBT2/DEBT3,na.rm=TRUE)) %>% 
  mutate(DEBT3=DEBT3*ratio1*ratio2,DEBT2=DEBT2*ratio1,
         DEBT=case_when(is.na(DEBT1) & is.na(DEBT2) ~ DEBT3,
                           is.na(DEBT1) ~ DEBT2,
                           TRUE ~ DEBT1)) %>% select(year,DEBT) 
  } 


