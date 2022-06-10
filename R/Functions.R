##############################################################################
#
# Auxiliary Functions
#
##############################################################################

packages <- c("tidyverse","httr","jsonlite","lubridate","zoo")
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
                        item_code2, item_code3, lang="kr"){
  
  url <- paste("http://ecos.bok.or.kr/api/StatisticSearch",ECOS_key,"json",lang,"1/10000",
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
getEcosList <- function(ECOS_key=ECOS_key,lang="kr"){
  url <- paste0("https://ecos.bok.or.kr/api/StatisticTableList/",ECOS_key,"/json/",lang,"/1/1000/")
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
    dplyr::select(STAT_CODE,
                  P_STAT_CODE,
                  STAT_NAME,
                  CYCLE,
                  ORG_NAME,
                  SRCH_YN)
}

# retrieving stats details from ECOS
getEcosCode <- function(ECOS_key=ECOS_key,STAT_CODE,lang="kr"){
  url <- paste0("http://ecos.bok.or.kr/api/StatisticItemList/",ECOS_key,"/json/",lang,"/1/1000/",STAT_CODE)
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
    dplyr::select(STAT_CODE,
                  STAT_NAME	,
                  GRP_CODE,
                  GRP_NAME,
                  P_ITEM_CODE,
                  ITEM_CODE,
                  ITEM_NAME,
                  CYCLE,
                  START_TIME,
                  END_TIME,
                  DATA_CNT,
                  WEIGHT)
}

# ECOS 100 Key statistics
getKeyStats <- function(ECOS_key=ECOS_key,lang="kr"){
  url <- paste0("http://ecos.bok.or.kr/api/KeyStatisticList/",ECOS_key,"/json/",lang,"/1/100/")
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
    dplyr::select(CLASS_NAME,
                  KEYSTAT_NAME,
                  CYCLE,
                  DATA_VALUE,
                  UNIT_NAME)
}

# ECOS statistics and code search
ecosSearch <- function(x) {
  data <- readRDS("Rdata/EcosStatsList.rds")
  search <- data %>% transmute(search=str_c(STAT_NAME,ITEM_NAME,STAT_NAME_EN,ITEM_NAME_EN,sep=" ")) %>% pull()
  flag <- map(x, ~str_detect(search,.x)) %>% reduce(magrittr::multiply_by) %>% as.logical()
  data %>% filter(flag) %>% distinct()
}


# ECOS API period setting
EcosTerm <- function(time,type){
  case_when(type=="M" ~  as.yearmon(time) %>% format(.,"%Y%m"),
            type=="Q" ~ as.yearqtr(time) %>% format(.,"%YQ%q"),
            type=="D" ~ as.Date(time) %>% format(.,"%Y%m%d"),
            TRUE ~ as.character(year(time)))
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
    DATA[[name]]%>% transmute(time=as.yearqtr(TIME,format="%YQ%q"), val=as.double(DATA_VALUE)) %>%  
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
    set_names(c("yearQ","DEBT1","DEBT2","DEBT3")) %>%  
    mutate(DEBT3=replace(DEBT3,DEBT3==0,NA),DEBT2=replace(DEBT2,DEBT2==0,NA)) %>%
    arrange(yearQ) %>% mutate(ratio1=mean(DEBT1/DEBT2,na.rm=TRUE),
                              ratio2=mean(DEBT2/DEBT3,na.rm=TRUE)) %>% 
    mutate(DEBT3=DEBT3*ratio1*ratio2,
           DEBT2=DEBT2*ratio1,
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
    set_names(c("year","DEBT1","DEBT2","DEBT3")) %>%  
    arrange(year) %>% mutate(DEBT3=replace(DEBT3,DEBT3==0,NA),DEBT2=replace(DEBT2,DEBT2==0,NA)) %>% 
    mutate(ratio1=mean(DEBT1/DEBT2,na.rm=TRUE),
           ratio2=mean(DEBT2/DEBT3,na.rm=TRUE)) %>% 
    mutate(DEBT3=DEBT3*ratio1*ratio2,DEBT2=DEBT2*ratio1,
           DEBT=case_when(is.na(DEBT1) & is.na(DEBT2) ~ DEBT3,
                          is.na(DEBT1) ~ DEBT2,
                          TRUE ~ DEBT1)) %>% select(year,DEBT) 
} 




##############################################################################
#
# Part III. Anything_at_Risk functions
#
##############################################################################



## 1. Component selection: Choosing most relevant components 

select_component <- function(data, var, pred_terms=12, no.var=6){
  
  #data=pca_data_aug ; var="RGDP_QG";  pred_terms=1; lead_period=1
  
  foreach(lead_period=1:pred_terms,.combine=cbind) %do% {  
    
    data.adj <- data %>% select(all_of(var),matches("^PC([1-9]|[1-3][0-9])$")) %>% 
      mutate({{var}}:=lead(.data[[var]],n=lead_period)) %>% 
      drop_na()
    
    var1 <- {cor(data.adj) %>% abs()}[1,][-1] %>% 
      .[.==max(.)] %>% names()
    
    var2 <- {data.adj %>% 
        select(starts_with("PC")) %>% 
        mutate(resid=resid(lm(as.formula(paste0(var," ~ ",var1)),data=data.adj)),.before=1) %>% 
        cor() %>% abs()}[1,][-1] %>% .[.==max(.)] %>% names()
    
    var3 <- {data.adj %>% 
        select(starts_with("PC")) %>% 
        mutate(resid=resid(lm(as.formula(paste0(var," ~ ",var1,"+",var2)),data=data.adj)),.before=1) %>% 
        cor() %>% abs()}[1,][-1] %>% .[.==max(.)] %>% names()
    
    var4 <- {data.adj %>% 
        select(starts_with("PC")) %>% 
        mutate(resid=resid(lm(as.formula(paste0(var," ~ ",var1,"+",var2,"+",var3)),data=data.adj)),.before=1) %>% 
        cor() %>% abs()}[1,][-1] %>% .[.==max(.)] %>% names()
    
    var5 <- {data.adj %>% 
        select(starts_with("PC")) %>% 
        mutate(resid=resid(lm(as.formula(paste0(var," ~ ",var1,"+",var2,"+",var3,"+",var4)),data=data.adj)),.before=1) %>% 
        cor() %>% abs()}[1,][-1] %>% .[.==max(.)] %>% names()
    
    var6 <- {data.adj %>% 
        select(starts_with("PC")) %>% 
        mutate(resid=resid(lm(as.formula(paste0(var," ~ ",var1,"+",var2,"+",var3,"+",var4,"+",var5)),data=data.adj)),.before=1) %>% 
        cor() %>% abs()}[1,][-1] %>% .[.==max(.)] %>% names()
    
    
    c(var1,var2,var3,var4,var5,var6)
  } %>% 
    as_tibble() %>% 
    set_names(str_c("term_",1:pred_terms)) %>% 
    slice_head(n=no.var)
}

## 2. Anything_at_Risk function (IMF GaR methods)

Anything_at_Risk <- function(var,pred_terms=12,quant_data=quant_data, components=components, tau=c(0.1,0.25,0.5,0.75,0.9),scaling_info){
  
  #var <- "USDKRW_Q";pred_terms=8;quant_data=quant_data;components=reg_components[[var]];tau=c(0.1,0.5,0.9)
  
  Res <- foreach(lead_period=1:pred_terms,.combine=rbind) %do% {
    
    # var <- "HOUSE_QG";lead_period=1;quant_data=quant_data;components=reg_components[[var]];tau=c(0.1,0.5,0.9)
    component_each=components[[str_c("term_",lead_period)]]
    
    formula = as.formula(str_c(var," ~ ", paste(component_each,sep = "",collapse = " + ")))
    
    model_data <- quant_data %>% 
      mutate({{var}}:=lead(.data[[var]],n=lead_period)) %>% 
      drop_na()
    
    mod <- map(tau, function(.x) rq(formula,.x,data=model_data))
    
    fitted_data <- map(mod, ~{augment(.x, model_data) %>% 
        select(yearQ,
               all_of(attr(attr(terms(formula),"factors"),"dimnames")[[1]]),
               .fitted)})
    fitted_data_aug <- reduce(fitted_data,
                              .f=function(.x,.y) 
                                left_join(.x,.y,by=c("yearQ",var,component_each))) %>%
      set_names(c("yearQ", attr(attr(terms(formula),"factors"),"dimnames")[[1]],as.character(tau)))
    
    
    eval_quant <- fitted_data_aug %>% 
      mutate(eval=ifelse(.data[[var]]<=.data[[as.character(tau[1])]]|
                           .data[[var]]>=.data[[as.character(tau[length(tau)])]],1,0)) %>% 
      summarise(eval=mean(eval)) %>% unlist()
    
    prediction <- map_dbl(mod, ~predict.rq(.x,newdata=quant_data %>% slice_tail())) %>% 
      set_names(as.character(tau))
    
    eval_fitted <- fitted_data_aug %>% mutate(eval_pred=abs(.data[[var]]-`0.5`)) %>% pull(eval_pred) %>% mean()
    
    c(prediction, eval_quant,eval_fitted)
  }
  
  
  toOriginScale <-{(scaling_info %>% filter(name==var) %>% pull(sd))*Res[,1:length(tau)]+
      (scaling_info %>% filter(name==var) %>% pull(mean)) } %>% 
    as_tibble() %>% 
    mutate(yearQ=as.yearqtr(seq(as.Date(pull(quant_data,yearQ) %>% last()),
                                by="quarter",length.out=pred_terms+1)[-1])) %>% 
    select(yearQ,everything())
  
  list(.pred=toOriginScale,.eval_quant=Res[,length(tau)+1] %>% unname(),.eval_fitted=Res[,ncol(Res)] %>% unname())
}


# Auto

# Auto_Anything_at_Risk <- function(data_selected, start_quarter,end_quarter,target){
#   
#   #data_selected=data_selected0 ;  start_quarter = "2012 Q1" ; end_quarter = "2021 Q4" ;target="USDKRW_Q"
#   start_yearQ <- start_quarter %>% as.yearqtr() %>% as.numeric()
#   end_yearQ <- end_quarter %>% as.yearqtr() %>% as.numeric()
#   
#   
#   var_na <- data_selected0 %>% 
#     filter(yearQ>=start_yearQ, yearQ<=end_yearQ) %>% 
#     is.na() %>% 
#     colSums() %>% 
#     .[.>0]
#   
#   # selected variables
#   data_selected <- data_selected0 %>% select(yearQ,!any_of(names(var_na))) %>% 
#     filter(yearQ>=start_yearQ, yearQ<=end_yearQ) 
#   
#   # define function
#   scale <- function(x) (x - mean(x))/sd(x)
#   
#   # scaled data
#   data_scaled <- data_selected %>% 
#     mutate_if(is.numeric,scale) 
#   
#   scaling_info <- data_scaled %>% 
#     pivot_longer(-yearQ) %>% 
#     group_by(name) %>% 
#     summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE)) 
#   
#   # data preparation
#   pca_data_aug <- data_scaled %>% 
#     select(-yearQ) %>% 
#     prcomp(scale = FALSE) %>%  
#     augment(data_scaled) 
#   
#   
#   # target variable 
#   pca_data_aug <- pca_data_aug %>% rename_with(.fn=~str_remove(.x,pattern=".fitted"))
#   reg_components <- map(target, ~select_component(data=pca_data_aug, var=.x,no.var=5)) %>% set_names(target)
#   components <- reg_components %>% unlist() %>% unname()
#   quant_data <- pca_data_aug %>%  select(yearQ, all_of(c(target,components)))
#   
#   Results <- map(target, 
#                  ~Anything_at_Risk(.x,pred_terms=8,
#                                    components=reg_components[[.x]] %>% slice_head(n=2),
#                                    quant_data=quant_data,
#                                    tau=c(0.05,0.5,0.95),scaling_info)) %>% 
#     set_names(target)
#   return(Results)
# }



