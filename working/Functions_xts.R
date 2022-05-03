packages <- c("tidyverse","xts","lubridate","zoo","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)


# level data cleansing
levelXTS <- function(DATA,name,period=c("QQ","YY")) {
  if(period=="QQ"){
     index=ceiling_date(as.Date(as.yearqtr(DATA[[name]]$TIME,format="%Y%q")),unit="quarter")-1
     res <- xts(as.double(DATA[[name]]$DATA_VALUE),order.by=index) 
     colnames(res) <- name
     return(res)
  }else if(period=="YY"){
    index <- as.Date(paste(DATA[[name]]$TIME,"12","31",sep = "-"))
    res <- xts(as.double(DATA[[name]]$DATA_VALUE),order.by=index) 
    colnames(res) <- name
    return(res)
  }else if(period=="MM"){
    index <- ceiling_date(ym(DATA[[name]]$TIME),unit="month")-1
    res <- xts(as.double(DATA[[name]]$DATA_VALUE),order.by=index) 
    colnames(res) <- name
    return(res)
    }else{ print("wrong input for the period argument")}
}




# retrieving data from ECOS

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


# ECOS API period setting
EcosTerm <- function(time,type){
  case_when(type=="QQ" ~  as.yearqtr(time) %>% format(.,"%Y%q"),
            type=="MM" ~   as.yearmon(time) %>% format(.,"%Y%m"),
            TRUE ~ as.character(year(time)))
}


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

        