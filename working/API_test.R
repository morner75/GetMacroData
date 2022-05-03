packages <- c("tidyverse","httr","jsonlite","xml2","foreach","lubridate","zoo","readxl","tempdisagg","tsbox","XML")
sapply(packages,require,character.only=TRUE)

source("Functions.R")

current_Date <- as.Date(Sys.time())

api_key <- Sys.getenv(x="FSIS_key")
getFsisInfos(api_key=api_key,info_name="companySearch",item_code="F")
getFsisInfos(api_key=api_key,info_name="statisticsListSearch",item_code="F")
getFsisInfos(api_key=api_key,info_name="accountListSearch",item_code="SF304")

getFsisInfos(api_key=api_key,info_name="companySearch",item_code="F")[["finance_cd"]]
getFsisData(api_key=api_key,finance_cd= x,list_no="SF304",account_cd="J01",term="Y",
            start_month="201812",end_month="202012")


ComNames <- getFsisInfos(api_key=api_key,info_name="companySearch",item_code="F")[["finance_cd"]]
temp <- map(ComNames, function(x) getFsisData(api_key=api_key,finance_cd= x,list_no="SF304",account_cd="J01",term="Y",
              start_month="201812",end_month="202012"))  %>% discard(~(length(.x)==0)) %>% 
              map(function(df) df %>% transmute(ID=finance_cd, value=mean(as.numeric(a))) %>% distinct() )%>% do.call(rbind,.)  





# Public data

api_key <- Sys.getenv(x="PUBLICDATA_key")
url <- paste0("http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19InfStateJson?serviceKey=",
              api_key,"&pageNo=",1,"&numOfRows=",1000,"&startCreateDt=",20200229,"&endCreateDt=",20210417)
xml_raw <- xmlParse(url)
data <- xmlToDataFrame(getNodeSet(xml_raw,"//items/item"))
COVID19 <- data %>% transmute(date=ymd(stateDt),
                               infected=as.numeric(decideCnt),
                               death=as.numeric(deathCnt),
                               inHospital=as.numeric(careCnt)) %>% 
                    arrange(date) %>% mutate(yearmon=as.yearmon(date),newinfected=infected-lag(infected,1)) %>% 
                    group_by(yearmon) %>% summarise(infeced=sum(newinfected))  %>% 
                    filter( as.Date(yearmon ) >="2020-03-01" )
