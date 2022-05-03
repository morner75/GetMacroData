packages <- c("tidyverse","foreach","lubridate","zoo","ggpubr")
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/Functions.R")

data <- readRDS("Rdata/macro_data.rds")$quarterly %>% select(yearQ,RGDP_Q,USDKRW,INT_CALL) %>% filter(yearQ>=1999)

## data for growth
data <- data %>% mutate(EX=USDKRW,RGDP=makeVariable(RGDP_Q,type="growth",terms=4),CALL=INT_CALL) %>% 
          select(yearQ,RGDP,EX,CALL) %>% filter(yearQ>=2000 & yearQ<2021)
dataL <- data %>% pivot_longer(-yearQ)
p1 <- ggplot(data=data,aes(yearQ))+
  geom_line(aes(y=RGDP))
p2 <- ggplot(data=data,aes(yearQ))+
  geom_line(aes(y=EX))
p3 <- ggplot(data=data,aes(yearQ))+
  geom_line(aes(y=CALL))

ggarrange(p1,p2,p3,ncol=1)


data[,-1] %>% as.matrix()
