packages <- c("tidyverse","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

source("R/Functions.R")
ecosSearch("국제상품가격") %>% View()

getEcosData(ECOS_key,'902Y015','M',
            start_time=EcosTerm(start_date,'M'),
            end_time =EcosTerm(today(),'M'),
            'USA','?','?') 
