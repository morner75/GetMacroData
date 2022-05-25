packages <- c("tidyverse","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

source("R/Functions.R")
ecosSearch("미국") %>% View()

getEcosData(ECOS_key,'901Y001','MM',
            start_time=EcosTerm(start_date,'MM'),
            end_time =EcosTerm(today(),'MM'),
            'AI1DC','?','?') 
