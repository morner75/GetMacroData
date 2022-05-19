packages <- c("tidyverse","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

source("R/Functions.R")
ecosSearch("통합재정수지") %>% View()

getEcosData(ECOS_key,'028Y009','MM',
            start_time=EcosTerm(start_date,'MM'),
            end_time =EcosTerm(today(),'MM'),
            'C','?','?') 
