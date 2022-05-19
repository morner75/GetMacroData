packages <- c("tidyverse","httr","jsonlite")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

source("R/Functions.R")
ecosSearch("주택담보대출") %>% View()

ecosSearch("4.2.") %>% View()
