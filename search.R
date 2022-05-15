packages <- c("tidyverse","stringr")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls)

ecosSearch <- function(x) {
  data <- readRDS("Output/EcosStatsList.rds")
  search <- data %>% transmute(search=str_c(통계명,통계항목명,sep=" ")) %>% pull()
  flag <- map(x, ~str_detect(search,.x)) %>% reduce(magrittr::multiply_by) %>% as.logical()
  EcosStatsList %>% filter(flag)
}

ecosSearch(c("CP"))
