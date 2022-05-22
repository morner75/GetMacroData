packages <- c("tidyverse","tidymodels","xts","lubridate","stringr","broom","quantreg","foreach")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/Functions.R")

## 0. Data preparation

# data loading
StarsDataQ <- readRDS('Output/macro_data.rds')[["quarterly"]]

# Step1: variable selection  : types - growth, level, etc.
variable_type1 <- names(StarsDataQ) %>% 
                    str_subset("_QG$|^INT_|RATIO|RATE|.+2.+|BAL|VOL")

variable_type2 <- c("AL_Q","CP_INTCOVERAGE_Q","CRRNT_ACC_Q","CU_Q","EMP_Q",  
                    "FIN_ACC_Q","GDP_DF_Q","NET_TERMS_TRADE_Q","NI_Q","OIL_Q","REER_BB_Q",
                    "REER_NB_Q","USDKRW_Q")

# selected variables and remove linear dependent varialbes
data_selected0 <- StarsDataQ %>% 
                  select(all_of(c("yearQ",variable_type1,variable_type2)) %>% sort(),
                         -c(CONSTRCTN_INVEST_QG,CRRNT_BAL_Q,EQUIP_INVEST_QG,
                            DEBT_QG,DEBT2GDP_Q,HOUSE2INCOME_Q,HH_DEBT_QG))  

## Step2 : Handling NAs (no observations)      

start_yearQ <- "2000 Q1" %>% as.yearqtr() %>% as.numeric()
end_yearQ <- "2022 Q1" %>% as.yearqtr() %>% as.numeric()


var_na <- data_selected0 %>% 
          filter(yearQ>=start_yearQ, yearQ<=end_yearQ) %>% 
          is.na() %>% 
          colSums() %>% 
          .[.>0]
var_na

# selected variables
data_selected <- data_selected0 %>% select(yearQ,!any_of(names(var_na))) %>% 
                  filter(yearQ>=start_yearQ, yearQ<=end_yearQ) 
# check
data_selected %>% is.na() %>% colSums()
# long-type
data_selectedL <- data_selected %>% 
                  pivot_longer(-yearQ) 

## Step3: Scaling

# define function
scale <- function(x) (x - mean(x))/sd(x)

# scaled data
data_scaled <- data_selected %>% 
                mutate_if(is.numeric,scale) 

# to long-type for plot
data_scaledL <- data_scaled %>% 
                    pivot_longer(-yearQ)

## Step4: plotting prepared data

ggplot(data_scaledL,aes(yearQ,value))+
  geom_line(aes(col=name),show.legend = F)

### 1. General approach

## 1.1. PCA

# scaling_info
scaling_info <- data_selectedL %>% 
                group_by(name) %>% 
                summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE))
              
# data preparation
pca_data <- data_scaled %>% 
              select(-yearQ)

# fitting
pca_fit <- pca_data %>% 
            prcomp(scale = TRUE)

# data augmentation
pca_data_aug <- pca_fit %>% augment(data_scaled) 

# fitted value 
ggplot(pca_data_aug, aes(.fittedPC1, .fittedPC2,color=RGDP_QG)) + 
  geom_point(size = 2) +
  scale_color_gradient(low="red",high="yellow")+
  theme_minimal()

# eigenvalues
pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  filter(PC<=15) %>% 
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:15) +
  scale_y_continuous(labels = scales::percent_format(),
                      expand = expansion(mult = c(0, 0.01)),
                     limits=c(0,0.5)) +
  theme_minimal()

arrow_style <- arrow(angle = 15, 
                     ends = "first", 
                     type = "closed", 
                     length = grid::unit(5, "pt"))

# biplot
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC", 
              names_prefix = "PC", 
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, 
               yend = 0, 
               arrow = arrow_style,
               color="gray") +
  geom_text(aes(label = column),
            hjust = 1, 
            nudge_x = -0.01, 
            size=2,
            color = "darkblue") +
  theme_minimal()


## 1.2. Quantile regression

# target variable 
target <- c("RGDP_QG","HOUSE_QG","CPI_QG",'USDKRW_Q',"INT_KTB3Y_Q")

quant_data <- pca_data_aug %>% 
              select(yearQ, all_of(target), PC1=.fittedPC1,PC2=.fittedPC2,PC3=.fittedPC3,PC4=.fittedPC4)
quant_dataL <- quant_data %>% 
               pivot_longer(-yearQ) %>% 
               mutate(vartype=ifelse(str_detect(name,"^PC"),"predictors","responses"))

ggplot(quant_dataL,aes(yearQ,value,group=name,color=name))+
  geom_line()+
  facet_wrap(~vartype, ncol=1,scales="free_y")+
  theme_minimal()


# prediction k=1,2,3,4,5,6,7,8,9,10,11,12

# 1.2.1. RGDP_QG
response <- "RGDP_QG"
formula = as.formula(paste0(response," ~ PC1 + PC2 +PC3")) 

Res <- foreach(lead_period=1:12,.combine=rbind) %do% {
lead_period <- 1

model_data <- quant_data %>% 
              mutate(RGDP_QG=lead(RGDP_QG,n=lead_period)) %>% 
              drop_na()

mod <- map(c(0.1,0.5,0.9), function(.x) rq(formula,.x,data=model_data))

fitted_data <- map(mod, ~{augment(.x, model_data) %>% 
                          select(yearQ,
                          all_of(attr(attr(terms(formula),"factors"),"dimnames")[[1]]),
                          .fitted)})
fitted_data_aug <- reduce(fitted_data,.f=function(.x,.y) left_join(.x,.y,by=c("yearQ","RGDP_QG","PC1","PC2","PC3"))) %>%
                   set_names(c("yearQ", attr(attr(terms(formula),"factors"),"dimnames")[[1]],"lower","med","upper"))

# ggplot(fitted_data_aug,aes(yearQ,med))+
#   geom_line()+
#   geom_ribbon(aes(ymin=lower,ymax=upper),fill="lightblue",alpha=0.5)+
#   geom_point(aes(y=RGDP_QG))+
#   theme_minimal()

eval <- fitted_data_aug %>% 
        mutate(eval=ifelse(RGDP_QG<=lower|RGDP_QG>=upper,1,0)) %>% 
        summarise(eval=mean(eval)) %>% unlist()

prediction <- map_dbl(mod, ~predict.rq(.x,newdata=quant_data %>% slice_tail())) %>% 
              set_names("lower","med","upper")

c(prediction, eval)
}


RGDP_QG_toOrignScale <- (scaling_info %>% filter(name=="RGDP_QG") %>% pull(sd))*Res[,1:3]+
                        (scaling_info %>% filter(name=="RGDP_QG") %>% pull(mean))



 






group1 <- c('INT_TS_Q',"INT_KTB3Y_VOL_Q",'INT_KTB3Y_Q','INT_KTB10Y_Q',
            'INT_CALL_Q','KOSPI_VOL_Q','KOSPI_QG','HOUSE_QG','INT_RKTB10Y_Q') 
group2 <- c('DEBT2GDP_Q','DEBT_QG','HOUSE2INCOME_Q','HOUSE_QG','CONSTRTN_QG',
            'RESID_PERMIT_Q','HH_MORT_QG','EXT_DEBT_Q','CRRNT_BAL_Q',
            'CP_DEBT2GDP_Q', 'HH_DEBT2GDP_Q')
group3 <- c('US_RGDP_QG','CHN_RGDP_QG','OIL_Q','AL_Q','CU_Q')




data_g1 <- data %>% select(all_of(group1))
data_g2 <- data %>% select(all_of(group2))




# 1. GaR methods on paper

data_g1 <- data %>% select(all_of(group1))
data_g2 <- data %>% select(all_of(group2))


pca_fit <- data_g1 %>% 
            prcomp(scale = TRUE)

data_g1_aug <- pca_fit %>% augment(data) 
  
ggplot(data_g1_aug, aes(.fittedPC1, .fittedPC2,color=RGDP_QG)) + 
  geom_point(size = 1.5) +
  scale_color_gradient(low="red",high="yellow")+
  theme_bw()



