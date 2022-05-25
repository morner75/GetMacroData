packages <- c("tidyverse","tidymodels","xts","lubridate","stringr","broom","quantreg","foreach","rlang")
if(!all(packages %in% installed.packages())) install.packages(pkgs=packages[!(packages %in% installed.packages())], dependencies = TRUE)
sapply(packages,require,character.only=TRUE)

rm(list=ls())
source("R/Functions.R")

## 0. Data preparation

# data loading
StarsDataQ <- openxlsx::read.xlsx('Output/macro_dataF.xlsx',sheet = "quarterly",rowNames = FALSE) %>% 
  mutate(yearQ=as.yearqtr(yearQ),
         INT_TS0_Q=INT_KTB3Y_Q-INT_BASE_Q)


# Step1: variable selection  : types - growth, level, etc.
variable_type1 <- names(StarsDataQ) %>% 
  str_subset("_QG$|^INT_|RATIO|RATE|.+2.+|BAL|VOL")

variable_type2 <- c("AL_Q","CP_INTCOVERAGE_Q","CRRNT_ACC_Q","CU_Q","EMP_Q",  
                    "FIN_ACC_Q","GDP_DF_Q","NET_TERMS_TRADE_Q","NI_Q","OIL_Q","REER_BB_Q",
                    "REER_NB_Q","USDKRW_Q")

# selected variables and remove linear dependent varialbes
data_selected0 <- StarsDataQ %>% 
  select(all_of(c("yearQ",variable_type1,variable_type2)) %>% sort(),
         -c(CONSTRCTN_INVEST_QG,DEBT_QG,DEBT2GDP_Q,HOUSE2INCOME_Q,HH_DEBT_QG))  

## Step2 : Handling NAs (missing observations)      

start_yearQ <- "2001 Q1" %>% as.yearqtr() %>% as.numeric()
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

# ggplot(data_scaledL,aes(yearQ2date(yearQ),value,group=name))+
#   geom_line(aes(col=name),show.legend = F)+
#   scale_x_date()+
#   theme_minimal()



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
  prcomp(scale = FALSE)

# data augmentation
pca_data_aug <- pca_fit %>% augment(data_scaled) 

# fitted value 
# ggplot(pca_data_aug, aes(.fittedPC1, .fittedPC2,color=RGDP_QG)) + 
#   geom_point(size = 2) +
#   scale_color_gradient(low="red",high="yellow")+
#   theme_minimal()

# eigenvalues
# pca_fit %>%
#   tidy(matrix = "eigenvalues") %>%
#   filter(PC<=15) %>% 
#   ggplot(aes(PC, percent)) +
#   geom_col(fill = "#56B4E9", alpha = 0.8) +
#   scale_x_continuous(breaks = 1:15) +
#   scale_y_continuous(labels = scales::percent_format(),
#                       expand = expansion(mult = c(0, 0.01)),
#                      limits=c(0,0.5)) +
#   theme_minimal()

# arrow_style <- arrow(angle = 15, 
#                      ends = "first", 
#                      type = "closed", 
#                      length = grid::unit(5, "pt"))

# biplot
# pca_fit %>%
#   tidy(matrix = "rotation") %>%
#   pivot_wider(names_from = "PC", 
#               names_prefix = "PC", 
#               values_from = "value") %>%
#   ggplot(aes(PC1, PC4)) +
#   geom_segment(xend = 0, 
#                yend = 0, 
#                arrow = arrow_style,
#                color="gray") +
#   geom_text(aes(label = column),
#             hjust = 1, 
#             nudge_x = -0.01, 
#             size=2,
#             color = "darkblue") +
#   theme_minimal()


## 1.2. Choose principal components

# target variable 
target <-  c('INT_TS0_Q',"INT_TS_Q","CPI_QG") # c("INT_KTB10Y_Q") #
pca_data_aug <- pca_data_aug %>% rename_with(.fn=~str_remove(.x,pattern=".fitted"))
reg_components <- map(target, ~select_component(data=pca_data_aug, var=.x,no.var=5)) %>% set_names(target)


## 1.3. Quantile regression

# target variable 
components <- reg_components %>% unlist() %>% unname()

quant_data <- pca_data_aug %>% 
  select(yearQ, all_of(c(target,components)))
# quant_dataL <- quant_data %>% 
#                pivot_longer(-yearQ) %>% 
#                mutate(vartype=ifelse(str_detect(name,"^PC"),"predictors","responses"))

# ggplot(quant_dataL,aes(yearQ,value,group=name,color=name))+
#   geom_line()+
#   facet_wrap(~vartype, ncol=1,scales="free_y")+
#   theme_minimal()

Results <- map(target, 
               ~Anything_at_Risk(.x,pred_terms=7,
                                 components=reg_components[[.x]] %>% slice_head(n=5),
                                 quant_data=quant_data,
                                 tau=c(0.05,0.10,0.5,0.90,0.95),scaling_info = scaling_info)) %>% 
  set_names(target)


## 1.4. Analysis
tidy_results <- Results %>% transpose() %>% pluck(".pred") %>% map_dfr(~.x,.id="var")

#tidy_results %>% write.csv("temp3.csv")

tidy_results %>% View(.)

ggplot(tidy_results,aes(yearQ,`0.5`))+
  geom_line()+
  geom_ribbon(aes(ymin=`0.05`,ymax=`0.95`),fill="lightblue",alpha=0.5)+
  geom_ribbon(aes(ymin=`0.1`,ymax=`0.9`),fill="blue",alpha=0.5)+
  facet_wrap(~var,ncol=1,scales="free_y")+
  ylab("")+
  scale_fill_discrete()+
  theme_minimal()

data_selected %>% select(yearQ,all_of(target)) %>% slice_tail(n=10)

# eval_quant <- Results %>% transpose() %>% pluck(".eval_quant")
# eval_quant %>% map(mean)
# 
# 
# eval_fitted <- Results %>% transpose() %>% pluck(".eval_fitted")
# eval_fitted %>% map(mean)




## 1.4. Model evaluation
# 
# pred_terms=8;quant_data=quant_data;tau=c(0.50)
# 
# Res <- foreach(var=target) %do% { 
#   res <- foreach(n=1:5, .combine=rbind) %do%{
#     
#     components=reg_components[[var]] %>% slice_head(n=n);
#     
#     Res <- foreach(lead_period=1:pred_terms,.combine=c) %do% {
#     #var <- "HOUSE_QG";lead_period=8;quant_data=quant_data;components=reg_components[[var]];tau=c(0.1,0.5,0.9)
#       component_each=components[[str_c("term_",lead_period)]]
#         
#       formula = as.formula(str_c(var," ~ ", paste(component_each,sep = "",collapse = " + ")))
#         
#       model_data <- quant_data %>% 
#         mutate({{var}}:=lead(.data[[var]],n=lead_period)) %>% 
#         drop_na()
#       
#       model_training <- map(seq_len(nrow(model_data)), ~{model_data %>% slice(-.x)}) 
#       model_test <- map(seq_len(nrow(model_data)), ~{model_data %>% slice(.x) %>% select(starts_with("PC"))})
#       actual <- map_dfr(seq_len(nrow(model_data)), ~{model_data %>% slice(.x) %>% select(all_of(var))}) %>% pull()
#       
#       mods <- map(model_training, ~rq(formula,tau=0.5,data=.x))   
#       pred <- map2_dbl(mods,model_test, ~predict(.x,newdata=.y))  
#       (actual-pred)^2 %>% mean()       
#     } %>% set_names(str_c("term",1:pred_terms))
#     
#   } %>% as_tibble() %>% mutate(n=1:5,.before=1)
# } %>% set_names(target)
# 
# saveRDS(Res,"ModEval.rds")
# Res <- readRDS("ModEval.rds")
# map_dfr(Res,~.x,.id="var") %>% 
#    pivot_longer(-c(var,n),names_to="term",values_to="eval") %>% 
#    mutate(n=factor(n),
#           term=factor(term,levels=str_c("term",1:pred_terms))) %>%
#    ggplot(aes(n,eval))+
#     geom_col(aes(fill=n))+
#     facet_wrap(vars(var, term),ncol=8,scales="free_y")+
#     theme_minimal()
# 
# 


# ## Ref: GaR original approach
# 
# group1 <- c('INT_TS_Q',"INT_KTB3Y_VOL_Q",'INT_KTB3Y_Q','INT_KTB10Y_Q',
#             'INT_CALL_Q','KOSPI_VOL_Q','KOSPI_QG','HOUSE_QG','INT_RKTB10Y_Q') 
# group2 <- c('DEBT2GDP_Q','DEBT_QG','HOUSE2INCOME_Q','HOUSE_QG','CONSTRTN_QG',
#             'RESID_PERMIT_Q','HH_MORT_QG','EXT_DEBT_Q','CRRNT_BAL_Q',
#             'CP_DEBT2GDP_Q', 'HH_DEBT2GDP_Q',"GOV_DEBT_QG","BIS_Q")
# group3 <- c('US_RGDP_QG','CHN_RGDP_QG','OIL_Q','AL_Q','CU_Q')
# 
# 
# start_yearQ <- 2000.75
# 
# data_g1 <- StarsDataQ %>% 
#             select(yearQ, all_of(c("RGDP_QG",group1))) %>% 
#             filter(as.numeric(yearQ)>=start_yearQ) %>% 
#             select(-INT_KTB10Y_Q) %>% 
#             drop_na()
#             
# 
# 
# data_g2 <- StarsDataQ %>% 
#             select(yearQ, all_of(c("RGDP_QG",group2))) %>% 
#             filter(as.numeric(yearQ)>=start_yearQ) %>% 
#             select(-c(RESID_PERMIT_Q,HH_MORT_QG))%>% 
#             drop_na()
# 
# data_g3 <- StarsDataQ %>% 
#             select(yearQ, all_of(c("RGDP_QG",group3))) %>% 
#             filter(as.numeric(yearQ)>=start_yearQ,year(yearQ)<=2021) %>% 
#             select(-CHN_RGDP_QG)%>% 
#             drop_na()
# 
#             
# 
# var1 <- data_g1 %>% select(-yearQ) %>% 
#               prcomp(scale = TRUE) %>% tidy(matrix="x") %>% filter(PC==1) %>% pull(value)
# var2 <- data_g2 %>% select(-yearQ) %>% 
#   prcomp(scale = TRUE)%>% tidy(matrix="x") %>% filter(PC==1) %>% pull(value)
# var3 <- data_g3 %>% select(-yearQ) %>% 
#   prcomp(scale = TRUE) %>% tidy(matrix="x") %>% filter(PC==1) %>% pull(value)
# 
# data <- cbind(var1,var2,var3) %>% as.data.frame() %>% mutate(yearQ=as.yearqtr(seq(2000.75,2021.75,by=0.25)),.before=1)
# 
# data0 <- data %>% left_join(data_g1 %>% select(yearQ,RGDP_QG),by="yearQ")
# 
# data <- map(1:12, ~{data0 %>% mutate(response=lead(RGDP_QG,n=.x))})
# Res <- map(1:12, ~rq(response ~ var1 + var2 + var3, tau=c(0.05,0.25,0.5,0.75,0.95), data=data[[.x]]))
# 
# data2 <- imap_dfr(Res, ~predict.rq(.x,newdata=data[[.y]] %>% slice_tail(n=1))) %>% 
#          set_names( as.character(c(0.05,0.25,0.5,0.75,0.95))) %>% 
#          mutate(yearQ=as.yearqtr(seq(2021.0,2023.75,by=0.25)),.before=1)
# 
# ggplot(data2,aes(yearQ,`0.5`))+
#   geom_line()+
#   geom_ribbon(aes(ymin=`0.05`,ymax=`0.95`),fill="lightblue",alpha=0.5)+
#   geom_ribbon(aes(ymin=`0.25`,ymax=`0.75`),fill="blue",alpha=0.5)+
#   ylab("")+
#   scale_fill_discrete()+
#   theme_minimal()
