
StarsDataQ <- readRDS('Output/macro_data.rds')[["quarterly"]]
names(StarsDataQ)
group1 <- c('INT_TS_Q',"INT_KTB3Y_VOL_Q",'INT_KTB3Y_Q','INT_KTB10Y_Q',
            'INT_CALL_Q','KOSPI_VOL_Q','KOSPI_QG','HOUSE_QG','INT_RKTB10Y_Q') 
group2 <- c('DEBT2GDP_Q','DEBT_QG','HOUSE2INCOME_Q','HOUSE_QG','CONSTRTN_QG',
            'RESID_PERMEIT_Q','HH_MORT_QG','EXT_DEBT_Q','CRRNT_BAL_Q',
            'CP_INTCOVERAGE_Q','CP_DEBT_2GDP_Q','CP_DEBT2EQUITY_Q', 'HH_DEBT2GDP_Q')
group3 <- c('US_RGDP_QG','CHN_RGDP_QG','OIL_Y','AL_Q','CU_Q')

StarsDataQ %>% select(any_of(c(group1,group2,group3))) 

StarsDataQ %>% select(all_of(group1))
