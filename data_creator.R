library(readxl)
library(xlsx)
library(tidyverse)
library(lubridate)

# read in data
spread <- read_excel("Estimation/data/estimation.xlsx", sheet = "spread") %>%
  filter(Dates >= '2001-01-31') %>% filter(Dates <= '2020-01-31')

bidask <- read_excel("Estimation/data/estimation.xlsx", sheet = "bidask") %>%
  filter(Dates >= '2001-01-31') %>% filter(Dates <= '2020-01-31')

vix <- read_excel("Estimation/data/estimation.xlsx", sheet = "vix") %>%
  filter(Dates >= '2001-01-31') %>% filter(Dates <= '2020-01-31')

reer <- read_excel("Estimation/data/REER.xlsx", sheet = "reer") %>%
  filter(date >= '2001-01-31') %>% filter(date <= '2020-01-31')

ind_growth <- read_excel("Estimation/data/ind_growth.xlsx", sheet = "ind_growth_rel") %>%
  filter(date >= '2001-01-31') %>% filter(date <= '2020-01-31') %>%
  fill(Austria:Spain, .direction = "down")


## special treatments (skip to line 46)
# govbal <- read_csv("Estimation/data/govbal.csv") %>% select(-X1) %>%
#   mutate(date = as.Date(date, "%d-%m-%y")) %>%
#   filter(date >= '2001-01-31') %>% filter(date <= '2020-01-31') %>%
#   rename("Dates" = date)
#  
# debtgdp <- read_csv("Estimation/data/debtgdp.csv") %>% select(-X1) %>%
#   mutate(date = as.Date(date, "%d-%m-%y")) %>%
#   filter(date >= '2001-01-31') %>% filter(date <= '2020-01-31') %>%
#   rename("Dates" = date)
# 
# # exp_govbal <- full_join(ind_growth %>% select(Dates = date, Austria_ind = Austria), 
# #                      govbal, by = "Dates") %>% select(-Austria_ind) %>%
# #   fill(Austria:Spain, .direction = "down")
# # 
# # exp_debtgdp <- full_join(ind_growth %>% select(Dates = date, Austria_ind = Austria), 
# #                         debtgdp, by = "Dates") %>% select(-Austria_ind) %>%
# #   fill(Austria:Spain, .direction = "down")
# 
# #write.csv(exp_govbal, "Estimation/exp_govbal.csv")
# #write.csv(exp_debtgdp, "Estimation/exp_debtgdp.csv")

exp_govbal <- read_excel("Estimation/data/expected_vars.xlsx", 
                         sheet = "exp_govbal_rel") %>%
  filter(Dates >= '2001-01-31') %>% filter(Dates <= '2020-01-31')

exp_debtgdp <- read_excel("Estimation/data/expected_vars.xlsx", 
                          sheet = "exp_debtgdp_rel") %>%
  filter(Dates >= '2001-01-31') %>% filter(Dates <= '2020-01-31')

# create PC1 and PC2 series
source("Estimation/pca.R")

###### CREATE COUNTRY SHEETS #########
ctry_list <- colnames(spread) 
ctry_list <- ctry_list[-1]

for(i in 1:length(ctry_list)){
  assign(ctry_list[i], 
         tibble(
          date = spread$Dates,
          spread = spread[1+i],
          spread_change = spread -lag(spread),
          bidask = bidask[1+i],
          bidask_change = bidask - lag(bidask),
          vix = log(vix$VIX),
          vix_change = vix - lag(vix),
          reer = log(reer[1+i]),
          reer_change = reer - lag(reer),
          ind_growth = ind_growth[1+i],
          ind_growth_change = ind_growth - lag(ind_growth),
          exp_debtgdp = exp_debtgdp[1+i],
          exp_debtgdp_change = exp_debtgdp - lag(exp_debtgdp),
          exp_govbal = exp_govbal[1+i],
          exp_govbal_change = exp_govbal - lag(exp_govbal),
          PC2_FS = PCA$PC2_FS,
          PC2_FS_change = PC2_FS - lag(PC2_FS),
          PC2_S1 = PCA$PC2_S1,
          PC2_S1_change = PC2_S1 - lag(PC2_S1),
          PC2_S2 = PCA$PC2_S2,
          PC2_S2_change = PC2_S2 - lag(PC2_S2),
          PC2_S3 = PCA$PC2_S3,
          PC2_S3_change = PC2_S3 - lag(PC2_S3)) %>% as.matrix())
}

write.csv(Austria, "Country data/Austria_data.csv")
write.csv(Belgium, "Country data/Belgium_data.csv")
write.csv(Finland, "Country data/Finland_data.csv")
write.csv(France, "Country data/France_data.csv")
write.csv(Greece, "Country data/Greece_data.csv")
write.csv(Ireland, "Country data/Ireland_data.csv")
write.csv(Italy, "Country data/Italy_data.csv")
write.csv(Netherlands, "Country data/Netherlands_data.csv")
write.csv(Portugal, "Country data/Portugal_data.csv")
write.csv(Spain, "Country data/Spain_data.csv")

