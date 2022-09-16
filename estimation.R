library(readxl)
library(xlsx)
library(lubridate)
library(forecast)
library(tidyverse)
library(kableExtra)
library(stats4)
library(lmtest)
library(sandwich)
library(tseries)

Austria <- read.csv("Country data/Austria_data.csv")
Belgium <- read.csv("Country data/Belgium_data.csv")
Finland <- read.csv("Country data/Finland_data.csv")
France <- read.csv("Country data/France_data.csv")
Greece <- read.csv("Country data/Greece_data.csv")
Ireland <- read.csv("Country data/Ireland_data.csv")
Italy <- read.csv("Country data/Italy_data.csv")
Netherlands <- read.csv("Country data/Netherlands_data.csv")
Portugal <- read.csv("Country data/Portugal_data.csv")
Spain <- read.csv("Country data/Spain_data.csv")

ctry_list <- c("Austria","Belgium","Finland","France","Greece","Ireland",
               "Italy","Netherlands","Portugal","Spain")

ctry_tbls <- list(Austria, Belgium, Finland, France, Greece,
                  Ireland, Italy, Netherlands, Portugal, Spain)

names(ctry_tbls) <- ctry_list

rm(list=setdiff(ls(), c("ctry_tbls", "ctry_list")))

####### PRE-CRISIS REGRESSIONS  #######

## Baseline model - Pre crisis
reg_m1s1_table <- tibble(
  Variable = c("constant", "t stat", "spread_l", "t stat", "reer", "t stat", "vix", "t stat", "Adj. R2", "ssr"))

for(i in 1:length(ctry_tbls)){
  ctry <- ctry_tbls[i]
  ctry <- ctry[[1]] %>% filter(X <= 79)
  model <- lm(spread ~ lag(spread) + reer + vix, ctry) 
  fit <- summary(model)
  t_vals <- coeftest(model, vcov = vcovHC(model, type = "HC0"))[,3]
  reg_m1s1_table[1,i+1] <- fit$coefficients[[1]]
  reg_m1s1_table[2,i+1] <- t_vals[1]
  reg_m1s1_table[3,i+1] <- fit$coefficients[[2]]
  reg_m1s1_table[4,i+1] <- t_vals[2]
  reg_m1s1_table[5,i+1] <- fit$coefficients[[3]]
  reg_m1s1_table[6,i+1] <- t_vals[3]
  reg_m1s1_table[7,i+1] <- fit$coefficients[[4]]
  reg_m1s1_table[8,i+1] <- t_vals[4]
  reg_m1s1_table[9,i+1] <- fit$adj.r.squared[1]
  reg_m1s1_table[10,i+1] <- sum(fit$residuals^2)
}
colnames(reg_m1s1_table) <- c("",ctry_list)
reg_m1s1_table

## Extended model - Pre crisis
reg_m2s1_table <- tibble(
  Variable = c("constant", "t stat", "spread_l", "t stat", "reer", "t stat", 
               "vix", "t stat", "bidask", "t stat", 
               "indgrowth", "t stat", "exp_govbal", "t stat",
               "exp_debtgdp", "t stat", "Adj. R2", "ssr"))

for(i in 1:length(ctry_tbls)){
  ctry <- ctry_tbls[i]
  ctry <- ctry[[1]] %>% filter(X <= 79)
  model <- lm(spread ~ lag(spread) + reer + vix + 
                      bidask + ind_growth + exp_debtgdp + exp_govbal, ctry)
  
  fit <- summary(model)
  t_vals <- coeftest(model, vcov = vcovHC(model, type = "HC0"))[,3]
  reg_m2s1_table[1,i+1] <- fit$coefficients[[1]]
  reg_m2s1_table[2,i+1] <- t_vals[1]
  reg_m2s1_table[3,i+1] <- fit$coefficients[[2]]
  reg_m2s1_table[4,i+1] <- t_vals[2]
  reg_m2s1_table[5,i+1] <- fit$coefficients[[3]]
  reg_m2s1_table[6,i+1] <- t_vals[3]
  
  reg_m2s1_table[7,i+1] <- fit$coefficients[[4]]
  reg_m2s1_table[8,i+1] <- t_vals[4]
  
  reg_m2s1_table[9,i+1] <- fit$coefficients[[5]]
  reg_m2s1_table[10,i+1] <- t_vals[5]
  
  reg_m2s1_table[11,i+1] <- fit$coefficients[[6]]
  reg_m2s1_table[12,i+1] <- t_vals[6]
  
  reg_m2s1_table[13,i+1] <- fit$coefficients[[7]]
  reg_m2s1_table[14,i+1] <- t_vals[7]
  
  reg_m2s1_table[15,i+1] <- fit$coefficients[[8]]
  reg_m2s1_table[16,i+1] <- t_vals[8]
  
  reg_m2s1_table[17,i+1] <- fit$adj.r.squared[1]
  reg_m2s1_table[18,i+1] <- sum(fit$residuals^2)
  rm(fit)
}

colnames(reg_m2s1_table) <- c("",ctry_list)
reg_m2s1_table


####### CRISIS REGRESSIONS  #######

## Baseline model - Crisis
reg_m1s2_table <- tibble(
  Variable = c("constant", "t stat", "spread_l", "t stat", "reer", "t stat", "vix", "t stat", 
               "PC2", "t stat", "Adj. R2", "ssr"))

for(i in 1:length(ctry_tbls)){
  ctry <- ctry_tbls[i]
  ctry <- ctry[[1]] %>% filter(X <= 144) %>% filter(X > 79)
  model <- lm(spread ~ lag(spread) + reer + vix + PC2_S2, ctry)
  fit <- summary(model)
  t_vals <- coeftest(model, vcov = vcovHC(model, type = "HC0"))[,3]
  reg_m1s2_table[1,i+1] <- fit$coefficients[[1]]
  reg_m1s2_table[2,i+1] <- t_vals[1]
  reg_m1s2_table[3,i+1] <- fit$coefficients[[2]]
  reg_m1s2_table[4,i+1] <- t_vals[2]
  reg_m1s2_table[5,i+1] <- fit$coefficients[[3]]
  reg_m1s2_table[6,i+1] <- t_vals[3]
  reg_m1s2_table[7,i+1] <- fit$coefficients[[4]]
  reg_m1s2_table[8,i+1] <- t_vals[4]
  reg_m1s2_table[9,i+1] <- fit$coefficients[[5]]
  reg_m1s2_table[10,i+1] <- t_vals[5]
  reg_m1s2_table[11,i+1] <- fit$adj.r.squared[1]
  reg_m1s2_table[12,i+1] <- sum(fit$residuals^2)
  rm(fit)
}
colnames(reg_m1s2_table) <- c("",ctry_list)
reg_m1s2_table

## Extended model - Crisis
reg_m2s2_table <- tibble(
  Variable = c("constant", "t stat", "spread_l", "t stat", "reer", "t stat", 
               "vix", "t stat", "bidask", "t stat", 
               "indgrowth", "t stat", "exp_govbal", "t stat",
               "exp_debtgdp", "t stat", "PC2", "t stat", "Adj. R2", "ssr"))

for(i in 1:length(ctry_tbls)){
  ctry <- ctry_tbls[i]
  ctry <- ctry[[1]] %>% filter(X <= 144) %>% filter(X > 79)
  model <- lm(spread ~ lag(spread) + reer + vix + 
                      bidask + ind_growth + exp_debtgdp + exp_govbal + PC2_S2, ctry)
  fit <- summary(model)
  t_vals <- coeftest(model, vcov = vcovHC(model, type = "HC0"))[,3]
  reg_m2s2_table[1,i+1] <- fit$coefficients[[1]]
  reg_m2s2_table[2,i+1] <- t_vals[1]
  reg_m2s2_table[3,i+1] <- fit$coefficients[[2]]
  reg_m2s2_table[4,i+1] <- t_vals[2]
  reg_m2s2_table[5,i+1] <- fit$coefficients[[3]]
  reg_m2s2_table[6,i+1] <- t_vals[3]
  
  reg_m2s2_table[7,i+1] <- fit$coefficients[[4]]
  reg_m2s2_table[8,i+1] <- t_vals[4]
  
  reg_m2s2_table[9,i+1] <- fit$coefficients[[5]]
  reg_m2s2_table[10,i+1] <- t_vals[5]
  
  reg_m2s2_table[11,i+1] <- fit$coefficients[[6]]
  reg_m2s2_table[12,i+1] <- t_vals[6]
  
  reg_m2s2_table[13,i+1] <- fit$coefficients[[7]]
  reg_m2s2_table[14,i+1] <- t_vals[7]
  
  reg_m2s2_table[15,i+1] <- fit$coefficients[[8]]
  reg_m2s2_table[16,i+1] <- t_vals[8]
  
  reg_m2s2_table[17,i+1] <- fit$coefficients[[9]]
  reg_m2s2_table[18,i+1] <- t_vals[9]
  
  reg_m2s2_table[19,i+1] <- fit$adj.r.squared[1]
  reg_m2s2_table[20,i+1] <- sum(fit$residuals^2)
  
  rm(fit)
}
colnames(reg_m2s2_table) <- c("",ctry_list)
reg_m2s2_table



####### POST CRISIS REGRESSIONS  #######

## Baseline model - POST CRISIS
reg_m1s3_table <- tibble(
  Variable = c("constant", "t stat", "spread_l", "t stat", "reer", "t stat", "vix", "t stat", 
               "PC2", "t stat", "Adj. R2", "ssr"))

for(i in 1:length(ctry_tbls)){
  ctry <- ctry_tbls[i]
  ctry <- ctry[[1]] %>% filter(X > 144) 
  model <- lm(spread ~ lag(spread) + reer + vix + PC2_S3, ctry)
  fit <- summary(model)
  t_vals <- coeftest(model, vcov = vcovHC(model, type = "HC0"))[,3]
  reg_m1s3_table[1,i+1] <- fit$coefficients[[1]]
  reg_m1s3_table[2,i+1] <- t_vals[1]
  reg_m1s3_table[3,i+1] <- fit$coefficients[[2]]
  reg_m1s3_table[4,i+1] <- t_vals[2]
  reg_m1s3_table[5,i+1] <- fit$coefficients[[3]]
  reg_m1s3_table[6,i+1] <- t_vals[3]
  reg_m1s3_table[7,i+1] <- fit$coefficients[[4]]
  reg_m1s3_table[8,i+1] <- t_vals[4]
  reg_m1s3_table[9,i+1] <- fit$coefficients[[5]]
  reg_m1s3_table[10,i+1] <- t_vals[5]
  reg_m1s3_table[11,i+1] <- fit$adj.r.squared[1]
  reg_m1s3_table[12,i+1] <- sum(fit$residuals^2)
  rm(fit)
}
colnames(reg_m1s3_table) <- c("",ctry_list)
reg_m1s3_table

## Extended model - POST CRISIS
reg_m2s3_table <- tibble(
  Variable = c("constant", "t stat", "spread_l", "t stat", "reer", "t stat", 
               "vix", "t stat", "bidask", "t stat", 
               "indgrowth", "t stat", "exp_govbal", "t stat",
               "exp_debtgdp", "t stat", "PC2", "t stat", "Adj. R2", "ssr"))

for(i in 1:length(ctry_tbls)){
  ctry <- ctry_tbls[i]
  ctry <- ctry[[1]] %>% filter(X > 144) 
  model <- lm(spread ~ lag(spread) + reer + vix + 
                bidask + ind_growth + exp_debtgdp + exp_govbal + PC2_S3, ctry)
  fit <- summary(model)
  t_vals <- coeftest(model, vcov = vcovHC(model, type = "HC0"))[,3]
  reg_m2s3_table[1,i+1] <- fit$coefficients[[1]]
  reg_m2s3_table[2,i+1] <- t_vals[1]
  reg_m2s3_table[3,i+1] <- fit$coefficients[[2]]
  reg_m2s3_table[4,i+1] <- t_vals[2]
  reg_m2s3_table[5,i+1] <- fit$coefficients[[3]]
  reg_m2s3_table[6,i+1] <- t_vals[3]
  
  reg_m2s3_table[7,i+1] <- fit$coefficients[[4]]
  reg_m2s3_table[8,i+1] <- t_vals[4]
  
  reg_m2s3_table[9,i+1] <- fit$coefficients[[5]]
  reg_m2s3_table[10,i+1] <- t_vals[5]
  
  reg_m2s3_table[11,i+1] <- fit$coefficients[[6]]
  reg_m2s3_table[12,i+1] <- t_vals[6]
  
  reg_m2s3_table[13,i+1] <- fit$coefficients[[7]]
  reg_m2s3_table[14,i+1] <- t_vals[7]
  
  reg_m2s3_table[15,i+1] <- fit$coefficients[[8]]
  reg_m2s3_table[16,i+1] <- t_vals[8]
  
  reg_m2s3_table[17,i+1] <- fit$coefficients[[9]]
  reg_m2s3_table[18,i+1] <- t_vals[9]
  
  reg_m2s3_table[19,i+1] <- fit$adj.r.squared[1]
  reg_m2s3_table[20,i+1] <- sum(fit$residuals^2)
  rm(fit)
}
colnames(reg_m2s3_table) <- c("",ctry_list)
reg_m2s3_table

write.xlsx(reg_m1s1_table, file="Estimation/est_results.xlsx", 
           sheetName="m1s1")

write.xlsx(reg_m2s1_table, file="Estimation/est_results.xlsx", 
           sheetName="m2s1", append = TRUE)

write.xlsx(reg_m1s2_table, file="Estimation/est_results.xlsx", 
           sheetName="m1s2", append = TRUE)

write.xlsx(reg_m2s2_table, file="Estimation/est_results.xlsx", 
           sheetName="m2s2", append = TRUE)

write.xlsx(reg_m1s3_table, file="Estimation/est_results.xlsx", 
           sheetName="m1s3", append = TRUE)

write.xlsx(reg_m2s3_table, file="Estimation/est_results.xlsx", 
           sheetName="m2s3", append = TRUE)

