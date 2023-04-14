library("corrplot")
library("lmtest")
library("sandwich")
library("readxl")
library("stargazer")
library("lessR")
library("ggplot2")
library("dplyr")
library(e1071)
library(data.table)
library(car)
library(caTools)
library(mctest)
library("writexl")
library('plm')
library(pedometrics)
#data_2021_renamed

# set your working directory here
# setwd(dir = "/Users/frankzhu/Desktop/ESG-Fama-French")

# read in excel 
df_panel <- read_excel("data_complete_ff.xlsx")

head(df_panel)

# a new column for excess return (snp_monthly_return-ten_yr_ytm_rf)
df_panel <- df_panel %>% mutate(risk_premium = snp_monthly_return - ten_yr_ytm_rf)


pooled = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, model="pooling", data=df_panel)
summary(pooled)




