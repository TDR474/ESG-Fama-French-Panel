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

# set your working directory here
# setwd(dir = "/Users/frankzhu/Desktop/ESG-Fama-French")

# read in excel 
df_panel <- read_excel("data_complete_ff.xlsx")
head(df_panel)
# a new column for excess return (snp_monthly_return-ten_yr_ytm_rf)
#df_panel <- df_panel %>% mutate(risk_premium = mkt_rf)
# rename icb_industry_code to Industry
df_panel <- df_panel %>% rename(Industry = icb_industry_code)

#df_panel <- pdata.frame(mydata, index = "t")
# print the columns in df_panel
colnames(df_panel)

## random effects
# Fama-French 3 factors
#https://stackoverflow.com/questions/45121817/plm-package-in-r-empty-model-when-including-only-variables-without-variation-o
#random_ff = plm(monthly_return~risk_premium+SMB+HML, model="random", data=df_panel)
#summary(random_ff)
# Dimensional (multiple scores) base model
random_dimensional_score = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, model="random", data=df_panel)
summary(random_dimensional_score)
# General ESG Score
random_general_score = plm(monthly_return~risk_premium+SMB+HML+sp_esg_score, model="random", data=df_panel)
summary(random_general_score)

# With industries
random_dimensional_score_ind = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry, model="random", data=df_panel)
summary(random_dimensional_score_ind)
random_general_score_ind = plm(monthly_return~risk_premium+SMB+HML+sp_esg_score+Industry, model="random", data=df_panel)
summary(random_general_score_ind)

# General ESG Score with industry interaction
random_general_score_ind_interaction = plm(monthly_return~risk_premium+SMB+HML+sp_esg_score+Industry*sp_esg_score, model="random", data=df_panel)
summary(random_general_score_ind_interaction)

# environmental dimension interaction
random_ind_env = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*env_dim, model="random", data=df_panel)
summary(random_ind_env)
# social dimension interaction
random_ind_soc = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*social_dim, model="random", data=df_panel)
summary(random_ind_soc)
# economic governmenance dimension interaction
random_ind_gov = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*economic_gov_dim, model="random", data=df_panel)
summary(random_ind_gov)
# all three dimensions interaction
random_ind_all = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, model="random", data=df_panel)
summary(random_ind_all)
# stargazer ourtput
stargazer(random_dimensional_score, random_general_score, random_dimensional_score_ind, random_general_score_ind, random_general_score_ind_interaction, random_ind_env, random_ind_soc, random_ind_gov, random_ind_all, type = "text", title = "Random Effects", out = "./reg_results/random_effects.txt")
stargazer(random_dimensional_score, random_general_score, random_dimensional_score_ind, random_general_score_ind, random_general_score_ind_interaction, random_ind_env, random_ind_soc, random_ind_gov, random_ind_all, type = "html", title = "Random Effects", out = "./reg_results/random_effects.html")


#---------------------------------#
## pooled regression
pooled = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, model="pooling", data=df_panel)
summary(pooled)
# export using stargazer
stargazer(pooled, type = "text", title = "Pooled OLS", out = "./reg_results/pooled_ols.txt")
stargazer(pooled, type = "html", title = "Pooled OLS", out = "./reg_results/pooled_ols.html")

## fixed effects
fixed = plm(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, model="within", data=df_panel)
summary(fixed)
# stargazer
stargazer(fixed, type = "text", title = "Fixed Effects", out = "./reg_results/fixed_effects.txt")
stargazer(fixed, type = "html", title = "Fixed Effects", out = "./reg_results/fixed_effects.html")

# The specification test devised by Hausman (1978). A formal test of
# fixed effects versus random effects in modelling panel data can be
# based on the fundamental assumption that the random effects, ei are
# considered to be independent of the explanatory variables, xit , that is
# E(ei xit) = 0.
# H0 : random effects
# H1 : fixed effects.
# • Under the hypothesis of no correlation, both FE (LSDV) and RE (FGLS)
# estimators are consistent, but FE is inefficient, whereas under the
# alternative, FE is consistent, but RE is not.

# The null hypothesis of the Hausman test is that the random effects (RE) estimator
# is indeed an efficient (and consistent) estimator of the true parameters. If this is
# the case, there should be no systematic difference between the RE and FE
# estimators and the RE estimator would be preferred as the more efficient
# technique. In contrast, if the null is rejected, the  fixed effect estimator needs to
# be applied.
phtest(fixed, random)
# > phtest(fixed, random)

#         Hausman Test

# data:  monthly_return ~ risk_premium + SMB + HML + economic_gov_dim +  ...
# chisq = 1.0633, df = 3, p-value = 0.7859

# alternative hypothesis: one model is inconsistent


# the null hypothesis that the difference in the coefficients is not systematic is
# NOT rejected at the 1% level, implying that the random effects model is not
# appropriate and that the random effects specification is to be preferred