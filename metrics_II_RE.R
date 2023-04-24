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
library('MCMCpack')
library(kableExtra)


# set your working directory here
setwd(dir = "/Users/frankzhu/Desktop/Fama-French-ESG-Panel")

# read in excel 
df_panel <- read_excel("data_complete_ff.xlsx")
head(df_panel)

# Cleaning
df_panel <- df_panel %>% rename(Industry = icb_industry_code)
df_panel = df_panel[df_panel$env_dim != 0,]
df_panel = df_panel[df_panel$social_dim != 0,]
df_panel$monthly_return_100 = df_panel$monthly_return * 100
df_panel['monthly_excess_return_100'] = df_panel['monthly_return_100'] - df_panel['RF_10']
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
df_panel_2 = df_panel[monthly_return]*100
random_ind_all = plm(monthly_return*100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, model="random", data=df_panel)
summary(random_ind_all)
# stargazer output
stargazer(random_dimensional_score, random_general_score, random_dimensional_score_ind, random_general_score_ind, random_general_score_ind_interaction, random_ind_env, random_ind_soc, random_ind_gov, random_ind_all, type = "text", title = "Random Effects", out = "./reg_results/random_effects.txt")
stargazer(random_dimensional_score, random_general_score, random_dimensional_score_ind, random_general_score_ind, random_general_score_ind_interaction, random_ind_env, random_ind_soc, random_ind_gov, random_ind_all, type = "html", title = "Random Effects", out = "./reg_results/random_effects.html")


#---------------------------------#
## pooled regression
pooled_1 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score, model="pooling", data=df_panel)
summary(pooled_1)
pooled_2 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score+Industry, model="pooling", data=df_panel)
summary(pooled_2)
pooled_3 = plm(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry, model="pooling", data=df_panel)
summary(pooled_3)
# export using stargazer
stargazer(pooled, type = "text", title = "Pooled OLS", out = "./reg_results/pooled_ols.txt")
stargazer(pooled, type = "html", title = "Pooled OLS", out = "./reg_results/pooled_ols.html")

#---------------------------------#
## fixed effects
fixed = plm(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, model="within", data=df_panel)
summary(fixed)
# stargazer
stargazer(fixed, type = "text", title = "Fixed Effects", out = "./reg_results/fixed_effects.txt")
stargazer(fixed, type = "html", title = "Fixed Effects", out = "./reg_results/fixed_effects.html")



#---------------------------------#
# lagrange multiplier test for Common Effects (Pooling or FE/RE)
plmtest(pooled_1, type ="bp")
plmtest(pooled_2, type ="bp")
plmtest(pooled_3, type ="bp")

#---------------------------------#
# random effects models to be tested
re_1 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score, model="random", data=df_panel)
summary(re_1)
re_2 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score+Industry, model="random", data=df_panel)
summary(re_2)
re_3 = plm(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, model="random", data=df_panel)
summary(re_3)

#---------------------------------#
# fixed effects models to be tested
fe_1 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score, model="within", data=df_panel)
summary(fe_1)
fe_2 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score+Industry, model="within", data=df_panel)
summary(fe_2)
fe_3 = plm(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, model="within", data=df_panel)
summary(fe_3)

# B-P Hausman test
phtest(re_1, fe_1)
phtest(re_2, fe_2)
phtest(re_3, fe_3)

#---------------------------------#
# Summary Stats & Kable output
kable(summary(df_panel[c("monthly_excess_return_100","risk_premium","SMB","HML","sp_esg_score","env_dim","social_dim","economic_gov_dim")]), 
      caption = "Summary Statistics") %>%
  kable_styling(latex_options = "scale_down") %>%
  add_header_above(c(" " = 1, "Summary Statistics" = 8)) %>%
  footnote(general = "Calculations using data from CRSP and WRDS") %>%
  kable_paper(full_width = F)
sum(df_panel$env_dim == 0)

#---------------------------------#
# Random Effects with Robust SE
## Baseline Model
re_1 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score, model="random", data=df_panel, random.method = "walhus")
apply(df_panel[c("monthly_excess_return_100","risk_premium","SMB","HML","sp_esg_score","Industry","env_dim","social_dim","economic_gov_dim")], 2, function(x) sum(x == 0))summary(re_1)
robust=coeftest(re_1, vcovHC)
summary(robust)
stargazer(robust, type="latex", 
          keep.stat=c("n","rsq"), out = "./reg_results/baseline_re_robust.tex")

## Industry Effects
re_2 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score+Industry, model="random", data=df_panel)
summary(re_2)
robust2=coeftest(re_2, vcovHC)
summary(robust2)
stargazer(robust2, type="latex", 
          keep.stat=c("n","rsq"), out = "./reg_results/baseline_re_robust_ind.tex")
stargazer(robust2, type="text", 
          keep.stat=c("n","rsq"))

## Dimensional Decomposition with Industry Interactions
re_3 = plm(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, model="random", data=df_panel)
summary(re_3)
robust3=coeftest(re_3, vcovHC)
summary(robust3)
stargazer(robust3, type="latex", 
          keep.stat=c("n","rsq"), out = "./reg_results/Decomp_re_robust_ind_inter.tex")
stargazer(robust3, type="text", 
          keep.stat=c("n","rsq"))


## Baseline Model with Industry Interactions
robust4 = plm(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score+Industry*sp_esg_score, model="random", data=df_panel)
summary(robust4)
stargazer(robust4, type="latex", 
          keep.stat=c("n","rsq"), out = "./reg_results/baseline_re_robust_ind_inter.tex")


## Bayesian Estimation
# Baseline
baseline_bayes = MCMCregress(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score, data=df_panel)
summary(baseline_bayes)
plot(baseline_bayes)
# Baseline w/ industry 
posterior2 = MCMCregress(monthly_excess_return_100~risk_premium+SMB+HML+sp_esg_score+Industry, data=df_panel)
summary(posterior2)
plot(posterior2)
# Decomposition w/ industry interaction
posterior3 = MCMCregress(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, data=df_panel)
summary(posterior3)
plot(posterior3)
# Baseline w/ industry interactions
posterior4 = MCMCregress(monthly_excess_return_100~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim+Industry*env_dim+Industry*social_dim+Industry*economic_gov_dim, data=df_panel)
summary(posterior4)
plot(posterior4)



#---------------------------------#
#SUR Model (Deprecated)

data("Grunfeld", package = "AER")
library("plm")
library("systemfit")
library("stargazer")
library("readxl")
library(tidyverse)
library(texreg)

df_panel_telecom = df_panel[df_panel$Industry == 'TELECOM',]

esg_sur =systemfit(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, method = "SUR", data = df_panel_telecom_no_ind)
summary(esg_sur)

head(df_panel_telecom_no_ind)

f_panel_telecom_telecom = df_panel[df_panel$Industry == 'TELECOM',]
df_panel_telecom_no_ind = df_panel_telecom[,-c(9)]
df_panel_telecom_no_ind = df_panel_telecom_no_ind [, -c(4)]
df_panel_telecom_no_ind = df_panel_telecom_no_ind [, -c(11)]

df_panel_telecom_no_ind= plm.data(df_panel_telecom_no_ind, c("ticker", "date"))

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
#---------------------------------#
#SUR Model

data("Grunfeld", package = "AER")
library("plm")
library("systemfit")
library("stargazer")
library("readxl")
library(tidyverse)
library(texreg)

df_panel_telecom = df_panel[df_panel$Industry == 'TELECOM',]

esg_sur =systemfit(monthly_return~risk_premium+SMB+HML+economic_gov_dim+env_dim+social_dim, method = "SUR", data = df_panel_telecom_no_ind)
summary(esg_sur)

head(df_panel_telecom_no_ind)

f_panel_telecom_telecom = df_panel[df_panel$Industry == 'TELECOM',]
df_panel_telecom_no_ind = df_panel_telecom[,-c(9)]
df_panel_telecom_no_ind = df_panel_telecom_no_ind [, -c(4)]
df_panel_telecom_no_ind = df_panel_telecom_no_ind [, -c(11)]

df_panel_telecom_no_ind= plm.data(df_panel_telecom_no_ind, c("ticker", "date"))

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