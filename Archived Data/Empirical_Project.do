
cap log using Empirical_Project.log, replace

use "Final_Data.dta", clear

*Model without ESG
reg yearly_return b_mkt_fama_french_3fac1 mktcap book_to_market1

*Check for heteroscedasticity
estat hettest

*Model without ESG factor with robust standard errors
reg yearly_return b_mkt_fama_french_3fac1 mktcap book_to_market1, r

*Model with ESG factor
reg yearly_return b_mkt_fama_french_3fac1 mktcap book_to_market1 SPGlobalESGScore, r

*Model with ESG factor and industry categorical variable
reg yearly_return b_mkt_fama_french_3fac1 mktcap book_to_market1 SPGlobalESGScore i.industry, r

cap close log

