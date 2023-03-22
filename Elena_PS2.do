cd "C:\Users\elena\OneDrive\Desktop\ESS\2nd year\Microeconometrics\Micrometrics_PS"
use pset_2_q_2_and_3.dta, clear

*3(a) - OLS
tab birthqtr, gen(Quarter)
tab region, gen(Region)
tab birthyear, gen(Birth)

reg Healthy Education

local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
reg Healthy Education `Controls' `Birth_Year_FEs'
outreg2 using TABLE_Q_3.xls, excel replace keep(Education) nocons ///
addtext(Controls, YES, Reg, OLS) ///


*ssc install ivreg2, replace

*3(b) - FIRST STAGE
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust first savefirst
scalar F_weak = e(widstat)
est restore _ivreg2_Education
outreg2 using TABLE_Q_3.xls, excel append ///
keep(Quarter1 Quarter2 Quarter3) nocons ///
addtext(Controls, YES, Reg, First Stage) ///
addstat("F-statistic instruments", F_weak)


*3(c) - REDUCED FORM
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs'
outreg2 using TABLE_Q_3.xls, excel append /// 
keep(Quarter1 Quarter2 Quarter3) nocons ///
addtext(Controls, YES, Reg, Reduced Form) 

///ADD COMMENTS LINKED TO THE RESULTS IN 2(e)


*3(d) - SECOND STAGE
* Estimate the second stage.
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) ///
`Controls' `Birth_Year_FEs', robust
outreg2 using TABLE_Q_3.xls, excel append ///
keep(Education) nocons ///
addtext(Controls, YES, Reg, IV) 


*3(e) - ADD COMMENTS


*3(f) 


tab bpl, gen(State)
local State_FEs "State1-State50"

tab birthdate, gen(Year_quarter)
local Year_Quarter_FEs "Year_quarter1-Year_quarter39"

egen State_quarter = concat(bpl birthqtr), decode punct(-)
tab State_quarter, gen(State_quarter)
local State_Quarter_FEs "State_quarter1-State_quarter203"


*3(g)
local Year_Quarter_FEs "Year_quarter1-Year_quarter39"
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
ivreg2 Healthy (Education = `Year_Quarter_FEs') ///
`Controls' `Birth_Year_FEs', robust
scalar F_test_Year_Quarter = e(widstat)

local State_Quarter_FEs "State_quarter1-State_quarter203"
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
local State_FEs "State1-State50"
ivreg2 Healthy (Education = `State_Quarter_FEs') ///
`Controls' `Birth_Year_FEs' `State_FEs', robust 
scalar F_test_State_Quarter = e(widstat)

*3(h)
display F_test_Year_Quarter 
*7.9607721

display F_test_State_Quarter
*3.1663298





















