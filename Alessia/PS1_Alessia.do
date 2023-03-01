********************************************
*	Microeconometrics - Problem Set 1
********************************************

*	Author: Alessia Pulvirenti
*	Date: February 25, 2022
***************************************************************************************************************************************
**# QUESTION 1 

* (a)

use "jtrain2.dta", clear

*	Construct a balance table
ssc install balancetable, replace

help balancetable

local control_vars age educ black hisp nodegree re75 re74 
balancetable train `control_vars' using TABLE_1_Ale.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames replace		//Note that, with option "wide" we can put the standard deviation of T and C next to the column of T and C, rather than in brackets below the values for the mean. 

* COMMENT ON HOW MANY VARIABLES ARE BALANCED OR NOT. IS THAT WHAT YOU EXPECTED? (TO DO)

/* (b) 
Regress re78 on train. Save the estimate and the standard error of the coefficient on train as scalars. Interpret the coefficient.
*/
reg re78 train

scalar train_coef = _b[train]
scalar train_se = _se[train]

di train_coef
di train_se

* WRITE HERE INTERPRETATION OF THE COEFFICIENT



/*(c) 
Construct a table by sequentially adding the output of the following regressions
to each column:
(1) re78 on train;
(2) re78 on train age educ black hisp;
(3) re78 on train age educ black hisp re74 re75;
Add rows to the table with the number of controls and treated in each regression.
Name it TABLE_2.
Are your results sensitive to the introduction of covariates?*/

quietly reg re78 train
outreg2 using TABLE_2.xls, excel replace

quietly reg re78 train age educ black hisp
outreg2 using TABLE_2.xls, excel append

quietly reg re78 train age educ black hisp re74 re75
outreg2 using TABLE_2.xls, excel append

* COMMENT IF RESULTS ARE SENSITIVE TO THE INTRODUCTION OF COVARIATES

/*(d) 
dfbeta is a statistic that measures how much the regression coefficient of a certain variable changes in standard deviations if the i-th observation is deleted.
Type help dfbeta and discover how to estimate this statistic after a regression.
Generate a variable named influence train storing the dfbetas of train of the last regression you did in point (c).
Redo the last regression you did in point (c) but removing the observations with the 3, 5, and 10 lowest and largest values in influence train. Are your results sensitive to in infuential observations?*/

qui help dfbeta

reg re78 train age educ black hisp re74 re75
dfbeta train, stub(influence_train)

rename influence_train1 influence_train

sort influence_train

list influence_train in 1/3
list influence_train


reg re78 train age educ black hisp re74 re75 if influence_train > influence_train[3] & influence_train < influence_train[443]
reg re78 train age educ black hisp re74 re75 if influence_train > influence_train[5] & influence_train < influence_train[441]
reg re78 train age educ black hisp re74 re75 if influence_train > influence_train[10] & influence_train < influence_train[436]


*	COMMENT ON THE DIFFERENT RESULTS OF THE REGRESSION

**# QUESTION 2
cd "C:/Users/pulvi/OneDrive - Università Commerciale Luigi Bocconi/Depr(ESS)ion/Second Year/Micrometrics/PS/Micrometrics_PS/Alessia"
use "jtrain3.dta", clear
/*
	(a) Do a table with the same structure of TABLE 1 of item (a) in question 1 for the following covariates: age educ black hisp 	re74 re75 (note that nodegree is not present in the current dataset.) Add the corresponding columns to TABLE 1.
*/

local control_vars age educ black hisp re75 re74 
balancetable train `control_vars' using TABLE_1_Ale.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(A20)		//Note that, with option "wide" we can put the standard deviation of T and C next to the column of T and C, rather than in brackets below the values for the mean. 


/* (b) Generate a variable named treated that randomly allocates half of observations to a (fake) treatment group and the other half to a (fake) control group. Fix a seed of 5 digits using the command set seed.
*/

* Generate a treatment variable 
gen treated= .

* Fix a 5-digit seed 
set seed 12345

replace treated = 1 if runiform() < 0.5
replace treated = 0 if missing(treated)

/*(c) Type ssc install randtreat. Then, read randtreat help file. Redo point (b) using the command randtreat.
Name treated_2 your new (fake) treatment variable. Check whether the correlation between treated 2 and treated is statistically
significant or not. (Hint: use pwcorr X Y, sig)
*/

ssc install randtreat, replace

qui help randtreat

randtreat, generate(treated_2) replace setseed(12345)

pwcorr treated treated_2, sig star(.05)

/*
pwcorr treatment treatment_2, sig

             | treatm~t treatm~2
-------------+------------------
   treatment |   1.0000 
             |
             |
 treatment_2 |   0.0676   1.0000 
             |   0.1549
             |

			 
			 
Correlation between treatment and treament_2 is 0.0676, while significance is 0.1549
*/

/*(d) Do a table with the same structure of TABLE 1 of item (a) in question 1., but using treated instead of train.
Use the same list of covariates of item (a) of this question. Add the corresponding columns to TABLE 1.
What you find corresponds to your expectations?
*/

local control_vars age educ black hisp re75 re74 
balancetable treated `control_vars' using TABLE_1_Ale.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(H20)		//Note that, with option "wide" we can put the standard deviation of T and C next to the column of T and C, rather than in brackets below the values for the mean.

/*
WHAT YOU FIND CORRESPONDS TO YOUR EXPECTATIONS?
*/

/*(e) Sequentially add the output of the following regressions to TABLE 2:
(1) re78 on treated;
(2) re78 on treated age educ black hisp;
(3) re78 on treated age educ black hisp re74 re75.
Add lines in the table with the number of controls and treated in each regression. Comment on what you find. Is it what you expected?
*/

local x_1 treated
local x_2 age educ black hisp
local x_3 re74 re75 
local x_4 train

reg re78 `x_1'
tabstat `x_1' if e(sample)==1, statistics(count) by(`x_1') save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append 
*
reg re78 `x_1' `x_2'
tabstat `x_1' if e(sample)==1, statistics(count) by(`x_1') save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append
*
reg re78 `x_1' `x_2' `x_2'
tabstat `x_1' if e(sample)==1, statistics(count) by(`x_1') save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append


/*(f)
(f) Sequentially add the output of the following regressions to TABLE 2:
(1) re78 on train;
(2) re78 on train age educ black hisp;
(3) re78 on train age educ black hisp re74 re75.
Add lines in the table with the number of controls and treated in each regression.
Compare the results with the first three columns of TABLE 2. Comment on what you find. Is it what you expected? Are your results sensitive to the introduction of covariates?
*/
reg re78 `x_4'
tabstat `x_1' if e(sample)==1, statistics(count) by(`x_1') save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append 
*
reg re78 `x_4' `x_2'
tabstat `x_1' if e(sample)==1, statistics(count) by(`x_1') save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append
*
reg re78 `x_4' `x_2' `x_3' 
tabstat `x_1' if e(sample)==1, statistics(count) by(`x_1') save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append



**# QUESTION 3

/*(a) Under which conditions, allowing for heterogeneous treatment effects, is Neyman's inference unbiased?
*/

/*Neyman's inference is based on the estimation of the average treatment effect for the sample at hand, and to construct confidence intervals for the average treatment effect. 
Assuming a FINITE SAMPLE, with no particular assumption on the construction of the sample itself, an unbiased estimatore found by Neyman consists simply in the difference in average outcomes by treatment status. Assuming potential outcomes are fixed, so that the only stochastic component is the treatment status, D_i, Neyman construct an estimator /tau_hat which is unbiased for /tau, the average treatment effect. 

However, Imbens and Rubin 2015, show that the sampling variance of the estimator of the average treatment effect derived by Neyman, over the randomisation distribution is composed of three elements: 
1. the variance of the potential outcomes Y_i(0) over the no. of control
2. the variance of the potential outcomes Y_i(0) over the no. of treated
3. the population variance of the unit level treatment effect, i.e., the pop variance of Y_i(1) - Y_i(0), over the whole sample. We know thta this term is unobservable, because we never observe Y_i(1) and Y_i(0) for the same unit. 

So the variance of Neyman's estimator /tau_hat is usually upward biased, leading to conservative confidence intervals for /tau_hat. There are two cases in which the bias of the sampling variance of the estimator disappears: 
1) The third element, i.e., the population variance of Y_i(1) - Y_i(0) is 0 whenever the TREATMENT EFFECT IS CONSTANT across all units. 
2) 













