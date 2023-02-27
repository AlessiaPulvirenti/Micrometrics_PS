********************************************
*	Microeconometrics - Problem Set 1
********************************************

*	Author: Alessia Pulvirenti
*	Date: February 25, 2022

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
cd "C:\Users\pulvi\OneDrive - Università Commerciale Luigi Bocconi\Depr(ESS)ion\Second Year\Micrometrics\PS\Micrometrics_PS\PS1"
use "jtrain3.dta", clear
/*
	(a) Do a table with the same structure of TABLE 1 of item (a) in question 1 for the following covariates: age educ black hisp 	re74 re75 (note that nodegree is not present in the current dataset.) Add the corresponding columns to TABLE 1.
*/

local control_vars age educ black hisp re75 re74 
balancetable train `control_vars' using TABLE_1_Ale.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(H1)		//Note that, with option "wide" we can put the standard deviation of T and C next to the column of T and C, rather than in brackets below the values for the mean. 


/* (b) Generate a variable named treated that randomly allocates half of observations to a (fake) treatment group and the other half to a (fake) control group. Fix a seed of 5 digits using the command set seed.
*/

* Generate a treatment variable 
gen treatment= .

* Fix a 5-digit seed 
set seed 12345

replace treatment = 1 if runiform() < 0.5
replace treatment = 0 if missing(treatment) 

















