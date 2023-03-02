											*******************************
											***							***
											***		Problem Set 1		***
											***		GROUP 8				***
											***		Aleksa Mitrovic		***
											***		Elena Neri			***
											***		Alessia Pulvirenti	***
											***		Tommaso Roccuzzo	***
											***							***
											*******************************
											
											
cd "/Users/tommasoroccuzzo/Library/Mobile Documents/com~apple~CloudDocs/Tommaso/Appunti/Bocconi/2021-2022/2° Semestre/Microeconometrics/Problem Sets/PS 1"

clear


************************************************	QUESTION 1 		************************************************

quietly{

cd "C:\Users\pulvi\OneDrive - Università Commerciale Luigi Bocconi\Depr(ESS)ion\Second Year\Micrometrics\PS\Micrometrics_PS\PS1 - submission"
use jtrain2.dta

***********    a    ***********

ssc install balancetable, replace

help balancetable

local control_vars age educ black hisp nodegree re75 re74 
balancetable train `control_vars' using TABLE_1.xls, ctitle("Mean C" "Sd C" "Mean T" " Sd T" "Diff. in means" "Sd Error") vce(robust)  leftctitle("Variable") varnames wide replace		//Note that, with option "wide" we can put the standard deviation of T and C next to the column of T and C, rather than in brackets below the values for the mean. 



* Alternative method, using the matrix function

/*
matrix balance = (.,.,.,.,.,.,.,.)

local i = 1

foreach var of varlist age educ black hisp nodegree re74 re75 {
	
	quietly sum `var' if train == 1
	
	matrix balance[`i',1]=r(mean)
	matrix balance[`i',3]=r(sd)
	
	
	quietly sum `var' if train == 0
	
	matrix balance[`i',2]=r(mean)
	matrix balance[`i',4]=r(sd)
	
	
	quietly regress `var' train, robust
	matrix rtable = r(table)

	matrix balance[`i',5] = rtable[1,1]
	matrix balance[`i',6] = rtable[2,1]
	matrix balance[`i',7] = rtable[3,1]
	matrix balance[`i',8] = rtable[4,1]
	
	local i = `i'+1
	
	if `i'<8  matrix balance=(balance \ .,.,.,.,.,.,.,.)
	
}

matrix rownames balance = age educ black hisp nodegree re74 re75
matrix colnames balance = mean_T mean_C StdDev_T StdDev_C Diff StdE_Diff t-stat p-value


matrix list balance

putexcel set TABLE_1_ok.xls, replace

putexcel A1=matrix(balance), hcenter names
putexcel A9 = "Observations"
quietly sum age if train == 1, d
scalar N_t = r(N)
putexcel B9 = N_t

quietly sum age if train == 0, d
scalar N_c = r(N)
putexcel C9 = N_c
*/




***********    b    ***********


reg re78 train

scalar train_coef = _b[train]
scalar train_se = _se[train]

di train_coef
di train_se

/*
di train_coef
1.7943431

di train_se
.63285355
*/

***********    c    ***********


qui reg re78 train, vce(robust)
estimates store Regression_1

qui reg re78 train age educ black hisp, vce(robust)
estimates store Regression_2

qui reg re78 train age educ black hisp re74 re75, vce(robust)
estimates store Regression_3

qui sum train if train == 0
scalar N_C = r(N)
qui sum train if train == 1
scalar N_T = r(N)

outreg2 [Regression_1 Regression_2 Regression_3] using TABLE_2.xls, nocons adds(# of units in Treatment, N_T, # of units in Control, N_C) excel replace ctitle("") 


***********    d    ***********


help dfbeta

local x_1 "train age educ black hisp re74 re75"
reg re78 `x_1' 
dfbeta, stub(beta)
rename beta1 influence_train 

sort influence_train
list influence_train if inrange(_n, 1, 10) | inrange(_n, _N - 9, _N) 

local x_1 "train age educ black hisp re74 re75"
reg re78 `x_1' if influence_train > influence_train[3] & influence_train < influence_train[443]
reg re78 `x_1' if influence_train > influence_train[5] & influence_train < influence_train[441]
reg re78 `x_1' if influence_train > influence_train[10] & influence_train < influence_train[436]

}


************************************************	QUESTION 2 		************************************************

quietly{
	
use jtrain3.dta, clear


***********    a    ***********

local control_vars age educ black hisp re75 re74 
balancetable train `control_vars' using TABLE_1.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(A11)	wide	

/*
matrix balance2 = (.,.,.,.,.,.,.,.)

local i = 1

foreach var of varlist age educ black hisp re74 re75 {
	
	quietly sum `var' if train == 1, d
	
	matrix balance2[`i',1]=r(mean)
	matrix balance2[`i',3]=r(sd)

	
	quietly sum `var' if train == 0, d
	
	matrix balance2[`i',2]=r(mean)
	matrix balance2[`i',4]=r(sd)
	
	
	quietly regress `var' train, robust
	matrix rtable = r(table)

	matrix balance2[`i',5] = rtable[1,1]
	matrix balance2[`i',6] = rtable[2,1]
	matrix balance2[`i',7] = rtable[3,1]
	matrix balance2[`i',8] = rtable[4,1]
	
	local i = `i'+1
	
	if `i'<7  matrix balance2=(balance2 \ .,.,.,.,.,.,.,.)
	
}

matrix rownames balance2 = age educ black hisp re74 re75
matrix colnames balance2 = mean_T mean_C StdDev_T StdDev_C Diff StdE_Diff t-stat p-value

matrix list balance2

matrix top = balance2[1..4,1..8]
matrix list top
matrix bot = balance2[5..6,1..8]
matrix list bot


putexcel set TABLE_1_ok.xls, modify
putexcel K1=matrix(top), hcenter colnames
putexcel K7=matrix(bot), hcenter

quietly sum age if train == 1, d
scalar N_t = r(N)
putexcel K9 = N_t

quietly sum age if train == 0, d
scalar N_c = r(N)
putexcel L9 = N_c


*/

***********    b    ***********


set seed 88888

generate treated = .

generate random = runiform()

sort random

generate index = _n

qui sum age
scalar cutoff = r(N)/2

replace treated = 0 if index <= cutoff
replace treated = 1 if index > cutoff & index<=cutoff*2


***********    c    ***********


randtreat, generate(treated_2) setseed(88888)

pwcorr treated treated_2, sig star(.05)


***********    d    ***********

local control_vars age educ black hisp re75 re74 
balancetable treated `control_vars' using TABLE_1_Ale.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(H11) wide		


/*
matrix balance3 = (.,.,.,.,.,.,.,.)

local i = 1

foreach var of varlist age educ black hisp re74 re75 {
	
	quietly sum `var' if treated == 1, d
	
	matrix balance3[`i',1]=r(mean)
	matrix balance3[`i',3]=r(sd)

	
	quietly sum `var' if treated == 0, d
	
	matrix balance3[`i',2]=r(mean)
	matrix balance3[`i',4]=r(sd)
	
	
	quietly regress `var' treated, robust
	matrix rtable = r(table)

	matrix balance3[`i',5] = rtable[1,1]
	matrix balance3[`i',6] = rtable[2,1]
	matrix balance3[`i',7] = rtable[3,1]
	matrix balance3[`i',8] = rtable[4,1]
	
	local i = `i'+1
	
	if `i'<7  matrix balance3=(balance3 \ .,.,.,.,.,.,.,.)
	
}

matrix rownames balance3 = age educ black hisp re74 re75
matrix colnames balance3 = mean_T mean_C StdDev_T StdDev_C Diff StdE_Diff t-stat p-value


matrix list balance3

matrix top = balance3[1..4,1..8]
matrix list top
matrix bot = balance3[5..6,1..8]
matrix list bot


putexcel set TABLE_1_ok.xls, modify
putexcel T1=matrix(top), hcenter colnames
putexcel T7=matrix(bot), hcenter

quietly sum age if train == 1, d
scalar N_t = r(N)
putexcel T9 = N_t

quietly sum age if train == 0, d
scalar N_c = r(N)
putexcel U9 = N_c


*/


***********    e    ***********


qui reg re78 treated, vce(robust)
estimates store Regression_4

qui reg re78 treated age educ black hisp, vce(robust)
estimates store Regression_5

qui reg re78 treated age educ black hisp re74 re75, vce(robust)
estimates store Regression_6

qui sum train if treated == 0
scalar N_C = r(N)
qui sum train if treated == 1
scalar N_T = r(N)

outreg2 [Regression_4 Regression_5 Regression_6] using TABLE_2.xls, nocons adds(# of units in Treatment, N_T, # of units in Control, N_C) excel append


***********    f    ***********


qui reg re78 train, vce(robust)
estimates store Regression_7

qui reg re78 train age educ black hisp, vce(robust)
estimates store Regression_8

qui reg re78 train age educ black hisp re74 re75, vce(robust)
estimates store Regression_9

qui sum train if train == 0
scalar N_C = r(N)
qui sum train if train == 1
scalar N_T = r(N)

outreg2 [Regression_7 Regression_8 Regression_9] using TABLE_2.xls, nocons adds(# of units in Treatment, N_T, # of units in Control, N_C) excel append
	
}


************************************************	QUESTION 3 		************************************************

quietly{


***********    a    ***********


***********    b    ***********


***********    c    ***********


use jtrain2.dta, clear

reg re78 train, robust
scalar myb = _b[train]
scalar list myb

generate index = .
generate random = .
generate treatment = .
scalar conta = 0

forvalues i=1(1)100000 {
	qui replace random = runiform()
		sort random
	qui replace index = _n
	qui replace treatment = 1 if index <= 185
	qui replace treatment = 0 if index > 185
	qui sum re78 if treatment == 0
		scalar mean_c = r(mean)
	qui sum re78 if treatment == 1
		scalar mean_t = r(mean)
		scalar Tave = mean_t - mean_c
	if abs(Tave) >= myb {
		scalar conta = conta + 1
	}

}

qui scalar p_value = conta/100000
scalar list p_value
	
	
}
