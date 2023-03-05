											***********************************
											***								***
											***		  Problem Set 1			***
											***								***
											***			GROUP 8				***	
											***								***
											***		Aleksa Mitrovic			***
											***		Elena Neri				***
											***		Alessia Pulvirenti		***
											***		Tommaso Roccuzzo		***
											***								***
											***********************************
											
											
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

putexcel set TABLE_1.xls, replace

putexcel A1=matrix(balance), hcenter names
putexcel A9 = "Observations"
quietly sum age if train == 1, d
scalar N_t = r(N)
putexcel B9 = N_t

quietly sum age if train == 0, d
scalar N_c = r(N)
putexcel C9 = N_c
*/


* Comment *
* We expect and find some imbalances, as we are working with subsets of the the original treatment and control groups of the experimental data used by LaLonde. Two variables are unbalanced: "nodegree", with the difference between treatment and control significant at confidence levels of 1%,5% and 10%, and "hisp", with the difference significant only at the 10% confidence level. The sizable imbalance in "nodegree" is most likely caused by the nature of the treatment, which specifically targeted, among the 4 different groups, young school drpouts. All other variables are balanced. *



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


* Comment *
* The coefficient captures the difference in observed mean values between the treatment and the control group for the dependent variable "re78". It means that receiving the treatment increased "re78" by circa 1.79, i.e., real annual earnings in the treated group increased on average by almost $1800. *

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


* Comment *
* The coefficient on "train" is positive and significant at 1% in the first regression. When we introduce the first covariates "age" "educ" "black" "hisp" it decreases slightly and becomes significant at 5%. This is not affected by the introudction of "re74" and "re75". 
* The coefficient for "black" is negative and the one for "educ" is positive, and both are significant at 5% in the second regression. In the third regression their size is not affected by the introduction of the two new covariates, and the one for "educ" becomes significant at 1%. 
* Thus, results are thus quite robust to the introduction of covariates. 
*
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

* Alternative construction of balance table using the matrix command	

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


putexcel set TABLE_1.xls, modify
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

* @Tommi: here we added the setseed option with the same seed as in point b.

pwcorr treated treated_2, sig star(.05)


***********    d    ***********

local control_vars age educ black hisp re75 re74 
balancetable treated `control_vars' using TABLE_1_Ale.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(H11) wide		


* Alternative construction of balance table using the matrix command

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


putexcel set TABLE_1.xls, modify
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
/*Under which conditions, allowing for heterogeneous treatment effects, is Neyman's inference unbiased?
*/

/*Neyman's inference is based on the estimation of the average treatment effect for the sample at hand, and to construct confidence intervals for the average treatment effect. 
Assuming a FINITE SAMPLE, with no particular assumption on the construction of the sample itself, an unbiased estimator found by Neyman consists simply in the difference in average outcomes by treatment status. Assuming potential outcomes are fixed, so that the only stochastic component is the treatment status, D_i, Neyman constructs an estimator /tau_hat which is unbiased for /tau, the average treatment effect. 

However, Imbens and Rubin 2015, show that the sampling variance of the estimator of the average treatment effect derived by Neyman, over the randomisation distribution is composed of three elements: 
1. the variance of the potential outcomes Y_i(0) over the no. of control
2. the variance of the potential outcomes Y_i(0) over the no. of treated
3. the population variance of the unit level treatment effect, i.e., the pop variance of Y_i(1) - Y_i(0), over the whole sample. We know that this term is unobservable, because we never observe Y_i(1) and Y_i(0) for the same unit. 

So the variance of Neyman's estimator /tau_hat is usually upward biased, leading to overly conservative confidence intervals for /tau_hat. If we allow for heterogeneous treatment effect, then this bias disappears if we assume the sample as a random sample from an infinite population. 
//(maybe add: In this case, unbiasedness results from assuming the sample is a random sample from an infinite population, implying the estimator of the variance of Neyman's estimator is an estimator of the population rather than sample average treatment effect)

(maybe mention something about CLT ? and large sample approximation? - Athey and Imbens mention that large-sample approximation is not needed)

*/


***********    b    ***********
/*Describe Fisher's inference and replicate section 4.1 of Athey and Imbens (2017) in Stata. Do you arrive at their same p-value? If not, why? Hint: Note that you can draw motivation from third-parties for your own answer; for this case, we suggest that you read Hess (2017).*/

se jtrain2.dta, clear

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


* Alternative way, using the permute function in Hess (2017). 
permute train _b[train], reps(10000) seed(0): /// 
	regress re78 train, vce(robust)
// Looking at the two sided p-value (because the alternative hypothesis is just that the treatment effect is different from 0, we reach the same p-value as with the loop procedure, i.e., 0.0044, which, net of rounding is very similar to the conclusion reached by Athey and Imbens)


/*
Fisherian inference of completely randomised experiments consists in assessing the sharp null hypothesis of no effect of the treatment versus the control group (Imbens and Rubin, 2015). In particular, Fisher's null hypothesis can be written as: 

H0: Y_i(0) = Y_(1) for each unit of the experiment

Under Fisher's null hypothesis, and under sharp null hypotheses more generally, for units with either potential outcome observed, the other potential outcome is known; and so, under such a sharp null hypothesis, both potential outcomes are "known" for each unit in the sample being either directly observed or inferred through the sharp null hypothesis (Imbens and Rubin, 2015). 

In practice what we would like to test through Fisher's inference is the hypothesis that the treatment effect is 0 for all units, and to do this we will reassign the treatment 10000 thousands times, keeping the number of treated and control equal to the original experiment. We will store the value of the average treatment effect for each of the iterations and count how many times over the iterations the coefficient obtained (i.e., the simple difference in average post treatment earnings between treated and control group) is equal or above 1.79. The number of iterations in which the coefficient is higher than 1.79 over the total number of iterations will tell us the p-value of the test:
H0: Y_i(0) = Y_(1) for each unit of the experiment
H1: Y_i(0) =! Y_(1) for each unit of the experiment
As in every other experiment, low values of the p-value indicate that we can reject the null hypothesis, which in this case is of non-existence of a treatment effect. 

In our case, we obtain a p-value of 0.0044, similar to the result of Athey and Imbens. The slight difference could be due to...



*/

***********    c    ***********

/* (c) Read again the randomization plan in LaLonde (1986). On which grounds Athey and Imbens (2017)'s illustration of Fisherian inference on LaLonde (1986)'s paper could be criticized?
*/

/*
Looking at the randomisation plan in LaLonde (1986) we can see that the randomisation was stratified at the city level. Stratifiying at some level the randomisation, allows to

Not stratifying by city implies that if more individuals from higher average earning cities are re-assigned to the treatment with respect to the initial assignment, the estimated average differences will be higher, and therefore also the probability of rejecting the null hypothesis (Type I error).

*/
