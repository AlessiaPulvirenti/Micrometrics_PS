					*******************************************
					***										***
					***		  Problem Set 1					***
					***										***
					***			GROUP 8						***	
					***										***
					***		Aleksa Mitrovic		3100079		***
					***		Elena Neri			3070190		***
					***		Alessia Pulvirenti	3060894		***
					***		Tommaso Roccuzzo	3080613		***
					***										***
					*******************************************
											

clear all


************************************************	QUESTION 1 		************************************************

quietly{

use jtrain2.dta

***********    a    ***********

ssc install balancetable, replace

help balancetable

local control_vars age educ black hisp nodegree re75 re74 
balancetable train `control_vars' using TABLE_1.xls, ctitle("Mean C" "Sd C" "Mean T" " Sd T" "Diff. in means" "Sd Error") vce(robust)  leftctitle("Variable") varnames wide replace		


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
/* We expect and find some imbalances, as we are working with subsets of the the original treatment and control groups of the experimental data used by LaLonde. Two variables are unbalanced: "nodegree", with the difference between treatment and control significant at 1%, and "hisp", with the difference significant only at the 10% (this can be seen by plotting the balance table with the matrix function). The sizable imbalance in "nodegree" is most likely caused by the nature of the treatment, which specifically targeted, among the 4 different groups, young school dropouts. All other variables are balanced.
*/


*Reason why we will use vce(robust) throughout the whole do file* 
/* 

qui reg re78 train
estat hettest

Breusch–Pagan/Cook–Weisberg test for heteroskedasticity 
Assumption: Normal error terms
Variable: Fitted values of re78

H0: Constant variance

    chi2(1) =  29.07
Prob > chi2 = 0.0000

Performing the Breusch–Pagan we find evidence of heteroskedasticity in the data (we reject the null hypothesis of homoskedasticity at a 1% level of significance). 
To correct for heteroskedasticity and to obtain robust standards errors, we include the "vce(robust)" option in all regressions and when constructing balance tables (with the exception of the regressions in point 1 (d), which cannot include the "vce(robust)" option to correctly adopt the postestimation command "dfbeta" computing the influence statistic ("DFBETA")).
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


* Comment *
* The coefficient captures the difference in observed mean values between the treatment and the control group for the dependent variable "re78". It means that receiving the treatment increased "re78" (real earnings in 1978) by circa 1.794 (in thousand of 1982 USD), i.e., real annual earnings in the treated group increased on average by around $1794 of 1982 USD, realtive to not receiving the treatment. *

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
/*The coefficient on "train" is positive and significant at 1% in the first regression. When we introduce the first covariates "age" "educ" "black" "hisp" it decreases slightly and becomes significant at 5%. This is not affected by the introudction of "re74" and "re75". 
The coefficient for "black" is negative and the one for "educ" is positive, and both are significant at 5% in the second regression. In the third regression their size is not affected by the introduction of the two new covariates, and the one for "educ" becomes significant at 1%. Moreover, when introducing the variables measuring earning in 1974 and 1975 measured in 1982 USD, we can see that these variables are not significant and that their introduction basically leaves the coefficient of "train" unchanged. 
Thus, results are thus quite robust to the introduction of covariates. 
*/

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



*Comment*
/*
Our results are sensitive to influencial observations, especially concerning the variable "train". First of all, looking at the significance of the coefficient, we notice that as we remove the 3, 5, 10 lowest and largest values in the dfbeta of "train", the p-value starts to increase to 0.009, 0.015 and  0.029, losing the significance at 1% of the coefficient in the regressions where we remove the 5 and 10 obs with the lowest and largest value of the dfbeta of "train". 

Focusing on the magnitude of the coefficient of "train" (i.e., the treatment effect), we can see that it drops from 1.68 to 1.36 when removing the 3 lowest and largest values of influence_train, representing a substantial fall in the magnitude of the treatment variable. Furthermore, the coefficient drops all the way down to 1.02 in the third regression, when removing the 10 lowest and largest values of "influence_train". This proves that our results are sensitive to influencial observations. 
*/ 

/*
local x_1 "train age educ black hisp re74 re75"
reg re78 `x_1', vce(robust)

*Comment*
If we re-run the regression with the option "vce(robust)" we find, as expected, that the magnitude of the estimates is unchanged, we only find slightly higher standard errors (i.e., for the treatment variable train, .6565083 > .6308616).
*/
}

************************************************	QUESTION 2 		************************************************

quietly{
	
use jtrain3.dta, clear


***********    a    ***********

local control_vars age educ black hisp re75 re74 
balancetable train `control_vars' using TABLE_1.xls, ctitle("Mean C" "Sd C" "Mean T" " Sd T" "Diff. in means" "Sd Error") vce(robust) leftctitle("Variable") varnames modify cell(A13) wide

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

pwcorr treated treated_2, sig star(.05)

/*
             |  treated treate~2
-------------+------------------
     treated |   1.0000 
             |
             |
   treated_2 |   0.0052   1.0000 
             |   0.7867
             |
			 
The correlation is not statistically different from 0 (the low correlation found is significant at around 21%) given that both assignments occurred completely at random.
*/


***********    d    ***********

local control_vars age educ black hisp re75 re74 
balancetable treated `control_vars' using TABLE_1.xls, ctitle("Mean C" "Sd C" "Mean T" " Sd T" "Diff. in means" "Sd Error") vce(robust) leftctitle("Variable") varnames modify cell(J13) wide		


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

*Comment*
/*
As exepcted, the re-assignment of the treatment in two equally-sized groups produces a balance table with perfectly balanced treatment and control groups. This was not the case in point 2 (a), where the size of the two groups was substantially different. 
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

/*
The variable treated is the result of a random re-assignment of the treatment status to the sample. As a consequence, we find that it is never significant in explaining changes in earnings in 1978. Moreover, we notice that, as we exclude the true treatment variable (train), other covariates which were not significant before (e.g., age and earnings in the previous years), become significant. 
*/


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
	

/*
The last three regressions show very different results. In particular, the coefficient of the treatment variable, "train", shows negative values in the first two regressions, and a positive - but not significant - in the last one. This is mostly due to the nature of the dataset. In fact, jtrain.3, as opposed to jtrain.2, is composed of 185 observations coming from treated individuals in the experiment but also of 2490 observations coming from observational data which are used as control group. As a consequence, the negative coefficient for "train" might be due to the fact that the observations used as control group in this case do not represent a "good" control group, as treatment assignment is not as-good-as random. In other words, the two (unbalanced) groups do not only differ for the treatment status, but also by individual characteristics that affect the change in earnings in 1978.

This can be explained also by looking at the fact that, when adding the controls, the sign of "train" reverses becoming positive, although it loses its significance. Controlling for individual characteristics thus allows us to compare two groups which are more similar (and that clearly do not differ only for the treatment assignment).
*/

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
3. the population variance of the unit level treatment effect, i.e., the pop variance of Y_i(1) - Y_i(0), over the whole sample. We know that this term is unobservable, because we never observe Y_i(1) and Y_i(0) for the same unit. This term has a minus in front of it. 

Not taking into account the third element (as it is unobservable), implies that the variance of Neyman's estimator /tau_hat is usually upward biased, leading to overly conservative confidence intervals for /tau_hat. If we allow for heterogeneous treatment effect, then this bias disappears if we assume the sample as a random sample from an infinite population, implying the estimator of the variance of Neyman's estimator is an estimator of the population rather than sample average treatment effect. 
*/


***********    b    ***********
/*Describe Fisher's inference and replicate section 4.1 of Athey and Imbens (2017) in Stata. Do you arrive at their same p-value? If not, why? Hint: Note that you can draw motivation from third-parties for your own answer; for this case, we suggest that you read Hess (2017).*/

use jtrain2.dta, clear

reg re78 train, robust
scalar myb = _b[train]
scalar list myb

generate index = .
generate random = .
generate treatment = .
scalar conta = 0

set seed 12345

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


/*
Fisherian inference of completely randomised experiments consists in assessing the sharp null hypothesis of no effect of the treatment versus the control group (Imbens and Rubin, 2015). In particular, Fisher's null hypothesis can be written as: 

H0: Y_i(0) = Y_(1) for each unit of the experiment

Under Fisher's null hypothesis, and under sharp null hypotheses more generally, for units with either potential outcome observed, the other potential outcome is known; and so, under such a sharp null hypothesis, both potential outcomes are "known" for each unit in the sample being either directly observed or inferred through the sharp null hypothesis (Imbens and Rubin, 2015). 

In practice what we would like to test through Fisher's inference is the hypothesis that the treatment effect is 0 for all units, and to do this we will reassign the treatment 10000 thousands times, keeping the number of treated and control equal to the original experiment. We will store the value of the average treatment effect for each of the iterations and count how many times over the iterations the coefficient obtained (i.e., the simple difference in average post treatment earnings between treated and control group) is equal or above 1.79. The number of iterations in which the coefficient is higher than 1.79 over the total number of iterations will tell us the p-value of the test:
H0: Y_i(0) = Y_(1) for each unit of the experiment
H1: Y_i(0) =! Y_(1) for each unit of the experiment
As in every other experiment, low values of the p-value indicate that we can reject the null hypothesis, which in this case is of non-existence of a treatment effect. 


In our case, we have produced a loop in which 100,000 iterations produce a p-value very similar to that found by Athey and Imbens (~ 0.0041). This means that in our case, we have that in 100,000 iterations, the difference between the sample averages is greater or equal than 1.79 in absolute value only about 410 times. The slight difference might be due to the fact that Athey and Imbens are considering a sample of 185 treated and 240 control, while we have 260 units in the control group. Moreover our procedure is stochastic in finite (small) sample sizes, so it is reasonable to expect minor variarions in the estimated values. 

*/

***********    c    ***********

/* (c) Read again the randomization plan in LaLonde (1986). On which grounds Athey and Imbens (2017)'s illustration of Fisherian inference on LaLonde (1986)'s paper could be criticized?
*/

/*
Looking at the randomisation plan in LaLonde (1986) we can see that the randomisation was stratified at the city level. Stratifiying allows us to compare individuals in different economic environments (cities). Not considering stratification in the process of reassignment of the treatment, implies that if, for example, more individuals from higher average earning cities are re-assigned to the treatment with respect to the initial assignment, the estimated average differences will be higher, and therefore also the probability of rejecting the null hypothesis (Type I error). On the other hand, overrepresentation of individuals from lower-earnings cities in the treatment group with respect to the original assignment, might lead to an underestimation of the p-value (probability of Type I error). 

In the Fisherian's inference proposed by Athey and Imbens (2017), the treatment is re-assigned randomly without considering the stratification included in the design of the experiment. 

In order to perform a correct re-randomisation, we would need to know the strata used in the first design of the experiment, and the command ritest proposed by Hess (2017) and its option strata() will allow to take into account the different strata, which then would need to be included also in the regression as strata fixed effects. 

*/
}