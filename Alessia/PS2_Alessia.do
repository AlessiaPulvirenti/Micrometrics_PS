					*******************************************
					***										***
					***		  Problem Set 2					***
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

ssc install ivreg2, replace


**# EXERCISE 1	

/*
Question 1 and Question 2 here
*/


* QUESTION 3


cd "C:\Users\pulvi\OneDrive - Università Commerciale Luigi Bocconi\Depr(ESS)ion\2. Second Year\Micrometrics\PS\Micrometrics_PS\Alessia"
/* (a) Estimate an OLS regression of Healthy on Education using Controls and Birth Year
FEs as controls, and export it to an excel table named TABLE Q_3. 
Only show the coefficient of Education and the usual regression statistics.
*/
use pset_2_q_2_and_3.dta, clear

tab region, gen(region)
tab birthyear, gen(birthyear_FE)

local Controls "region* Central Married"
local Birth_Year_FEs "birthyear_FE*"

reg Healthy Education `Controls' `Birth_Year_FEs'  
outreg2 using TABLE_Q_3.xls, excel keep (Education) nocons addtext(Controls, YES, Year of Birth FEs, YES, Regression, OLS) title("OLS and IV") replace


/*(b) Estimate a first stage for an IV regression of Healthy on Education using Quarter1
Quarter2 Quarter3 as instruments for Education, and Controls and Birth Year FEs
as controls, and append such a first stage regression to TABLE Q 3.
Only show the coefficient of Quarter1 Quarter2 Quarter3 and the usual regression
statistics.
Add a line with the F-statistic of the excluded instruments, naming it F-statistic IVs.*/ 

tab birthqtr, gen(Quarter)

ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust first savefirst
scalar Fstat = e(widstat)
est restore _ivreg2_Education
outreg2 using TABLE_Q_3.xls, excel keep (Quarter1 Quarter2 Quarter3) addtext(Controls, YES, Year of Birth FEs, YES, Regression, First Stage) title("OLS and IV") addstat("F-statistic IVs", Fstat) nocons append

/*

(c) Estimate a reduced form regression of Healthy on instruments Quarter1 Quarter2
Quarter3 and Controls and Birth Year FEs as controls.
Append such a reduced form regression to TABLE_Q_3.
Based on the results of question 2.(e), what are the expected signs of the coefficients
of Quarter1 Quarter2 Quarter3?
Are these reduced form coefficients in line with your expectations?
*/

reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs', robust 
outreg2 using TABLE_Q_3.xls, excel keep (Quarter1 Quarter2 Quarter3) addtext(Controls, YES, Year of Birth FEs, YES, Regression, Reduced Form) nocons append

/*
Write comments here based on Q2 results

*/

/* (d)
Estimate a second stage for an IV regression of Healthy on Education using Quarter1
Quarter2 Quarter3 as instruments for Education, and Controls and Birth Year FEs
as controls.
Append such a second stage regression to TABLE_Q_3.
Only show the coefficient of Education and the usual regression statistics.
*/

ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust
outreg2 using TABLE_Q_3.xls, excel keep (Education) nocons addtext(Controls, YES, Year of Birth FEs, YES, Regression, Second Stage) title("OLS and IV") append


/*(e)
Bound et al. (1995) discuss a number of issues that arise when confronted with both weak instruments and finite sample bias, in particular, when there exists a weak correlation between the instrument and the outcome variable. Discuss how these issues can generate a bias in the IV regression you have estimated in item (d).
Can you reject the the null hypothesis of the test of joint significance of the instruments?
Based on the size the F-statistic, can you say if finite sample bias is likely or not to be an issue in this case?
*/

reg Education Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs'
scalar R_sqrd = e(r2)
display R_sqrd

ssc install pcorr2, replace
pcorr2 Education Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs' //The squared partial correlation of the ...


*ADD A FEW LINES ABOUT THE NULL HYPOTHESIS OF JOINT SIGNIFICANCE 

/*
Y = /beta X + /epsilon		(1) - original model to be estimated - X is endogenous
X = /pie Z + /nu 			(2)	- First stage

Bound et al. (1995) analyse two cases under which the IV approach can lead to unconsistent estimates, with inconsistency that could be larger than that of the OLS estimator using and endogenous variable. 
One case is that of a weak instrument: when the instrument Z, used to get around the problem of endogeneity of the treatment variable X, is weakly correlated with the endogenous variable in question, this is likely to generate a large inconsistency in the IV estimates. In the case of an IV approach with no covariates, the authors prove this by showing that the ratio of the inconsistency of the IV estimator to the inconsistency of the OLS estimator is an expression that has at the denominator the correlation between X and Z, and at the numerator, the correlation between Z and the error term, and X and the error term. Therefore, if the instrument is only slightly endogenous (i.e., if the instrument Z has a very low correlation with the unboservable characteristics that affect Y in eq. (1)), this bias can be greatly amplified by low correlation between Z and X. In an IV setting where the first stage (and thus the second stage) include some covariates as in this case, Bound et al. prove that the ratio of the inconsistency of IV to inconsistency of OLS id negatively associated to the partial R-squared of the first stage, that is, the R-squared from the regression of x on z once the common exogenous variable have been partialled out from both X and Z.

In our case, since the partialled R-Squared for all three instruments is very low, if the quarter of birth proves to be even slightly endogenous to the unobserved characteristics that might affect health status, then the coefficient computed in the second stage could be biased. For example, if the quarter of birth is endogenous to the socio-economic status of individuals in the sample, then the IV coefficient could be upward biased. This could be a plausible explanation since our IV estimate is higher than the OLS estimate. Angrist and Krueger (1991, 1992 - a subsequent paper) provide many evidence to show that compulsory attendance laws are working to induce a correlation in educational attainment. However, if compulsory schooling law are not the ONLY channel through which quarter of birth and educational attainment are correlated (exclusion restriction), and quarter of birth is correlated in some way with health status, then our IV estimate can be strongly inconsistent, even more than than the OLS. Bound et al. 1995 provide some evidence of the correlation between birth seasonality and physical and mental health of individuals, listing some papers that prove that individuals born early in the years are more likely to be affected by schizofrenia, mental retardation, autism etc. 

The other problem discussed by Bound and coauthors when dealing with IVs is related to finite-sample bias. Assuming that the instrument Z is completely exogenous, the IV is a consistent estimator of /beta, but in finite samples, it is biased in the same direction of the OLS estimator. The magnitude of the bias depends negatively on the sample size, and negatively on the multiple correlation between the instruments and the endogenous explanatory variables.

Bound et al. (1995) suggest that to assess finite-sample bias, it is useful to examine the F-statistic from the First Stage. They claim that the bias of the instrumental variable (IV) relative to ordinary least squares (OLS) is inversely related to the F-statistic of the excluded instruments from the first stage. In our analysis, we obtained an F-statistic of 62. According to Bound et al. (1995), a close-to-1 F-statistic raises concerns about finite-sample bias. However, since our F-statistic is large, we can confidently state that finite-sample bias is not an issue in this case. Additionally, Staiger and Stock (1994) suggest that the bias of OLS relative to IV could be approximated by 1/F, where F is the F-statistic. Therefore, if the F-statistic is large, as in our case, the bias is likely to be very small. 
*/

/*
(f) Create a local named State FEs with dummies for each state of birth, except Wyoming,
which is intended to be the reference category.
*/

tab bpl, gen(State)
local State_FEs "State1-State50"

tab birthdate, gen(Year_quarter)
local Year_Quarter_FEs "Year_quarter1-Year_quarter39"

egen State_quarter = concat(bpl birthqtr), decode punct(-)
tab State_quarter, gen(State_quarter)
local State_Quarter_FEs "State_quarter1-State_quarter203"


/*(g)
Estimate an IV regression of Healthy on Education using Year Quarter FEs as instruments
for Education, Controls and Birth Year FEs as controls.
Estimate an IV regression of Healthy on Education using State Quarter FEs as instruments
for Education, and Controls, Birth Year FEs, and State FEs as controls.
Use Wyoming as omitted category for state of birth and include Washington DC.
*/
local Year_Quarter_FEs "Year_quarter1-Year_quarter39"
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
ivreg2 Healthy (Education = `Year_Quarter_FEs') ///
`Controls' `Birth_Year_FEs', robust partial(`Birth_Year_FEs')

local State_Quarter_FEs "State_quarter1-State_quarter203"
local Controls "Central Married Region*"
local Birth_Year_FEs "Birth*"
local State_FEs "State1-State50"
ivreg2 Healthy (Education = `State_Quarter_FEs') ///
`Controls' `Birth_Year_FEs' `State_FEs', robust partial(`Birth_Year_FEs' `State_FEs')

/*(h)
Compute the F-statistic for the excluded instruments in point (g).
Can you say if both regressions are likely to suffer from finite sample bias?*/






