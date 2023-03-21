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



************************************************	EXERCISE 1 		************************************************

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

local Controls "region1 region2 region3 region4 region5 region6 region7 region8 region9 Central Married"
local Birth_Year_FEs "birthyear_FE*"

reg Healthy Education `Controls' `Birth_Year_FEs'  
outreg2 using TABLE_Q_3.xls, excel keep (Education) nocons addtext(Controls, YES, Year of Birth FEs, YES) addtext(Regression, OLS) title("OLS and IV") replace


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
outreg2 using TABLE_Q_3.xls, excel keep (Quarter1 Quarter2 Quarter3) addtext(Controls, YES, Year of Birth FEs, YES) addtext(Regression, First Stage) title("OLS and IV") addstat("F-statistic IVs", Fstat) nocons append

/*

(c) Estimate a reduced form regression of Healthy on instruments Quarter1 Quarter2
Quarter3 and Controls and Birth Year FEs as controls.
Append such a reduced form regression to TABLE_Q_3.
Based on the results of question 2.(e), what are the expected signs of the coefficients
of Quarter1 Quarter2 Quarter3?
Are these reduced form coefficients in line with your expectations?
*/

reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs', robust 
outreg2 using TABLE_Q_3.xls, excel keep (Quarter1 Quarter2 Quarter3) addtext(Regression, Reduced Form) nocons append

/*
Write comments here based on Q2 results

*/

/*
Estimate a second stage for an IV regression of Healthy on Education using Quarter1
Quarter2 Quarter3 as instruments for Education, and Controls and Birth Year FEs
as controls.
Append such a second stage regression to TABLE_Q_3.
Only show the coefficient of Education and the usual regression statistics.
*/

ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust
outreg2 using TABLE_Q_3.xls, excel keep (Education) nocons addtext(Controls, YES, Year of Birth FEs, YES) addtext("Second Stage") title("OLS and IV") append


