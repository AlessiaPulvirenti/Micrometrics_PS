**************
* EXERCISE 2 *
**************

quietly{

clear all
cd ""
use pset_2_q_2_and_3.dta
qui des

*(a)*
	qui sum Healthy
	scalar mu_y=r(mean)
	display mu_y
	qui sum Education 
	scalar mu_x=r(mean)
	display mu_x

*(b)*
	generate Quarter1 = birthqtr == 1
	generate Quarter2 = birthqtr == 2
	generate Quarter3 = birthqtr == 3
	generate Quarter4 = birthqtr == 4
	
	ssc install dummies 
	dummies region
	qui summ region*
	forvalues i=1/9 {
		rename region`i' region_cat_`i'
	}
	local Controls "Central Married region_cat_1-region_cat_9"
	qui sum `Controls'
	
	forvalues i=0/9 {
		generate birthyear_`i' = birthyear == (1930+`i')
	}
	local Birth_Year_FEs "birthyear_0-birthyear_9"
	qui sum `Birth_Year_FEs'
	
*(c)*
	*I used vce(robust) everywhere not sure if it's right
	
	qui reg Healthy Education, vce(robust)
	estimates store Regression_1
	qui reg Healthy Education `Controls', vce(robust)
	estimates store Regression_2
	qui reg Healthy Education `Controls' `Birth_Year_FEs', vce(robust)
	estimates store Regression_3
	
*(d)*
	qui reg Healthy Education, vce(robust)
	outreg2 using TABLE_Q_2.xls, excel replace nocons keep(Education) addtext(Controls, NO, Birth Year FEs, NO)  addstat("Mean y", mu_y, "Mean x", mu_x) ctitle("")
	qui reg Healthy Education `Controls', vce(robust)
	outreg2 using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Birth Year FEs, NO)  addstat("Mean y", mu_y, "Mean x", mu_x) ctitle("")
	qui reg Healthy Education `Controls' `Birth_Year_FEs', vce(robust)
	outreg2 using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Birth Year FEs, YES)  addstat("Mean y", mu_y, "Mean x", mu_x) ctitle("")
	
*(e)*
	*ssc install ivreg2
	*qui ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3), robust //first
	*qui ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls', robust //first
	*qui ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust //first

*(f)*
	qui ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3), robust //first
	outreg2 using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, NO, Birth Year FEs, NO)    addstat("F-statistic IVs", e(F)) ctitle("")
	qui ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls', robust //first
	outreg2 using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Birth Year FEs, NO)    addstat("F-statistic IVs", e(F)) ctitle("")
	qui ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust //first
	outreg2 using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Birth Year FEs, YES)    addstat("F-statistic IVs", e(F)) ctitle("")
	
}	