** Microeconometrics 20295, Problem Set 1

** Elena Neri

********************************************************************************

cd "C:\Users\elena\OneDrive\Desktop\ESS\2nd year\Microeconometrics\Micrometrics_PS\Elena"
use jtrain2.dta, clear


** Question 1
* (a) 

ssc install balancetable, replace

local control_vars "age educ black hisp nodegree re75 re74" 
balancetable train `control_vars' using TABLE_1.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames replace	


/*global controls "age educ black hisp nodegree re74 re75"
matrix balcheck=(.,.,.,.,.,.)
local i=1

foreach var in $controls {
	
	qui sum `var' if train==0, d
	matrix balcheck[`i',1]=r(N)
	matrix balcheck[`i',2]=r(mean)
	matrix balcheck[`i',3]=r(sd) 
	
	qui sum `var' if train==1, d
	matrix balcheck[`i',4]=r(N)
	matrix balcheck[`i',5]=r(mean)
	matrix balcheck[`i',6]=r(sd) 
	
	local i=`i'+1 
	
	if `i'<=7 matrix balcheck=(balcheck \ .,.,.,.,.,.) 
	
}
*
matrix rownames balcheck=age educ black hisp nodegree re74 re75
matrix colnames balcheck=N Mean StDev N Mean StDev
*
matrix list balcheck
*
*
putexcel set "balance_table.xlsx", sheet("Balance_Table") replace
putexcel A2="Age" A3="Education" A4="Black" A5="Hispanic" A6="Drop-outs" A7="Real earnings 1974" A8="Real earnings 1975"
putexcel B1="N" C1="Mean" D1="StdDev" E1="Median" F1="N" G1="Mean" H1="StdDev" I1="Median"
putexcel B2=matrix(balcheck)
*/ 


* (b) 

regress re78 train
return list 
matrix A  =  r(table)   
matrix list A 
scalar b = A[1,1] 
scalar list b
scalar stde = A[2,1]
scalar list stde


* (c)
reg re78 train
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel replace

reg re78 train age educ black hisp
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append

*
reg re78 train age educ black hisp re74 re75
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append
*

* (d)

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




** Question 2
* (a) 

cd "C:\Users\elena\OneDrive\Desktop\ESS\2nd year\Microeconometrics\Micrometrics_PS\Elena"
use jtrain3.dta, clear

local control_vars "age educ black hisp re75 re74" 
balancetable train `control_vars' using TABLE_1.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(A19)	 	


* (b) 

gen treated=. 
set seed 88888

gen random=uniform()
sort random

egen random_order=rank(random)

qui sum random
gen N =r(N)
replace treated=0 if random_order<=(N/2)
replace treated=1 if random_order>(N/2) & random_order<=N


*  (c)
ssc install randtreat
help randtreat

set seed 88888
randtreat, gen(treated_2)

pwcorr treated_2 treated, sig

*the Pearson correlation coefficient:  		0.0052
*the level of statistical significance:		0.7867

* (d)

local control_vars "age educ black hisp re75 re74" 
balancetable treated_2 `control_vars' using TABLE_1.xls, ctitle("Mean C" "Mean T" "Diff. t-test") vce(robust)  leftctitle("Variable") varnames modify cell(F19)


* (e)
global x_1 "treated"
global x_2 "age educ black hisp" 
global x_3 "re74 re75" 
global x_4 "train"

reg re78 $x_1
tabstat $x_1 if e(sample)==1, statistics(count) by($x_1) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append 
*
reg re78 $x_1 $x_2
tabstat $x_1 if e(sample)==1, statistics(count) by($x_1) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append
*
reg re78 $x_1 $x_2 $x_3 
tabstat $x_1 if e(sample)==1, statistics(count) by($x_1) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append


* (f)
reg re78 $x_4
tabstat $x_1 if e(sample)==1, statistics(count) by($x_1) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append 
*
reg re78 $x_4 $x_2
tabstat $x_1 if e(sample)==1, statistics(count) by($x_1) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append
*
reg re78 $x_4 $x_2 $x_3 
tabstat $x_1 if e(sample)==1, statistics(count) by($x_1) save
return list
scalar N_control = r(Stat1)[1,1]
scalar N_treated = r(Stat2)[1,1]
outreg2 using Table_2.xls, adds(N_control, N_control, N_treated, N_treated) excel append





** Question 3












