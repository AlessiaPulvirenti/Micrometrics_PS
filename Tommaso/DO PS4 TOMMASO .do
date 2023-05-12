					*******************************************
					***										***
					***		  Problem Set 4					***
					***										***
					***			GROUP 8						***	
					***										***
					***		Aleksa Mitrovic		3100079		***
					***		Elena Neri			3070190		***
					***		Alessia Pulvirenti	3060894		***
					***		Tommaso Roccuzzo	3080613		***
					***										***
					*******************************************
											
cd "/Users/tommasoroccuzzo/Library/Mobile Documents/com~apple~CloudDocs/Tommaso/Appunti/Bocconi/2021-2022/2° Semestre/Microeconometrics/Problem Sets/PS 4"

*** QUESTION 1 ***

use pset_4.dta, clear


*(a)

gen TREATED = .

replace TREATED = 1 if num_PSINPRES1980 > 0
replace TREATED = 0 if num_PSINPRES1980 == 0

gen POST_TREATED = TREATED*POST


*(b)

gen T1 = HEALTH_CENTER_VL if ele1v_post92 < 1994 & TREATED == 1
gen C1 = HEALTH_CENTER_VL if ele1v_post92 < 1994 & TREATED == 0

gen T2 = HEALTH_CENTER_VL if ele1v_post92 < 1997 & ele1v_post92 > 1993 & TREATED == 1
gen C2 = HEALTH_CENTER_VL if ele1v_post92 < 1997 & ele1v_post92 > 1993 & TREATED == 0

gen T3 = HEALTH_CENTER_VL if ele1v_post92 > 1996 & TREATED == 1
gen C3 = HEALTH_CENTER_VL if ele1v_post92 > 1996 & TREATED == 0

preserve

collapse T1 T2 T3 C1 C2 C3, by(year)

twoway (line T1 year) (line C1 year), title("Election years: 1992-1993", size(4)) ytitle("Health Centers", size(3)) xtitle("Years", size(3)) legend(order(1 "Treatment" 2 "Control") rows(2)) xline(1993) saving(graph1,replace)

twoway (line T2 year) (line C2 year), title("Election years: 1994-1997", size(4)) ytitle("Health Centers", size(3)) xtitle("Years", size(3)) legend(order(1 "Treatment" 2 "Control") rows(2)) xline(1997) saving(graph2, replace)

twoway (line T3 year) (line C3 year), title("Election years: 1998-2000", size(4)) ytitle("Health Centers", size(3)) xtitle("Years", size(3)) legend(order(1 "Treatment" 2 "Control") rows(2)) xline(2000) saving(graph3, replace)

graph combine graph1.gph graph2.gph graph3.gph, saving(Graph_1, replace) cols(3)

restore

*** From what we observe in the graph, it appears that pre-treatment trends are indeed parallel in all three sets of villages. ***


*(c)

matrix define A = J(3,3,.)
matrix rownames A = "POST=1" "POST=0" "Difference 1"
matrix colnames A = "TREATED=1" "TREATED=0" "Difference 2"

sum HEALTH_CENTER_VL if POST == 1 & TREATED == 1
scalar P1T1 = round(r(mean), 0.0001) 

sum HEALTH_CENTER_VL if POST == 1 & TREATED == 0
scalar P1T0 = round(r(mean), 0.0001)

sum HEALTH_CENTER_VL if POST == 0 & TREATED == 1
scalar P0T1 = round(r(mean), 0.0001)

sum HEALTH_CENTER_VL if POST == 0 & TREATED == 0
scalar P0T0 = round(r(mean), 0.0001)

matrix A[1,1]= P1T1
matrix A[1,2]= P1T0
matrix A[2,1]= P0T1
matrix A[2,2]= P0T0
matrix A[3,1]= P1T1 - P0T1
matrix A[3,2]= P1T0 - P0T0
matrix A[1,3]= P1T1 - P1T0
matrix A[2,3]= P0T1 - P0T0
matrix A[3,3] = (P1T1 - P0T1) - (P1T0 - P0T0)

putexcel set TABLE_1, replace
putexcel A1 = matrix(A), names

*** We recover the DiD estimator as [(P1T1 - P0T1) - (P1T0 - P0T0)] =  0,0122. Hence, having at least one school constructed in the village increases the proability of having a health center by approximately 1.22 percentage points ***


*(d)

reg HEALTH_CENTER_VL POST TREATED POST*TREATED, cluster(idkab_num)

* alternatively, reg HEALTH_CENTER_VL POST##TREATED, cluster(idkab_num)

outreg2 using TABLE_2.xls, excel replace

xtreg HEALTH_CENTER_VL POST i.year POST*TREATED, fe i(v_id) cluster(idkab_num)
outreg2 using TABLE_2.xls, excel

areg HEALTH_CENTER_VL POST i.year POST*TREATED, absorb(v_id) cluster(idkab_num)
outreg2 using TABLE_2.xls, excel


*(e)

gen INTENSITY = num_PSINPRES1980
gen POST_INTENSITY = POST*INTENSITY

xtreg HEALTH_CENTER_VL POST POST*INTENSITY i.year, fe i(v_id) cluster(idkab_num)

*** INSERT COMMENT HERE ***


*(f)
 
xtreg HEALTH_CENTER_VL i.year i(1990 1993 1996 2000 2003)year#c.INTENSITY if ele1v_post92 < 1994, fe i(v_id) cluster(idkab_num)

xtreg HEALTH_CENTER_VL i.year i(1990 1993 1996 2000 2003)year#c.INTENSITY if ele1v_post92 < 1997 & ele1v_post92 > 1993, fe i(v_id) cluster(idkab_num)

xtreg HEALTH_CENTER_VL i.year i(1990 1993 1996 2000 2003)year#c.INTENSITY if ele1v_post92 > 1996, fe i(v_id) cluster(idkab_num)

*** INSERT COMMENT HERE ***


*(g)

sort v_id year

bysort v_id: gen LAG1=HEALTH_CENTER_VL[_n-1]

xtreg HEALTH_CENTER_VL POST POST*INTENSITY i.year LAG1, fe i(v_id) cluster(idkab_num)

*** INSERT COMMENT HERE ***


*(h)

xtreg HEALTH_CENTER_VL POST POST*INTENSITY i.year i(1990 1993 1996 2000 2003)year#c.dum_otrocop_pre i(1990 1993 1996 2000 2003)year#c.num_bank_pre i(1990 1993 1996 2000 2003)year#c.pedati_pre , fe i(v_id) cluster(idkab_num)

*** INSERT COMMENT HERE ***


*(i)

preserve

drop if year>1990


gen PLACEBO_POST = 1 if year == 1990
replace PLACEBO_POST = 0 if year < 1990
gen PLACEBO_POST_INTENSITY = PLACEBO_POST*INTENSITY

xtreg HEALTH_CENTER_VL PLACEBO_POST PLACEBO_POST*INTENSITY, fe i(v_id) cluster(idkab_num)

restore

*** The coefficient of interest is the one on PLACEBO_POST_INTENSITY, which is positive but has an associated p-value of 0.489, therefore it is insignificant. We are satisfied by this result which sustains our design. Indeed, there appears to be no effect in the pre-treatment years. Thus, it is unlikely that the previosuly estimated causal effect of interest is caused by Omitted Variable Bias, which would have been picked also up in this placebo test and disguised as a significant causal effect. Notice that year fixed effect (the dummy for 1990) have been excluded from the regression because of multicollinearity with PLACEBO_POST. ***
