clear
cd "C:\Users\elena\OneDrive\Desktop\ESS\2nd year\Microeconometrics\PS\problem set 4"
use pset_4.dta


bysort v_id: egen year_rk = rank(year)
xtset v_id year_rk

*(a)
tab num_PSINPRES1980
gen TREATED = (num_PSINPRES1980 == 1 | num_PSINPRES1980 == 2)
gen POST_TREATED = POST * TREATED

*(b) 
{
local elections "1993 1996 2000"

local cond_1993 "if (ele1v_post92 == 1992 | ele1v_post92 == 1993)"
local cond_1996 "if (ele1v_post92 == 1994 | ele1v_post92 == 1995 | ele1v_post92 == 1996)"
local cond_2000 "if (ele1v_post92 == 1997 | ele1v_post92 == 1998 | ele1v_post92 == 1999 | ele1v_post92 == 2000)"	

foreach x in `elections' {
		
	gen Y_C_`x'=HEALTH_CENTER_VL `cond_`x'' & TREATED==0
	gen Y_T_`x'=HEALTH_CENTER_VL `cond_`x'' & TREATED==1
				
	preserve 

	collapse Y_C_`x' Y_T_`x', by(year)
		
	twoway  (line Y_T_`x' year) (line Y_C_`x' year, lpattern(dash)),  ///
	xline(`x', lcolor(green) lpattern(solid)) ///
	title(Outcome trends) ytitle(Health Center Percentage)  yscale(titlegap(*10))  /// 
	graphregion(color(white)) xlabel(1986 1990 1993 1996 2000 2003) ///
	legend(order(1 "Treatment" 2 "Control") size(medsmall) nobox region(lstyle(none)    lcolor(white)))  
	graph rename Y_`x', replace 
	
	restore 

}

graph combine Y_1993 Y_1996 Y_2000, graphregion(color(white)) row(1)
graph export "Graph_1.png", replace width(2000) height(500)


*(c)
	
*Treated-post
sum HEALTH_CENTER_VL if TREATED==1 & POST==1
scalar AVG_Y_T_1 = r(mean)
*Treated-pre
sum HEALTH_CENTER_VL if TREATED==1 & POST==0
scalar AVG_Y_T_0 = r(mean)
*Control-post
sum HEALTH_CENTER_VL if TREATED==0 & POST==1
scalar AVG_Y_C_1 = r(mean)
*Control-pre
sum HEALTH_CENTER_VL if TREATED==0 & POST==0
scalar AVG_Y_C_0 = r(mean)


matrix Table_1 = [AVG_Y_T_1, AVG_Y_C_1, (AVG_Y_T_1 - AVG_Y_C_1)\ ///
                  AVG_Y_T_0, AVG_Y_C_0, (AVG_Y_T_0 - AVG_Y_C_0)\ ///
				  (AVG_Y_T_1-AVG_Y_T_0), (AVG_Y_C_1-AVG_Y_C_0), ((AVG_Y_T_1-AVG_Y_T_0)-(AVG_Y_C_1-AVG_Y_C_0))]
				  
matrix rownames Table_1 = "Post=1" "Post=0" "Difference 1"
matrix list Table_1

putexcel set "Table_1.xlsx", sheet("Table 1") replace
putexcel A2="POST=1" A3="POST=0" A4="Difference 1" 
putexcel B1="TREATED=1" C1="TREATED=0" D1="Difference 2" 
putexcel B2=matrix(Table_1)


*(d)
reg HEALTH_CENTER_VL POST_TREATED POST TREATED, vce(cluster idkab_num)
outreg2 using Table_2.xls, excel replace lab title("Table 2") ctitle("Pooled OLS") br bdec(4) addtext(Year FEs, NO, Village FEs, NO, Cluster, YES)

xtreg HEALTH_CENTER_VL POST_TREATED POST i.year_rk, fe i(v_id) vce(cluster idkab_num)
outreg2 using Table_2.xls, excel append lab drop(i.year_rk) title("Table 2") ctitle("xtreg") br bdec(4) addtext(Year FEs, YES, Village FEs, YES, Cluster, YES)

areg HEALTH_CENTER_VL POST_TREATED POST i.year_rk, absorb(v_id) vce(cluster idkab_num)
outreg2 using Table_2.xls, excel append lab drop(i.year_rk) title("Table 2") ctitle("areg") br bdec(4) addtext(Year FEs, YES, Village  FEs, YES, Cluster, YES)


*(e)
gen INTENSITY = num_PSINPRES1980
gen POST_INTENSITY = POST*INTENSITY

local year_FEs = "i.year_rk"
local village_FEs = "i.v_id"

xtreg HEALTH_CENTER_VL POST_INTENSITY POST `year_FEs', fe i(v_id) vce(cluster idkab_num)

*(f)
tab year, gen(years)
rename years1 y_1986
rename years2 y_1990
rename years3 y_1993
rename years4 y_1996
rename years5 y_2000
rename years6 y_2003

local census_years "1990 1993 1996 2000 2003"

foreach v in `census_years' {
	gen INTENSITY_`v' = INTENSITY*y_`v'
	local temp = "INTENSITY_`v'"
	local intensities `intensities' `temp'
}

local year_FEs = "i.year_rk"
local village_FEs = "i.v_id"

local elections "1993 1996 2000"
local cond_1993 "if (ele1v_post92 == 1992 | ele1v_post92 == 1993)"
local cond_1996 "if (ele1v_post92 == 1994 | ele1v_post92 == 1995 | ele1v_post92 == 1996)"
local cond_2000 "if (ele1v_post92 == 1997 | ele1v_post92 == 1998 | ele1v_post92 == 1999 | ele1v_post92 == 2000)"	

foreach ele in `elections' {
	
areg HEALTH_CENTER_VL `intensities' `year_FEs' `cond_`ele'', absorb(v_id) vce(cluster idkab_num)

reghdfe HEALTH_CENTER_VL `intensities' `cond_`ele'', absorb(v_id year) vce(cluster idkab_num)

}

*(g)
local year_FEs = "i.year_rk"
local village_FEs = "i.v_id"

xtreg HEALTH_CENTER_VL l.HEALTH_CENTER_VL POST_INTENSITY POST `year_FEs', fe i(v_id) vce(cluster idkab_num)

*(h)
local year_FEs = "i.year"
local village_FEs = "i.v_id"


xtreg HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_rk#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre) `year_FEs', fe i(v_id) vce(cluster idkab_num)

areg HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_rk#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre) `year_FEs', absorb(v_id) vce(cluster idkab_num)

reghdfe HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_rk#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre), absorb(v_id year) vce(cluster idkab_num)


reghdfe HEALTH_CENTER_VL POST_INTENSITY POST, absorb(v_id year) vce(cluster idkab_num)
outreg2 using Table_Comparison.xls, excel replace lab nocons keep(POST_INTENSITY) title("Point H: Comparisons between Diff-in-Diff") ctitle("DiD with Intensities") br bdec(4) addtext(Village FEs, YES, Year FEs, YES, Cluster, YES, Lagged Variable, NO, Baseline Controls, NO)

reghdfe HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_rk#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre), absorb(v_id year) vce(cluster idkab_num)
outreg2 using Table_Comparison.xls, excel append lab nocons keep(POST_INTENSITY) title("Point H: Comparisons between Diff-in-Diff") ctitle("Base Controls") br bdec(4) addtext(Village FEs, YES, Year FEs, YES, Cluster, YES, Lagged Variable, NO, Baseline Controls, YES)

reghdfe HEALTH_CENTER_VL l.HEALTH_CENTER_VL POST_INTENSITY POST, absorb(v_id year) vce(cluster idkab_num)
outreg2 using Table_Comparison.xls, excel append lab nocons keep(POST_INTENSITY) title("Point H: Comparisons between Diff-in-Diff") ctitle("Lagged Variable") br bdec(4) addtext(Village FEs, YES, Year FEs, YES, Cluster, YES, Lagged Variable, YES, Baseline Controls, YES)


*(i)
gen PLACEBO_POST = (year>=1990)
gen PLACEBO_POST_INTENSITY = PLACEBO_POST*INTENSITY

xtreg HEALTH_CENTER_VL PLACEBO_POST_INTENSITY PLACEBO_POST if year < 1993, fe i(v_id) vce(cluster idkab_num)


*(l)
xtset v_id year
areg HEALTH_CENTER_VL POST_TREATED POST i.year, absorb(v_id) vce(cluster idkab_num)
scalar beta_fe=_b[POST_TREATED]

twowayfeweights HEALTH_CENTER_VL v_id year POST_TREATED, type(feTR) controls(POST) path("weights.dta")
ereturn list
scalar sigma_fe1 = e(lb_se_te)

di sigma_fe1

preserve
use "weights.dta", clear
drop if weight==0
tabstat weight, stats(sd mean sum count) save
return list
scalar weights_num = r(StatTotal)[4,1]
scalar sd_weights = r(StatTotal)[1,1]*weights_num
scalar sigma_fe2 = abs(beta_fe)/sd_weights
di sigma_fe2
restore














