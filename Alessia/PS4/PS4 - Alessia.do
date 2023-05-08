					*******************************************
					***										***
					***		  	Problem Set 4				***
					***										***
					***				GROUP 8					***	
					***										***
					***		Aleksa Mitrovic		3100079		***
					***		Elena Neri			3070190		***
					***		Alessia Pulvirenti	3060894		***
					***		Tommaso Roccuzzo	3080613		***
					***										***
					*******************************************



use "pset_4.dta", clear

*(a)
gen TREATED = (num_PSINPRES1980 == 1 | num_PSINPRES1980 == 2)
gen POST_TREATED = POST*TREATED

*(b)
gen v_1993 = inlist(ele1v_post92, 1992, 1993)
gen v_1996 = inlist(ele1v_post92, 1994, 1995, 1996)
gen v_2000 = inlist(ele1v_post92, 1997, 1998, 1999, 2000)

preserve
forvalues i = 0/1{
	foreach year in 1993 1996 2000{
		gen Y_`i'_v_`year' = HEALTH_CENTER_VL if (TREATED == `i' & v_`year' == 1)
	}
}

collapse Y_0_v_1993 Y_0_v_1996 Y_0_v_2000 Y_1_v_1993 Y_1_v_1996 Y_1_v_2000, by(year)

foreach year in 1993 1996 2000{
	twoway  (line Y_1_v_`year' year) (line Y_0_v_`year' year, lpattern(dash)), xline(80) title(Parallel Trend Assumption) ytitle(Probability of a health center in a village, size(7pt)) legend(order(1 "Treatment" 2 "Control")) xline(`year') name(gr_`year', replace)
}

restore

graph combine gr_1993 gr_1996 gr_2000, rows(1) cols(3) iscale(0.9) xsize(10) ysize(4)
gr export "Graph_1.pdf", replace 

* COMMENT HERE!!!!!!!!!!!!!!!!!!

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

* COMMENT HERE!!!!!!!!!!!!!!!!!!

*(d)
*i - POOLED OLS
reg HEALTH_CENTER_VL POST_TREATED POST TREATED, vce(cluster idkab_num)
outreg2 using Table_2.xls, excel replace lab title("Table 2") ctitle("Pooled OLS") br bdec(4) addtext(Year FEs, NO, Village FEs, NO, Cluster, YES)

*COMMENT ON WHETHER THE RESULTS ARE THE SAME AS IN THE TABLE ABOVE !!!

*ii - FE with xtreg
xtreg HEALTH_CENTER_VL POST_TREATED POST i.year, fe i(v_id) vce(cluster idkab_num)
outreg2 using Table_2.xls, excel append lab drop(i.year_rk) title("Table 2") ctitle("xtreg") br bdec(4) addtext(Year FEs, YES, Village FEs, YES, Cluster, YES)

* Explain why they ask us to include only the the interaction variable, and not the TREATED variable (?) as asked in the hint !!

*iii - FE with xtreg
areg HEALTH_CENTER_VL POST_TREATED POST i.year, absorb(v_id) vce(cluster idkab_num)
outreg2 using Table_2.xls, excel append lab drop(i.year_rk) title("Table 2") ctitle("areg") br bdec(4) addtext(Year FEs, YES, Village  FEs, YES, Cluster, YES)


*(e)
gen INTENSITY = num_PSINPRES1980
gen POST_INTENSITY = INTENSITY*POST

xtreg HEALTH_CENTER_VL POST POST_INTENSITY i.year, fe i(v_id) vce(cluster idkab_num)

*This specification allows us to estimate the differential treatment intensity (depending on the number of INPRES schools constructed in the village), rather than estimating simply the treatment effect of having at least 1 INPRES school being constructed with the village (measured) with a dummy variable.  [CONTINUE] !!!!

*(f)
foreach year in 1990 1993 1996 2000 2003{
	gen INTENSITY_`year' = 1*INTENSITY if year == `year'
	replace INTENSITY_`year' = 0 if INTENSITY_`year'==.
}

xtreg HEALTH_CENTER_VL INTENSITY_* i.year if v_1993 == 1, fe i(v_id) vce(cluster idkab_num) 
xtreg HEALTH_CENTER_VL INTENSITY_* i.year if v_1996 == 1, fe i(v_id) vce(cluster idkab_num) 
xtreg HEALTH_CENTER_VL INTENSITY_* i.year if v_2000 == 1, fe i(v_id) vce(cluster idkab_num) 

* For the first two groups of villages, i.e., those who carried out elections in year 1992-1993 and 1994-1996, we can see the coefficients of the interaction between INTENSITY and years before treatment are not statistically significant. However, for the group of villages that carried out elections in years 1997-2000, have the coefficient of INTENSITY*(year == 1996) statistically significant. This might raise concerns about the parallel trend assumption, showing that the number of health centers has been increasing in the years before the first elections with potentially more educated politicians take place. However, Martinez-Bravo shows that looking at many outcomes that measure public service provision, in out of 18 regressions, only two tests are significant at 10% level (and exactly the one we found). This outcome can be due simply to chance (Online Appendix, Table 3A). 


*(g)
sort v_id year
bysort v_id: gen HEALTH_CENTER_VL_lag1=HEALTH_CENTER_VL[_n-1]

xtreg HEALTH_CENTER_VL HEALTH_CENTER_VL_lag1 POST POST_INTENSITY i.year, fe i(v_id) cluster(idkab_num)

* **** FIX THIS HERE FROM A&P ***
*When introducing the laggend dependent variable, the problem is thatthe differenced residual is necessarily correlated with the lagged dependent variable, HEALTH_CENTER_VL_lag1, because both are a function of it 1: Consequently, OLS estimates of (5.3.6) are not consistent for the parameters in (5.3.5), a problem Örst noted by Nickell (1981). This problem can be solved, though the solution requires strong assumptions. The easiest solution is to use yit 2 as an instrument for yit 1 in (5.3.6).10 But this requires that yit 2 be uncorrelated with the di§erenced residuals, it. This seems unlikely since residuals are the part of earnings left over after accounting for covariates. Most peopleís earnings are highly correlated from one year to the next, so that past earnings are an excellent predictor of future earnings and earnings growth . If it is serially correlated, there may be no consistent estimator for (5.3.6). (Note also that the IV strategy using yit 2 as an instrument requires at least three periods to obtain data for t; t 1; and t 2).

*(h) 
* We included each of the unbalanced controls interacted with year dummies. 

foreach variable in dum_otrocop_pre num_bank_pre pedati_pre{
	foreach year in 1990 1993 1996 2000 2003{
		gen `variable'_`year' = 1*`variable' if year == `year'
		replace `variable'_`year' = 0 if missing(`variable'_`year')
}
}
xtreg HEALTH_CENTER_VL POST POST_INTENSITY dum_otrocop_pre_* num_bank_pre_* pedati_pre_* i.year, fe i(v_id) vce(cluster idkab_num)

*ARE THE RESULTS ROBUST TO THE INCLUSION OF UNBALANCED BASELINE CONTROLS? - ADD COMMENT
*ARE RESULTS ROBUS TO THE INCLUSION OF LAGGED - ADD COMMENT***

* (i)
preserve 

keep if year == 1986 | year == 1990

replace POST =. 
replace POST = 1 if year == 1990
replace POST = 0 if year == 1986
replace POST_INTENSITY = POST*INTENSITY
sum POST
sum POST_INTENSITY
tab year

xtreg HEALTH_CENTER_VL POST POST_INTENSITY i.year, fe i(v_id) vce(cluster idkab_num)

restore

* COMMENT HERE: Do we find statistically significant effects? What do these results tell us about the plausibility of the results being explained by an omitted variable?

*

*(j)
* Write comment here (Elena)


*(l)
*ssc install twowayfeweights
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

*(m)
*In our code above we have computed the weights and the standard deviation of the weights, as Clément de Chaisemartin and Xavier D'Haultfoeuille suggest. The computed standard deviation of the weights is 0.6796259. This is the minimal value of standard deviation of the ATEs in each (g-t) cell for which the estimated FE beta and the ATE may be of opposite sign. This means that if this value is very low, close to 0, then under a small amount of treatment effect heterogenity, the ATE and the resulting /beta_fe can be of opposite sign. On the other hand, if the value is high, this means that the two are of opposite sign only under a large (and implausible) amount of treatment effect variability. In our case, the standard deviation is fairly low, and thus we we cannot be confident about the sign of our estimate. Also, this would constitute a serious concern when interpreting the FE coefficient as ATT.







