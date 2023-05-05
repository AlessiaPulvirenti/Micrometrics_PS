

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

graph combine gr_1993 gr_1996 gr_2000, rows(1) cols(3) iscale(0.9) xsize(10) ysize(4)
gr export "Graph_1.pdf", replace 
restore 

* COMMENT HERE

*(c)
*TO DO!!!!!!!!!!!!




*(d)
*i - Pooled OLS

reg HEALTH_CENTER_VL TREATED POST POST_TREATED, cluster(idkab_num)
est store POLS

*COMMENT ON WHETHER THE RESULTS ARE THE SAME AS IN THE TABLE ABOVE

*ii - FE with xtreg

xtreg HEALTH_CENTER_VL POST POST_TREATED i.year, fe i(v_id) cluster(idkab_num)
est store FE_XTREG

* Explain why they ask us to include only the the interaction variable, and not the TREATED variable (?) as asked in the hint. 

*iii FE with areg

areg HEALTH_CENTER_VL POST POST_TREATED i.year, absorb(v_id) cluster(idkab_num)
est store FE_AREG

outreg2 [POLS, FE_XTREG, FE_AREG] using "TABLE_2.xls", excel replace nocons title("Estimations - point d") keep(POST_TREATED POST) //There's a mistake here, fix later! 

*(e)
gen INTENSITY = num_PSINPRES1980
gen POST_INTENSITY = INTENSITY*POST

xtreg HEALTH_CENTER_VL POST POST_INTENSITY i.year, fe i(v_id) cluster(idkab_num)

*This specification allows us to estimate the differential treatment intensity (depending on the number of INPRES schools constructed in the village), rather than estimating simply the treatment effect of having at least 1 INPRES school being constructed with the village (measured) with a dummy variable. 

*(f)
foreach year in 1990 1993 1996 2000 2003{
	gen INTENSITY_`year' = 1*INTENSITY if year == `year'
	replace INTENSITY_`year' = 0 if INTENSITY_`year'==.
}

xtreg HEALTH_CENTER_VL INTENSITY_* i.year if v_1993 == 1, fe i(v_id) cluster(idkab_num) 
xtreg HEALTH_CENTER_VL INTENSITY_* i.year if v_1996 == 1, fe i(v_id) cluster(idkab_num) 
xtreg HEALTH_CENTER_VL INTENSITY_* i.year if v_2000 == 1, fe i(v_id) cluster(idkab_num) 

**COMMENT***


*(g)
sort v_id year
bysort v_id: gen HEALTH_CENTER_VL_lag1=HEALTH_CENTER_VL[_n-1]

xtreg HEALTH_CENTER_VL HEALTH_CENTER_VL_lag1 POST POST_INTENSITY i.year, fe i(v_id) cluster(idkab_num)

*COMMENT ON THE POTENTIAL SOURCES OF BIAS ARISING FROM ESTIMATING SUCH A SPECIFICATION - LOOK AT A&P (CH. 5 AT THE END)

*(h) 
* We included each of the unbalanced controls interacted with year dummies. 

xtreg HEALTH_CENTER_VL POST POST_INTENSITY dum_otrocop_pre#i.year num_bank_pre#i.year pedati_pre#i.year i.year, fe i(v_id) cluster(idkab_num)

*ARE THE RESULTS ROBUST TO THE INCLUSION OF UNBALANCED BASELINE CONTROLS? - ADD COMMENT
*ARE RESULTS ROBUS TO THE INCLUSION OF LAGGED - ADD COMMENT***

* (i)
preserve 

keep if year == 1986 | year == 1990

replace POST =. 
replace POST = 1 if year == 1990
replace POST = 0 if year == 1980
sum POST

xtreg HEALTH_CENTER_VL POST POST_INTENSITY i.year, fe i(v_id) vce(cluster idkab_num) //insufficient observations???????

restore

*(j)
*comment






