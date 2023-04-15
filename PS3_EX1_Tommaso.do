cd "/Users/tommasoroccuzzo/Library/Mobile Documents/com~apple~CloudDocs/Tommaso/PS 3"

********************************************************************************
********************************** EXERCISE 1 **********************************
********************************************************************************


use pset_3.dta, clear

		
* (a) *

rdplot T X, c(0) graph_options(ytitle("Treatment Variable") xtitle("Running Variable"))

*** We immediately notice that the RD design is sharp and not fuzzy. At the cutoff value c = 0 for the running variable, the treatment variable T displays a clear and sharp discontinuity, jumping from 0 to 1. Hence, this is evidence of the fact that treatment assignment is a deterministic function of our covariate X, and in particular T = 0 for X < 0 and T = 1 for X > 0. ***


* (b) *

global covariates = "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"

matrix Table1 = J(9, 4, .)

local i = 1

foreach var of varlist $covariates {
	
	qui rdrobust `var' X, kernel(triangular) p(1) bwselect(mserd)
	 
	matrix Table1[`i',1] = round(e(h_l), .001)
	matrix Table1[`i',2] = round(e(tau_cl), .001)
	matrix Table1[`i',3] = round(e(pv_rb), .001)
	matrix Table1[`i',4] = round(e(N_h_l) + e(N_h_r), .001)
	
	local i = `i' + 1
}

matrix colnames Table1 = "MSE-Optimal Bandwidth" "RD Estimator" "p-value" "Effective Number of Observations"

matrix list Table1


foreach var of varlist $covariates {

	local x : variable label `var'
	
	gen label_`var' = "`x'"
	
}

putexcel set Table1.xlsx, replace

putexcel A1 = "Variable"
putexcel B1 = "Label"


local i = 2
foreach var of varlist $covariates{
	putexcel B`i' = label_`var'
	putexcel A`i' = "`var'"
	drop label_`var'
	local i = `i' + 1
}

putexcel C1=matrix(Table1), colnames


* (c) *

local i = 1

foreach var of varlist $covariates{
	local x : variable label `var'
	local y : variable label X
	qui rdplot `var' X, graph_options(title("RD plot for: `x'") ytitle("Dependent Variable: `var'") xtitle("Running Variable: X, `y'"))
	graph save mygraph`i'.gph, replace
	local i = `i' + 1
}

graph combine mygraph1.gph mygraph2.gph mygraph3.gph mygraph4.gph mygraph5.gph mygraph6.gph mygraph7.gph mygraph8.gph mygraph9.gph, iscale (*0.45)
graph export Graph_1.png, replace


* (d) *

******* ATTENTION!!! *******

qui rdrobust Y X, c(0) p(1) kernel(triangular) bwselect(mserd)
scalar bw_h = e(h_l)
scalar bw_l = -e(h_l)

twoway (histogram X if X < 0 & X > bw_l, freq width(1) color(blue)) (histogram X if X >= 0 & X < bw_h, freq width(1) color(red)), xlabel(-25(5)25) xline(0, lwidth(0.5) lcolor(green)) graphregion(color(white)) xtitle("Running Variable")  ytitle("Number of Observations") legend(off) title("Hystogram: frequency of obs. around the cutoff", size(4))
graph save X_Y.gph, replace

rddensity X, plot graph_opt(xline(0, lwidth(0.5) lcolor(green)) legend(off) graphregion(color(white)) ytitle("Density") xtitle("Running Variable") title("Estimated Density of the Running Variable", size(4)))
graph save density.gph, replace

graph combine X_Y.gph density.gph, cols(1)
graph export X_Y.png, replace


* (e) *

rddensity X

* With a p-value of 0.16 we are not ablet to reject the null hypothesis at any conventional significance level. Therefore, our data suggests that there is no statistically significant discontinuity in the running variable at the cutoff. thus indicating that the RD design is potentially invalid, at least for the suggested cutoff of c=0. It might be that at different cutoffs we obtain a significant discontinuity in the running variable. *


* (f) *

rddensity X, c(-10)

* p-value: 0.4259 *

rddensity X, c(-5)

* p-value: 0.0547 *

rddensity X, c(5)

* p-value: 0.0547 *

rddensity X, c(10)

* p-value: 0.5026 *

* The manipulation testing plots indicate that we can easily disregard the cutoffs c = +/-10. Instead, the cutoffs c = +/-5 would lead us to reject the null hypothesis of no discontinuity in the running variable for a confidence level of 10%. *


* (g) *	

rdplot Y X, nbins(20 20) graph_options(title("Share of high-school educated 15-20 y.o. woman vs Islamic vote margin in 1994", size(3)) ytitle("Outcome") xtitle("Running Variable"))


* (h) *	


rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)

rdrobust Y X, p(1) kernel(uniform) bwselect(mserd)

	
* (i) *

* (j) *
	
* (k) *

* (l) *	
	
}

********************************************************************************
********************************** EXERCISE 2 **********************************
********************************************************************************

use fraud_pcenter_final.dta, clear

* (a) *


* (b) *
	
* (c) *

