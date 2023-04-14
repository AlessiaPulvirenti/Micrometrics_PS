cd "/Users/tommasoroccuzzo/Library/Mobile Documents/com~apple~CloudDocs/Tommaso/Appunti/Bocconi/2021-2022/2° Semestre/Microeconometrics/Problem Sets/PS 3"

********************************************************************************
********************************** EXERCISE 1 **********************************
********************************************************************************


use pset_3.dta, clear

		
* (a) *

rdplot T X, c(0) graph_options(ytitle("Treatment Variable") xtitle("Running Variable"))

*** INSERT COMMENT HERE ***


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
	quietly rdplot `var' X, graph_options(title("RD plot for `var'") ytitle("Treatment Variable") xtitle("Running Variable: `var'"))
	graph save mygraph`i'.gph, replace
	local i = `i' + 1
}

graph combine mygraph1.gph mygraph2.gph mygraph3.gph mygraph4.gph mygraph5.gph mygraph6.gph mygraph7.gph mygraph8.gph mygraph9.gph, iscale (*0.45) saving(Graph_1.gph, replace)


* (d) *

******* ATTENTION!!! *******

******* FOR THE 1st GRAPH DO THEY WANT ALL OBSERVATIONS OR ONLY THOSE WITHIN THE BANDWITH??? ********

twoway (histogram X if X < 0, freq width(2) color(blue)) (histogram X if X >= 0, freq width(2) color(red)), xlabel(-100(20)100) xline(0, lwidth(0.5) lcolor(green)) graphregion(color(white)) xtitle("Running Variable")  ytitle("Number of Observations") legend(off) title("Hystogram: frequency of obs. around the cutoff", size(4))
graph save X_Y.gph, replace

rddensity X, plot graph_opt(xline(0, lwidth(0.5) lcolor(green)) legend(off) ytitle("Density") xtitle("Running Variable") title("Estimated Density of the Running Variable", size(4)))
graph save density.gph, replace

graph combine X_Y.gph density.gph, cols(1) saving(Graph_2.gph, replace)


* (e) *

rddensity X

******* ADD COMMENT HERE *******


* (f) *

rddensity X, c(-10)
rdplot T X, c(-10)

rddensity X, c(-5)
rdplot T X, c(-5)

rddensity X, c(5)
rdplot T X, c(5)

rddensity X, c(10)
rdplot T X, c(10)

******* ADD COMMENT HERE *******


* (g) *	

rdplot Y X, nbins(20 20) graph_options(title("Share of high-school educated 15-20 y.o. woman vs Islamic vote margin in 1994", size(3)) ytitle("Outcome") xtitle("Running Variable"))


* (h) *	

******* ATTENTION ********

rdrobust Y T, p(1) kernel(triangular)

rdrobust Y T, p(1) kernel(uniform)

	
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


rdplot vote_comb _dist, c(0) p(2) kernel(triangular)

rdplot vote_comb_ind _dist, c(0) p(2) kernel(triangular)




* (b) *
	
* (c) *

