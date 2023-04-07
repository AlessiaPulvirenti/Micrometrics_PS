
net install rdrobust, ///
from("https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata") replace
*
* Install *rddensity*
net install rddensity, ///
from("https://raw.githubusercontent.com/rdpackages/rddensity/master/stata") replace
*
* Install *lpdensity*
net install lpdensity, /// 
from("https://raw.githubusercontent.com/nppackages/lpdensity/master/stata") replace

*******************************************
**# QUESTION 1

*(a)
use "pset_3.dta", clear

rdplot T X, graph_options(xtitle(Running Variable) ytitle(Treatment Variable))

//The current design is a sharp RDD, because the Treatment variable is a deterministic probability (i.e., it takes either value 1 or 0) of the running variable. There is no partial compliance. 

*(b) ?????
local covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"

*(c)
foreach z in `covariates'{
	local vlabel : variable label `z'
	rdplot `z' X, graph_options(title(`vlabel', size(7pt)) xtitle(Running Variable, size(7pt)) ytitle(Covariate, size(7pt)) legend(off))
	graph rename `z', replace
}
graph combine `covariates'
graph export Graph_1.pdf, replace


*(d)
* Histogram
rdrobust Y X
scalar h_left = -e(h_l)	//Lower bound of the sample used to estimate the polynomial
scalar h_right = e(h_r)	//Upper bound of the sample used to estimate the polynomial
twoway (histogram X if X >=h_left & X < 0, freq width(1) color(blue)) ///
	(histogram X if X >= 0 & X <= h_right, freq width(1) color(green)), xlabel(-30(10)30) ///
	graphregion(color(white)) xtitle(Score) ytitle(Number of Observations) legend(off)	///
	xline(0)
	
graph rename histo, replace
	
*Plot of density (to check for manipulation)
local h_l = h_left
local h_r = h_right
rddensity X, plot plot_range(`h_l' `h_r')

graph rename dens, replace

graph combine histo dens
graph export Graph_2.pdf, replace

*(e)
rddensity X

/*
Running variable: X.
------------------------------------------
            Method |      T          P>|T|
-------------------+----------------------
            Robust |   -1.3937      0.1634
------------------------------------------
*/

/*The null hypothesis of this tests is that the density of the running variable is "continuous" at the cutoff. The p-value of this test tells us that we cannot reject the null hypothesis at a significance of 10%, thus we have no discontinuity in the running variable, and no evidence of manipulation. We can thus conclude that wrt manipulation, this RD design is valid*/

*(f)


