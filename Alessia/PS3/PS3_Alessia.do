
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
**# EXERCISE 1

*(a)
use "pset_3.dta", clear

rdplot T X, graph_options(xtitle(Running Variable) ytitle(Treatment Variable))

//The current design is a sharp RDD, because the Treatment variable is a deterministic probability function (i.e., it takes either value 1 or 0) of the running variable. There is no partial compliance. 

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

/*The null hypothesis of this tests is that the density of the running variable is "continuous" at the cutoff. The p-value of this test tells us that we cannot reject the null hypothesis at a significance of 15%, thus we have no discontinuity in the running variable, and no evidence of manipulation. We can thus conclude that wrt manipulation, this RD design is valid*/

*(f)
forvalues i= -10(5)10{
	rddensity X, c(`i')
}
* Looking at the outcomes for all 4 alternatives of cut-off thresholds, we can see that we cannot reject the null hypothesis at a significance above 15% for three values of the four chosen. However, at a threshold of -5.0, the null hypothesis of no manipulation can be rejected with a p-value of 0.055, i.e., at a standard significance level of 10%. This means that there might have been some manipulation in the running variable around the value of -5, and this could threaten the validity of our RD design unless the researcher is able to prove that the cause of the discontinuity is not to be found in the ability of units to sort themselves above or below a certain value of the running variable. 

*(g)
rdplot Y X, nbins(20 20) graph_options(xtitle(Running Variable) ytitle(Outcome))

*(h)Does electing a mayor from an Islamic party has a significant effect on the educational attainment of women? Do results differ significantly for different kernel choices?

rdrobust Y X, p(1) kernel(uniform)

rdrobust Y X, p(1) kernel(triangular)

//WRITE COMMENT HERE


*(i)
* Generate variables to fit a polynomial of order 4
gen X_2 = X^2
gen X_3 = X^3
gen X_4 = X^4
*
gen X_T = T*X
gen X_T_2 = T*X_2
gen X_T_3 = T*X_3
gen X_T_4 = T*X_4
*
* Estimate global regression, fitting a polynomial of order 4 on our outcome
reg Y T ///
X X_2 X_3 X_4 ///
X_T X_T_2 X_T_3 X_T_4


*(j)


*(k)


*(l)


**# EXERCISE 2

use "fraud_pcenter_final.dta", clear

*(a)

*Generate the "adjusted" measure of distance, both for our proxied variable, _dist, and the dist variable used by Gonzales 2021. 

gen _temp=_dist
replace _temp=-_dist if cov==0

gen temp=dist
replace temp=-dist if cov==0

*Polynomial of order 4
rdplot cov _temp, graph_options(xtitle(New Running Variable) ytitle(Treatment Variable))

*Polynomial of order 1
rdplot cov _temp , graph_options(xtitle(New Running Variable) ytitle(Treatment Variable)) c() p(1) kernel(triangular)

*Polynomial of order 3
rdplot cov _temp , graph_options(xtitle(New Running Variable) ytitle(Treatment Variable)) c() p(3) kernel(triangular)

rdplot cov temp, graph_options(xtitle(Old Running Variable from Gonzales 2021) ytitle(Treatment Variable)) 
*Here, we perform RD plot using the adjusted distance variable directly from Gonzales (2021) - we can see, as written in the paper, that it is a sharp discontinuity. 

rdrobust vote_comb _temp, p(1) kernel(triangular) fuzzy(cov)
rdrobust vote_comb_ind _temp, p(1) kernel(triangular) fuzzy(cov)


/* Looking at the RD plot of the adjusted version of the treatment variable on the adjusted version _dist we can see that there is a fuzzy discontinuity in the probability of being treated (i.e., of being indicated as a locality with coverage) around the boundary point. On the other hand, by plotting coverage on the measure of distance used by the author, i.e., temp, we can see that the discontinuity is sharp. Indeed the Gonzales employs a one-dimensional sharp RDD. For his one-dimensional estimates to be valid, the following identification assumptions must hold: 
---> [WRITE HERE IDENTIFICATION ASSUMPTIONS]

Moreover, Gonzales 2021 identifies some threats to identification: 
--> [WRITE HERE THREATS TO IDENTIFICATION FROM THE ADDITIONAL RESULTS SECTION]
* it must be that that the phone providers do not select where to locate based on characteristics which can also affect the level of fraud in elections, such as education levels, urbanisation levels, population density. 

* In his "Additional Results" section, Gonzales (2021) provides evidence of the fact that selection into coverage is not a cause of concern for the one-dimensional RD design adopted. Another threat to identification is Mobile Coverage Spillovers and Spatial Displacement of Fraud. Coverage in one area can lead to positive spillovers in another area which is officially considered as "uncovered". In particular, and this emerges from our RD plot using the distance measure with proxied latitude, spillovers can result from a coverage boundary which is not sharp. This could be the case if polling centers in non-coverage areas (at the boundary of non-coverage areas) can still benefit from coverage from close areas. If this were to be the case, than non-coverage areas can have lower level of fraud because they benefitted from coverage from other areas. This could downward bias the one-dimensional estimates that Gonzales (2021) shows in Table 2 of page 18 of the paper. A proper fuzzy RDD design cannot be implemented by Gonzales because we are not sure if these polling centers acyally benefitted from some coverage from the neighbouring centers (in that case, they would be non-compliers but we cannot ascertain that). ANOTHER THREAT IS... [CONTINUE]
*/

*(b)
* The setting in which we would not have to change the (sharp) RD design used by Gonzales 2021 would be one in which the cell phone coverage boundaries are sharp. This would be the case if ... (mutually exclusive areas, no spillovers...) --> In this way we would obtain a sharp RD plot of the first stage, as when we plot rdplot cov temp. 






*(c)


*Optimal Bandwidth

*Our dist variable takes already into account the fact that negative values of the variable indicate those that are in non-covered areas, so we do not need to create a temp variable to compute the optimal bandwidth. 

foreach var in /*600 95 ecc*/ comb comb_ind {
		rdbwselect vote_`var' _temp if ind_seg50==1, vce(cluster segment50)
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' _temp if ind_seg50==1 & region2==`r', vce(cluster segment50)
			scalar hopt_`var'_`r'=e(h_mserd)
	}
}

xtset, clear
xtset segment50 pccode


********************************************************************************
* 		B. Local Linear Regression (using distance as forcing variable)
********************************************************************************

foreach var in comb_ind comb {	
	* All regions
	xtreg vote_`var' cov##c.(_temp) if ind_seg50==1 & _temp<=hopt_`var', fe robust 
		est store col1_a_`var'
		est title: "All regions"
		est store col1_a_`var'
		label variable _est_col1_a_`var' "All regions"
		
	* Southeast
	xtreg vote_`var' cov##c.(_temp) if ind_seg50==1 & _temp<=hopt_`var'_1 & ///
	region2==1, fe robust 
		est store col1_b_`var'
		est title: "Southeast"
		est store col1_b_`var'
		label variable _est_col1_b_`var' "Southeast"

	* Northwest
	xtreg vote_`var' cov##c.(_temp) if ind_seg50==1 & _temp<=hopt_`var'_2 & ///
	region2==2, fe robust 
		est store col1_c_`var'
		est title: "Northwest"
		est store col1_c_`var'
		label variable _est_col1_c_`var' "Northwest"
 }
 
 * Putting the table together - Wide version
estout col1_a_comb_* col1_b_comb_* col1_c_comb_*  ///
using "results_onedim_a.tex", replace style(tex) ///
cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
keep(1.cov) mlabels() label title("Panel A - At least one station with Category C fraud") eqlabels(, none)

estout col1_a_comb  col1_b_comb  col1_c_comb  ///
using "results_onedim_b.tex", replace style(tex) ///
label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
keep(1.cov) mlabels() legend title("Panel B - Share of votes under Category C fraud") eqlabels(, none)


include "https://raw.githubusercontent.com/steveofconnell/PanelCombine/master/PanelCombine.do"
panelcombine, use(results_onedim_a.tex results_onedim_b.tex)  columncount(3) paneltitles() save(combined_table.tex)



estout col1_a_comb_*  col1_b_comb_*  col1_c_comb_*   ///
using "_results_onedim_a.tex", replace style(tex) ///
label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
keep(1.cov) mlabels(, none) collabels(, none) eqlabels(, none) ///
stats(Obs Mean Bw Gr, fmt(a3) ///
labels("Observations" "Mean Outside coverage" "Bandwidth (km)" "Neighborhoods"))



outreg [col1_a_comb_* col1_b_comb_* col1_c_comb_*] using Table2_rep.xls, excel keep(1.cov) nocons label() title("Panel A - At least one station with Category C fraud") nor2 noni noobs nonotes replace

outreg2 [col1_a_comb  col1_b_comb  col1_c_comb] using Table2_rep.xls, excel keep(1.cov) nocons label() title("Panel B - Share of votes under Category C fraud") nor2 noni noobs replace




























