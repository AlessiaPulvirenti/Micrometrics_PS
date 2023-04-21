					*******************************************
					***										***
					***		  Problem Set 3					***
					***										***
					***			GROUP 8						***	
					***										***
					***		Aleksa Mitrovic		3100079		***
					***		Elena Neri			3070190		***
					***		Alessia Pulvirenti	3060894		***
					***		Tommaso Roccuzzo	3080613		***
					***										***
					*******************************************
											

*COMMANDS TO HAVE INSTALLED TO RUN THIS CODE

/*ssc install ivreg2, replace

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

*/

*******************************************
**# EXERCISE 1

*(a)
use "pset_3.dta", clear
rdplot T X, graph_options(xtitle(Running Variable) ytitle(Treatment Variable))

*** We immediately notice that the RD design is sharp and not fuzzy. At the cutoff value c = 0 for the running variable, the treatment variable T displays a clear and sharp discontinuity, jumping from 0 to 1. Hence, this is evidence of the fact that treatment assignment is a deterministic function of our covariate X, and in particular T = 0 for X < 0 and T = 1 for X > 0. ***

*(b) 
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

putexcel A1 = "Label"


local i = 2
foreach var of varlist $covariates{
	putexcel A`i' = label_`var'
	drop label_`var'
	local i = `i' + 1
}

putexcel C1=matrix(Table1), colnames


*(c)
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


*(d)
*Domain of both graphs restricted to the rdrobust bandwidths

* Histogram

twoway (histogram X if X < 0 & X > bw_l, freq width(1) color(blue)) (histogram X if X >= 0 & X < bw_h, freq width(1) color(red)), xlabel(-25(5)25) xline(0, lwidth(0.5) lcolor(green)) graphregion(color(white)) xtitle("Running Variable")  ytitle("Number of Observations") legend(off) title("Hystogram: frequency of obs. around the cutoff", size(4))
graph save X_Y.gph, replace

*Plot of density (to check for manipulation)
local h_l = bw_l
local h_r = bw_h 
rddensity X, plot graph_opt(xline(0, lwidth(0.5) lcolor(green)) legend(off) graphregion(color(white)) ytitle("Density") xtitle("Running Variable") title("Estimated Density of the Running Variable", size(4))) plot_range(`h_l' `h_r')
graph save density.gph, replace


graph combine X_Y.gph density.gph, iscale(*0.80)
graph export Graph_2.png, replace

*Alternative graph with no restrictions on the x axis for the density graph

*Plot of density (to check for manipulation) - with no restrictions
rddensity X, plot graph_opt(xline(0, lwidth(0.5) lcolor(green)) legend(off) graphregion(color(white)) ytitle("Density") xtitle("Running Variable") title("Estimated Density of the Running Variable", size(4))) 
graph save density_alt.gph, replace

graph combine X_Y.gph density_alt.gph, iscale(*0.80)
graph export Graph_2_alternative.png, replace


*(e)
rddensity X
/*The null hypothesis of this tests is that the density of the running variable is "continuous" at the cutoff. The p-value of this test tells us that we cannot reject the null hypothesis at a significance of 15%, thus we have no discontinuity in the running variable, and no evidence of manipulation. We can thus conclude that wrt manipulation around the cutoff of 0, this RD design is valid*/

*(f)
forvalues i= -10(5)10{
	rddensity X, c(`i')
}
* The manipulation test indicates that for cutoffs c = -10, -5, 10, there is no evidence of discontinuity (manipulation) in the running variable X. On the other hand, for a cutoff c = -5, we fail to reject the null of continuity at a significance level of 10% This means that there might have been some manipulation in the running variable around the value of -5, and this could threaten the validity of our RD design unless the researcher is able to prove that the cause of the discontinuity is not to be found in the ability of units to sort themselves above or below a certain value of the running variable. 

*(g)
rdplot Y X, nbins(20 20) graph_options(title("Share of high-school educated 15-20 y.o. woman vs Islamic vote margin in 1994", size(3)) ytitle("Outcome") xtitle("Running Variable")) binselect(es)


*(h)
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
rdrobust Y X, p(1) kernel(uniform) bwselect(mserd)

*Yes, electing a mayor from an Islamic party has a statistically significant effect on the educational attainment of women. In particular, the treatment effect of Islamic rule on the educational attainment of women is significant at 95% confidence level and it is about 3 percentage points. Estimated coefficients and p-values are no significantly different under both kernels. 


*(i)
*We estimate the effect of T on Y using a global approach through a global WLS regression with weights defined according to the triangular kernel formula. 

* We generate variables to fit a polynomial of order 4.
gen X_2 = X^2
gen X_3 = X^3
gen X_4 = X^4
gen X_T = T*X
gen X_T_2 = T*X_2
gen X_T_3 = T*X_3
gen X_T_4 = T*X_4

*We define triangular weights globally.
gen glob_weights = .
replace glob_weights = (1 - abs(X/100)) if X < 0 
replace glob_weights = (1 - abs(X/100)) if X >= 0 

*We estimate the global polynomial regression of order 4.
reg Y T X X_2 X_3 X_4 X_T X_T_2 X_T_3 X_T_4 [aw = glob_weights]


*(j)
*We now estimate the effect of T on Y using a local approach by restricting our sample to a window within the optimal bandwidth obtained via *rdrobust*'s bandwidth selection *mserd*. 
rdrobust Y X, p(1) all kernel(triangular) bwselect(mserd)
local opt_i = e(h_l)

*We define triangular weights locally.
gen loc_weights = .
replace loc_weights = (1 - abs(X/`opt_i')) if X < 0 & X >= -`opt_i'
replace loc_weights = (1 - abs(X/`opt_i')) if X >= 0 & X <= `opt_i'

*We estimate the local linear regression on the left of the cut-off.
reg Y X [aw=loc_weights] if X < 0 & X >= -`opt_i'
matrix coef_left = e(b)
matrix var_left = e(V)
scalar intercept_left = coef_left[1, 2]

*We estimate the local linear regression on the right of the cut-off.
reg Y X [aw=loc_weights] if (X <= `opt_i' & X >=0) 
matrix coef_right = e(b)
matrix var_right = e(V)
scalar intercept_right = coef_right[1, 2]

*We compute the treatment effect.
scalar difference = intercept_right - intercept_left
matrix var_conventional = var_left + var_right
scalar se_difference = sqrt(var_conventional[2,2])

scalar list difference
*difference =  3.0195263
scalar list se_difference
*se_difference =  1.1676311

*We re-run the regression performed in (h).
rdrobust Y X, p(1) kernel(triangular)
ereturn list 
display e(tau_cl)
*3.0195263

*By estimating the effect of T on Y using the local approach we get the same results with respect to those obtained in (h). We run a WLS with weights defined according to the triangular kernel formula. As oppose to the uniform kernel and OLS, the triangular kernel produces estimators using non-uniform weights, assigning greater weights to observations closer to the cut-off. 
*In fact, running the local linear regressions without the weights leads to different results. To verify this we re-estimated the regression excluding the weights.  

/*We estimate the local linear regression on the left of the cut-off.
reg Y X if X >=-`opt_i' & X < 0 
matrix coef_left = e(b)
matrix var_left = e(V)
scalar intercept_left = coef_left[1, 2]

*We estimate the local linear regression on the right of the cut-off.
reg Y X if X >= 0 & X <=`opt_i'
matrix coef_right = e(b)
matrix var_right = e(V)
scalar intercept_right = coef_right[1, 2]

*We compute the treatment effect.
scalar difference = intercept_right - intercept_left
matrix var_conventional = var_left + var_right
scalar se_difference = sqrt(var_conventional[2,2])

scalar list difference
*difference =  3.0595105
scalar list se_difference
*se_difference =  1.2974771

*We re-run the regression performed in point (h)
rdrobust Y X, p(1) kernel(triangular)
ereturn list 
display e(tau_cl)
*3.0195263

gen difference_2 = difference - e(tau_cl)
display difference_2
*0.0399842

*As expected not including the weights in the local linear regressions leads to different results with respect to those obtained in (h). The difference between the treatment effect coefficients is 0.0399842.*/


*(k)
*We obtain and store in the local "opt_i" the optimal bandwidths obtained in (h) and re-run *rdrobust* to check the robustness of the results to changes in bandwidths. 
rdrobust Y X, all kernel(triangular) p(1) 
local opt_i = e(h_l)

local bwd050 = 0.5*`opt_i'
local bwd075 = 0.75*`opt_i'
local bwd125 = 1.25*`opt_i'
local bwd15 = 1.5*`opt_i'

di `opt_i' 
di `bwd050' 
di `bwd075' 
di `bwd125' 
di `bwd15'

matrix define R = J(5, 6, .)
global bandwidths "`opt_i' `bwd050' `bwd075' `bwd125' `bwd15'"
local r = 1
foreach k of global bandwidths {
	rdrobust Y X, all kernel(triangular) p(1) h(`k')
	matrix R[`r', 1] = `k'
	matrix R[`r', 2] = e(tau_cl)
	matrix R[`r', 3] = e(tau_bc)
	matrix R[`r', 4] = e(se_tau_rb)
	matrix R[`r', 5] = R[`r', 2] - invnormal(0.975) * R[`r', 4]
	matrix R[`r', 6] = R[`r', 2] + invnormal(0.975) * R[`r', 4]
	local r = `r' + 1
}

preserve
	clear
	svmat R
	twoway (rcap R5 R6 R1, lcolor(navy)) /*
	*/ (scatter R2 R1, mcolor(cranberry) yline(0, lcolor(black) lpattern(dash))), /*
	*/ graphregion(color(white)) /*
	*/ xlabel(8.62 12.93 17.24 21.55 25.86, labsize(small)) /*
	*/ ytitle("RD Treatment Effect") /*
	*/ legend(off) xtitle("Bandwidth") yscale(range(-5 10)) 
	graph export "Graph_3.png", replace
restore

*The plot of the five RD estimates indicates results are quite robust to alternative bandwidths: results are similar in magnitude, ranging from 1.8047 (bandwith 0.5*opt_i) and 3.0195 (bandwith opt_i). Increasing the bandwidths increases the sample size, and consequently increases the precision of the estimates obtained. This is why we observe narrower confidence intervals as the bandwidths increase (bandwith 0.5*opt_i estimate's confidence interval is the largest and not significantly different from zero). However, in a RD setting we must keep in mind the existing trade-off between greater sample size - improving precision - and internal validity. 


*(l)
rddensity x, all
*Conventional and robust methods both indicate that we cannot reject the null hypothesis of manipulation/discontinuity in the alternative running variable.

foreach i in -10 -5 5 10{
	rddensity x, all c(`i')
}
*Futhermore, no significant discountinuity in the alternative running variable is found at the 4 alternatives of cut-off thresholds considered in point (f), which seems indicate the alternative running variable x is valid.

***Graphical analysis***
rdplot T x, graph_options(title(T-x Discontinuity) ///
	xtitle(Alternative Running Variable) ytitle(Treatment Variable)) 
graph rename T_x, replace
*Fuzzy RDD design. There is a discountinuous jump in the probability of treatment after the cutoff. 

rdplot Y x, graph_options(title(Y-x Discontinuity) ///
	xtitle(Alternative Running Variable) ytitle(Treatment Variable)) 
graph rename Y_x, replace
	
graph combine T_x Y_x
graph export "Graph_5.png", replace

*We employ graphical analysis to check whether the RD design with x as a running variable is a valid design. From the first graph (the rdplot of T on x) there appears to be a small discontinuity in the probability of treatment around the cutoff. If we were to employ x as a valid running variable, we would have to adopt a fuzzy RD design because the probability of treatment is not deterministically determined by the running variable (the probability of treatment does not jump from 0 to 1 at the cut-off). 
*However, we also notice that the probability of treatment starts to increase before the threshold and from the graph we cannot establish whether there is a significant jump in treatment status at the cut-off. Hence, to check whether this discountinuity is significant we estimate the treatment effect using *rdrobust* and check whether the first stage is significant.

rdrobust Y x, fuzzy(T) bwselect(mserd)
*We observe that the first stage estimate is not significant, hence we conclude that the RD design with x as an alternative running variable is not a valid design. 



**# EXERCISE 2

cd "C:\Users\pulvi\OneDrive - Università Commerciale Luigi Bocconi\Depr(ESS)ion\2. Second Year\Micrometrics\PS\Micrometrics_PS\Alessia\PS3"

use "fraud_pcenter_final.dta", clear

*(a)

* Generate the "adjusted" measure of distance, both for our proxied variable, _dist, and the dist variable used by Gonzales 2021, so that polling centres located in areas with coverage == 0, will have a negative distance from the cutoff. 

gen _temp=_dist
replace _temp=-_dist if cov==0

gen temp=dist
replace temp=-dist if cov==0

*RD plotting of the treatment variable, cov, on the new adjusted running variable, _temp

*Polynomial of order 4
rdplot cov _temp, graph_options(xtitle(Adjusted New Running Variable (_temp)) ytitle(Treatment Variable (Coverage)) title(RDplot of the New Running variable on Coverage))

*Polynomial of order 1
rdplot cov _temp , graph_options(xtitle(Adjusted New Running Variable (_temp)) ytitle(Treatment Variable (Coverage)) title(RDplot of the New Running variable on Coverage)) c() p(1) kernel(triangular)

*RD plotting of the "OLD" adjusted running variable, used by Gonzales 2021, on the treatment variable
rdplot cov temp, graph_options(xtitle(Old Running Variable from Gonzales 2021) ytitle(Treatment Variable)) 
*Here, we perform RD plot using the adjusted distance variable directly from Gonzales (2021) - from the graph it can be easily seen that the discontinuity is sharp. Indeed, Gonzales 2021 employs a sharp spatial regression discontinuity. 

* RD estimation using the new running variable: 
rdrobust vote_comb_ind _temp, p(1) kernel(triangular) fuzzy(cov) bwselect(mserd)
rdrobust vote_comb _temp, p(1) kernel(triangular) fuzzy(cov) bwselect(mserd)

rdrobust vote_comb_ind _dist, p(1) kernel(triangular) fuzzy(cov) bwselect(mserd)
rdrobust vote_comb _dist, p(1) kernel(triangular) fuzzy(cov) bwselect(mserd)


/* Looking at the RD plot of the treatment variable on the adjusted version of _dist we can see that there is a fuzzy discontinuity in the probability of being treated (i.e., of being indicated as a locality with coverage) around the boundary point. On the other hand, by plotting coverage on the measure of distance used by Gonzales, i.e., temp, we can see that the discontinuity is sharp. 

Indeed the Gonzales employs a one-dimensional sharp RDD. For his one-dimensional estimates to be valid, the following identification assumptions must hold: 
1. Continuity of potential outcomes at the cutoff point: the potential outcome functions must be continuous at 0, the selected 		cut-off point. In other words, absent the treatment, the potential outcome would not have jumped, i.e. there are no competing interventions at the boundary points. In Gonzales' setting, this assumption implies that polling centres characteristics must "transition" smoothly across the treatment boundary. In this way, centres in the non-coverage regions must be a valid counterfactual for centres in the coverage regions. 
2. Assignment is free of manipulation: there must be no endogenous sorting of polling centers or sorting of villages near the boundary. To claim the validity of the RD design, the author performs the Cattaneo, Jansson, Ma's (2019) test for breaks in the density of the running variable at the treatment boundary. Moreover, Gonzales (2021) notes that the presence of endogenous sorting of polling centers is, in any case, unlikely, since polling centres location was decided by the location of settlements rather than cellphone coverage by the UN-led IEC. With regards to the endogenous sorting of villages, or of people in villages, Gonzales notes that because of the rapid expansion of cellphone coverage in Afghanistan, the incentives for a household to move to another village that had already cellphone coverage were very low. 

As to the preliminary RD estimation we perform with RD robust, we use 2 running variables: 
1) _temp, which is the "adjusted" measure of _dist
2) _dist, our proxied, and noisy, measure of distance

Looking at the results of RD estimation with the variable temp, we obtain qualitatively negative coefficients, significant at 10% level only for the outcome variable vote_comb (the share of votes under category C fraud). This result is in line with what found by Gonzales (2021) in table 2. On the other hand, using our noisy _dist variable, the coefficients are qualitatively positive (and enormous), as well as non-significant. This is due to the fact that RDrobust is implementing the first stage using _dist to construct the Instrumental variable used to instrument cov, which produces a very low and non-significant first stage with the treatment variable cov by design. 
*/

*(b)
* The case in which we would need to modify our RD design would be the one in which there are spillovers and spatial displacement of fraud (we could not label coverage boundary as "sharp" in this case) that lead to a downward bias of estimates presented in Gonzalez's Table 2. In the "Mobile Coverage Spillovers and Spatial Displacement of Fraud" part of the Additional Results section, Gonzalez introduces bands of 2km, 4km, 6km and 8km on the non-coverage side to estimate the likelihood of fraud in these non-overlapping regions. He found no statistically significant spikes nor drops in fraud measures in comparison to areas just outside the band, meaning that the coverage boundary "remains" sharp. For these reasons, we should not worry about our proxy leading to a bias in the estimates, provided that the absolute difference between true longitude and observed proxy is less than the length of the bands. 

*(c)

*We first compute the Optimal Bandwidth based on the variable _temp

foreach var in /*600 95 ecc*/ comb comb_ind {
		rdbwselect vote_`var' _temp if ind_seg50==1, vce(cluster segment50) fuzzy(cov) //Add the option fuzzy because we are performing a fuzzy RDD
		scalar hopt_`var'=e(h_mserd)
		forvalues r=1/2 {
			rdbwselect vote_`var' _temp if ind_seg50==1 & region2==`r', vce(cluster segment50) fuzzy(cov)
			scalar hopt_`var'_`r'=e(h_mserd)
	}
}


* Control means
*In the optimal bandwidth
foreach var in /*600 95 ecc*/ comb comb_ind {
		sum vote_`var' if (cov==0 & ind_seg50==1 & _dist<=hopt_`var')
		scalar mean_`var'=r(mean)
		forvalues r=1/2 {
			sum vote_`var' if (cov==0 & ind_seg50==1 & _dist<=hopt_`var'_`r' & region2==`r')
			scalar mean_`var'_`r'=r(mean)
	}
}
*All observations
foreach var in /*600 95 ecc*/ comb comb_ind {
		sum vote_`var' if (cov==0 & ind_seg50==1)
		scalar mean_`var'_all=r(mean)
		forvalues r=1/2 {
			sum vote_`var' if (cov==0 & ind_seg50==1 & region2==`r')
			scalar mean_`var'_`r'_all=r(mean)
	}
}

xtset, clear
xtset segment50 pccode


*********************************************************************
*  					LOCAL LINEAR REGRESSIONS						*
*********************************************************************

* In order to replicate the estimation performed by Gonzales (2021), but in the context of a Fuzzy Regression Discontinuity Design, we need to perform an IV estimation of the specification used by Gonzales in eq. (1) of his paper, where the running variable _dist is used as an instrument for the treatment variable coverage, cov. We perform this estimation with the command xtvireg, which allows us to perform 2SLS estimation for panel data models, allowing us to include segment fixed effect. 

* To perform IV estimation of eq. (1) of Gonzales 2021, we generate a dummy variable z_i = 1(_temp >= 0), i.e., and indicator function/dummy variable that takes value of 1 if our "adjusted" running variable _temp is greater or equal than the cutoff, and 0 otherwise. Moreover, we create the interaction term between our running variable _temp and the treatment variable, and the instrument for it. 

*****************************************************************
*		USING VARIABLE _temp AS RUNNING VARIABLE				*	
*****************************************************************
* Generate the dummy to instrument the treatment variable cov
gen z_ = 0
replace z_ = 1 if _temp >= 0

* Generate interaction term between running variable and treatment variable
gen _tc_ = _temp*cov

* Generate the interaction term between the running variable and the instrument of cov, to instrument the interaction
gen _tz_ = _temp*z

* The need to create the interaction term and and the instrument for it, arises from the fact that the specification that Gonzales performes with xtreg: xtreg vote_`var' cov##c.(dist) if ind_seg50==1 & dist<=hopt_`var', fe robust, is incompatibile with xtivreg. 


* We use the newly created variable to perform IV estimations of the specification of eq. (1)
foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' _temp (cov _tc_ = z_ _tz_)  if ind_seg50==1 & _temp<=hopt_`var', fe vce(robust) first
		est store col1_a_`var'
		label variable _est_col1_a_`var' "All regions"
		estadd scalar Obs = e(N)
		estadd scalar Mean = mean_`var'_all
		estadd scalar Gr = e(N_clust)
		
		
	* Southeast
	xtivreg vote_`var' _temp (cov _tc_ = z_ _tz_)  if ind_seg50==1 & _temp<=hopt_`var'_1 & ///
	region2==1, fe vce(robust) first
		est store col1_b_`var'
		label variable _est_col1_b_`var' "Southeast"
		estadd scalar Obs = e(N)
		estadd scalar Mean = mean_`var'_1_all
		estadd scalar Gr = e(N_clust)

	* Northwest
	xtivreg vote_`var' _temp (cov _tc_ = z_ _tz_)  if ind_seg50==1 & _temp<=hopt_`var'_2 & ///
	region2==2, fe vce(robust) first
		est store col1_c_`var'
		label variable _est_col1_c_`var' "Northwest"
		estadd scalar Obs = e(N)
		estadd scalar Mean = mean_`var'_2_all
		estadd scalar Gr = e(N_clust)
}

*Using _temp as the running variable, both to construct the instrumental dummy variable and in the specification, we obtain similar results as Gonzales 2021. Indeed, as in the original sharp design, the coefficient are statistically and economically significant only when analysing All Regions together and Southeast regions, as well as negative. The positive coefficient as well as the lack of significance for the coefficients in the third column is justified by the author with the fact that in the Northwestern regions of the country the levels of fraud were much lower relative to the Southeastern ragions. 

*Our coefficients are higher than the one found by Gonzales 2021. This can be explained by the fact that fuzzy RD design estimates the LATE at the cutoff level. In this case, the cutoff indicates the areas which have 0 distance from the tower, and thus our coefficients estimate the treatment effect in those areas which are very likely to have a very high mobile phone signal. In terms of interpretation of the coefficients, for All Regions we can see a drop in the outcome variable vote_comb_ind (At least one Station with category C fraud) of about 10 percentage points. For the Southeast region, the drop in the same outcome for polling centres inside the coverage area within the optimal bandwidth is instead of 27 percentage points. Looking at the outcome variable of Panel B of Table 2, "Share of votes under Category C fraud", the drops in All Region analysis and in Southeast region are of 10 percentage points and 21 percentage points, respectively. 

*Below, we also propose an alternative IV estimation of eq. (1) inputting _dist in the specification of the regression, but generating the instrumental variable from _temp. Running the code below, we can see that, apart from the case of the "All Regions" sample, the coefficients are always qualitatively different from the ones obtained by Gonzales 2021. Moreover, the p-values are very high, showing lack of significance of the point estimates proposed. We posit that, despite having created the instrument starting from the variable _temp, which represents a good proxy/instrument for the treatment variable cov (we can see it looking at the First Stage results of xtivreg) the odd results that we obtained might be due to the excessive noise in the proxied _dist variable, which is composed of 300 negative observations. In particular, if the "wrongly" negative observations of _dist belong specifically to observation very close to the cutoff, where the coverage is high, this could bias upward our LATE estimate. 


**************************************************************************************************
*		ALTERNATIVE: USING VARIABLE _dist AS RUNNING VARIABLE, instrumenting cov through _temp	 *
**************************************************************************************************
* Generate the dummy to instrument the treatment variable cov
gen z = 0
replace z = 1 if _temp >= 0

* Generate interaction term between running variable and treatment variable
gen _tc = _dist*cov

* Generate the interaction term between the running variable and the instrument of cov, to instrument the interaction
gen _tz = _dist*z

* The need to create the interaction term and and the instrument for it, arises from the fact that the specification that Gonzales performes with xtreg: xtreg vote_`var' cov##c.(dist) if ind_seg50==1 & dist<=hopt_`var', fe robust, is incompatibile with xtivreg. 


* We use the newly created variable to perform IV estimations of the specification of eq. (1)
foreach var in comb_ind comb {	
	* All regions
	xtivreg vote_`var' _dist (cov _tc = z _tz)  if (ind_seg50==1 & _dist<=hopt_`var'), fe vce(robust) first
		est store col1_a_`var'_a
		label variable _est_col1_a_`var'_a "All regions"
		estadd scalar Obs = e(N)
		estadd scalar Mean = mean_`var'_all
		estadd scalar Gr = e(N_clust)
		
		
	* Southeast
	xtivreg vote_`var' _dist (cov _tc = z _tz)  if ind_seg50==1 & _dist<=hopt_`var'_1 & ///
	region2==1, fe vce(robust) first
		est store col1_b_`var'_a
		label variable _est_col1_b_`var'_a "Southeast"
		estadd scalar Obs = e(N)
		estadd scalar Mean = mean_`var'_1_all
		estadd scalar Gr = e(N_clust)

	* Northwest
	xtivreg vote_`var' _dist (cov _tc = z _tz)  if ind_seg50==1 & _dist<=hopt_`var'_2 & ///
	region2==2, fe vce(robust) first
		est store col1_c_`var'_a
		label variable _est_col1_c_`var'_a "Northwest"
		estadd scalar Obs = e(N)
		estadd scalar Mean = mean_`var'_2_all
		estadd scalar Gr = e(N_clust)
 }


*STORING THE RESULTS OF THE FIRST REGRESSION

* Using esttab to output the TeX code of two tables, to be then combined in a unique TeX table with the code by Steve Of Connel retrieved from github (https://github.com/steveofconnell/PanelCombine/blob/master/ExampleUse.do): 

esttab col1_a_comb_* col1_b_comb_* col1_c_comb_*  ///
using "results_onedim_a.tex", replace style(tex) ///
star(* 0.10 ** 0.05 *** 0.01) ///
keep(cov) mtitles("\makecell{All Regions\\ (1)}"  "\makecell{Southeast\\(3)}" "\makecell{Northwest\\(5)}") label title("Table 2 - EFFECT OF CELLPHONE COVERAGE ON CATEGORY C FRAUD") eqlabels(none) nonotes

esttab col1_a_comb  col1_b_comb  col1_c_comb  ///
using "results_onedim_b.tex", replace style(tex) ///
cells(b(star fmt(3)) se(par fmt(3))) star(* 0.10 ** 0.05 *** 0.01) ///
keep(cov) mtitles("\makecell{All Regions\\ (1)}"  "\makecell{Southeast\\(3)}" "\makecell{Northwest\\(5)}") label eqlabels(none) nonotes

include "https://raw.githubusercontent.com/steveofconnell/PanelCombine/master/PanelCombineSutex.do"
panelcombinesutex, use(results_onedim_a.tex results_onedim_b.tex)  columncount(3) paneltitles("At least one station with Category C fraud" "Share of votes under Category C fraud") save(combined_table.tex) addcustomnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip \textbf{Note:} Table shows summary statistics for cars in different estimation samples.\end{minipage}" )


**STORING THE RESULTS OF THE SECOND REGRESSION  //TO BE ELIMINATED - USE IT TO LOOK AT THE TEX TABLE IN THE FOLDER

esttab col1_a_comb_*_a col1_b_comb_*_a col1_c_comb_*_a  ///
using "results_onedim_a_a.tex", replace style(tex) ///
star(* 0.10 ** 0.05 *** 0.01) ///
keep(cov) mtitles("\makecell{All Regions\\ (1)}"  "\makecell{Southeast\\(3)}" "\makecell{Northwest\\(5)}") label title("Table 2 - EFFECT OF CELLPHONE COVERAGE ON CATEGORY C FRAUD") eqlabels(none) nonotes

esttab col1_a_comb_a  col1_b_comb_a  col1_c_comb_a  ///
using "results_onedim_b_a.tex", replace style(tex) ///
cells(b(star fmt(3)) se(par fmt(3))) star(* 0.10 ** 0.05 *** 0.01) ///
keep(cov) mtitles("\makecell{All Regions\\ (1)}"  "\makecell{Southeast\\(3)}" "\makecell{Northwest\\(5)}") label eqlabels(none) nonotes

include "https://raw.githubusercontent.com/steveofconnell/PanelCombine/master/PanelCombineSutex.do"
panelcombinesutex, use(results_onedim_a_a.tex results_onedim_b_a.tex)  columncount(3) paneltitles("At least one station with Category C fraud" "Share of votes under Category C fraud") save(combined_table_a.tex) addcustomnotes("\begin{minipage}{`linewidth'\linewidth} \footnotesize \smallskip \textbf{Note:} Table shows summary statistics for cars in different estimation samples.\end{minipage}" )



//KEEP THIS, THIS GOES BELOW THE ESTTAB RESULTS OF THE FIRST REGRESSION
* Alternatively, we use the estout command as done by Gonzales in his replication package, to obtaine the two separated tables: 
estout col1_a_comb_* col1_b_comb_* col1_c_comb_*  ///
using "results_onedim_a_estout.tex", replace style(tex) ///
cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
keep(1.cov) mlabels() label title("Panel A - At least one station with Category C fraud") eqlabels(, none)

estout col1_a_comb  col1_b_comb  col1_c_comb  ///
using "results_onedim_b_estou.tex", replace style(tex) ///
label cells(b(star fmt(3)) se(par fmt(3))) starlevels(* 0.10 ** 0.05 *** 0.01) ///
keep(1.cov) mlabels() legend title("Panel B - Share of votes under Category C fraud") eqlabels(, none)




