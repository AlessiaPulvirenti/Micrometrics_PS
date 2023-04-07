					*******************************************
					***										***
					***		  Problem Set 2					***
					***										***
					***			GROUP 8						***	
					***										***
					***		Aleksa Mitrovic		3100079		***
					***		Elena Neri			3070190		***
					***		Alessia Pulvirenti	3060894		***
					***		Tommaso Roccuzzo	3080613		***
					***										***
					*******************************************
											

clear all

ssc install ivreg2, replace


**# EXERCISE 1	

/*
Question 1 and Question 2 here
*/


* QUESTION 3


cd "C:\Users\elena\OneDrive\Desktop\ESS\2nd year\Microeconometrics\PS\problem set 2\exercise_2"
/* (a) Estimate an OLS regression of Healthy on Education using Controls and Birth Year
FEs as controls, and export it to an excel table named TABLE Q_3. 
Only show the coefficient of Education and the usual regression statistics.
*/
use pset_2_q_2_and_3.dta, clear

tab region, gen(region)
tab birthyear, gen(birthyear_FE)

local Controls "region* Central Married"
local Birth_Year_FEs "birthyear_FE*"

reg Healthy Education `Controls' `Birth_Year_FEs'  
outreg2 using TABLE_Q_3.xls, excel keep (Education) nocons addtext(Controls, YES, Year of Birth FEs, YES, Regression, OLS) title("OLS and IV") replace


/*(b) Estimate a first stage for an IV regression of Healthy on Education using Quarter1
Quarter2 Quarter3 as instruments for Education, and Controls and Birth Year FEs
as controls, and append such a first stage regression to TABLE Q 3.
Only show the coefficient of Quarter1 Quarter2 Quarter3 and the usual regression
statistics.
Add a line with the F-statistic of the excluded instruments, naming it F-statistic IVs.*/ 

tab birthqtr, gen(Quarter)

ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust first savefirst
scalar Fstat = e(widstat)
est restore _ivreg2_Education
outreg2 using TABLE_Q_3.xls, excel keep (Quarter1 Quarter2 Quarter3) addtext(Controls, YES, Year of Birth FEs, YES, Regression, First Stage) title("OLS and IV") addstat("F-statistic IVs", Fstat) nocons append

/*

(c) Estimate a reduced form regression of Healthy on instruments Quarter1 Quarter2
Quarter3 and Controls and Birth Year FEs as controls.
Append such a reduced form regression to TABLE_Q_3.
Based on the results of question 2.(e), what are the expected signs of the coefficients
of Quarter1 Quarter2 Quarter3?
Are these reduced form coefficients in line with your expectations?
*/

reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs', robust 
outreg2 using TABLE_Q_3.xls, excel keep (Quarter1 Quarter2 Quarter3) addtext(Controls, YES, Year of Birth FEs, YES, Regression, Reduced Form) nocons append

/*
Write comments here based on Q2 results

*/

/* (d)
Estimate a second stage for an IV regression of Healthy on Education using Quarter1
Quarter2 Quarter3 as instruments for Education, and Controls and Birth Year FEs
as controls.
Append such a second stage regression to TABLE_Q_3.
Only show the coefficient of Education and the usual regression statistics.
*/

ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust
outreg2 using TABLE_Q_3.xls, excel keep (Education) nocons addtext(Controls, YES, Year of Birth FEs, YES, Regression, Second Stage) title("OLS and IV") append


/*(e)
Bound et al. (1995) discuss a number of issues that arise when confronted with both weak instruments and finite sample bias, in particular, when there exists a weak correlation between the instrument and the outcome variable. Discuss how these issues can generate a bias in the IV regression you have estimated in item (d).
Can you reject the the null hypothesis of the test of joint significance of the instruments?
Based on the size the F-statistic, can you say if finite sample bias is likely or not to be an issue in this case?
*/

reg Education Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs'
scalar R_sqrd = e(r2)
display R_sqrd

ssc install pcorr2, replace
pcorr2 Education Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs' //The squared partial correlation of the ...


*ADD A FEW LINES ABOUT THE NULL HYPOTHESIS OF JOINT SIGNIFICANCE 

/*
Y = /beta X + /epsilon		(1) - original model to be estimated - X is endogenous
X = /pie Z + /nu 			(2)	- First stage

Bound et al. (1995) analyse two cases under which the IV approach can lead to unconsistent estimates, with inconsistency that could be larger than that of the OLS estimator using and endogenous variable. 
One case is that of a weak instrument: when the instrument Z, used to get around the problem of endogeneity of the treatment variable X, is weakly correlated with the endogenous variable in question, this is likely to generate a large inconsistency in the IV estimates. In the case of an IV approach with no covariates, the authors prove this by showing that the ratio of the inconsistency of the IV estimator to the inconsistency of the OLS estimator is an expression that has at the denominator the correlation between X and Z, and at the numerator, the correlation between Z and the error term, and X and the error term. Therefore, if the instrument is only slightly endogenous (i.e., if the instrument Z has a very low correlation with the unboservable characteristics that affect Y in eq. (1)), this bias can be greatly amplified by low correlation between Z and X. In an IV setting where the first stage (and thus the second stage) include some covariates as in this case, Bound et al. prove that the ratio of the inconsistency of IV to inconsistency of OLS id negatively associated to the partial R-squared of the first stage, that is, the R-squared from the regression of x on z once the common exogenous variable have been partialled out from both X and Z.

In our case, since the partialled R-Squared for all three instruments is very low, if the quarter of birth proves to be even slightly endogenous to the unobserved characteristics that might affect health status, then the coefficient computed in the second stage could be biased. For example, if the quarter of birth is endogenous to the socio-economic status of individuals in the sample, then the IV coefficient could be upward biased. This could be a plausible explanation since our IV estimate is higher than the OLS estimate. Angrist and Krueger (1991, 1992 - a subsequent paper) provide many evidence to show that compulsory attendance laws are working to induce a correlation in educational attainment. However, if compulsory schooling law are not the ONLY channel through which quarter of birth and educational attainment are correlated (exclusion restriction), and quarter of birth is correlated in some way with health status, then our IV estimate can be strongly inconsistent, even more than than the OLS. Bound et al. 1995 provide some evidence of the correlation between birth seasonality and physical and mental health of individuals, listing some papers that prove that individuals born early in the years are more likely to be affected by schizofrenia, mental retardation, autism etc. 

The other problem discussed by Bound and coauthors when dealing with IVs is related to finite-sample bias. Assuming that the instrument Z is completely exogenous, the IV is a consistent estimator of /beta, but in finite samples, it is biased in the same direction of the OLS estimator. The magnitude of the bias depends negatively on the sample size, and negatively on the multiple correlation between the instruments and the endogenous explanatory variables.

Bound et al. (1995) suggest that to assess finite-sample bias, it is useful to examine the F-statistic from the First Stage. They claim that the bias of the instrumental variable (IV) relative to ordinary least squares (OLS) is inversely related to the F-statistic of the excluded instruments from the first stage. In our analysis, we obtained an F-statistic of 62. According to Bound et al. (1995), a close-to-1 F-statistic raises concerns about finite-sample bias. However, since our F-statistic is large, we can confidently state that finite-sample bias is not an issue in this case. Additionally, Staiger and Stock (1994) suggest that the bias of OLS relative to IV could be approximated by 1/F, where F is the F-statistic. Therefore, if the F-statistic is large, as in our case, the bias is likely to be very small. 
*/

/*
(f) Create a local named State FEs with dummies for each state of birth, except Wyoming,
which is intended to be the reference category.
*/

tab bpl, gen(State)
local State_FEs "State1-State50"

tab birthdate, gen(Year_quarter)
local Year_Quarter_FEs "Year_quarter1-Year_quarter39"

egen State_quarter = concat(bpl birthqtr), decode punct(-)
tab State_quarter, gen(State_quarter)
local State_Quarter_FEs "State_quarter1-State_quarter203"


/*(g)
Estimate an IV regression of Healthy on Education using Year Quarter FEs as instruments
for Education, Controls and Birth Year FEs as controls.
Estimate an IV regression of Healthy on Education using State Quarter FEs as instruments
for Education, and Controls, Birth Year FEs, and State FEs as controls.
Use Wyoming as omitted category for state of birth and include Washington DC.
*/
local Year_Quarter_FEs "Year_quarter1-Year_quarter39"
local Controls "Central Married region*"
local Birth_Year_FEs "birthyear_FE*"
ivreg2 Healthy (Education = `Year_Quarter_FEs') ///
`Controls' `Birth_Year_FEs', robust partial(`Birth_Year_FEs')

local State_Quarter_FEs "State_quarter1-State_quarter203"
local Controls "Central Married region*"
local Birth_Year_FEs "birthyear_FE*"
local State_FEs "State1-State50"
ivreg2 Healthy (Education = `State_Quarter_FEs') ///
`Controls' `Birth_Year_FEs' `State_FEs', robust partial(`Birth_Year_FEs' `State_FEs')

/*(h)
Compute the F-statistic for the excluded instruments in point (g).
Can you say if both regressions are likely to suffer from finite sample bias?*/








**# EXERCISE 2

* QUESTION 1 


*(a) Assume that import competition affects the rate of manufacturing employment homogeneously. State which identification assumptions must hold under this setting for those IV estimates presented in Autor et al. (2013) to be consistent.

/* Autor et al. 2013 investigate the effects of increased import competition from China in the manufacturing sector on wages and employment in the US. However, the change in import competition from Chinese products (what can be called, the "China shock") in a specific industry could be correlated with import demand shocks in that industry, leading the OLS estimate to be biased if both US employment (the outcome variable) and imports may be positively correlated with unobserved demand shocks. Then, in order to identify the causal effect of rising Chinese import exposure on local labour market outcomes, they employ and IV approach that instruments US imports from China with other countries' imports from China, weighted by the portion of employment in that industry to the employment in all industries (the "share"). Moreover, to avoid reverse causality/contemporaneity bias, the auhors use the share of employment in each industry for every location measured in the previous 10 years. Given this setting, the main identification assumptions for the instrument to be valid are: 
1) Relevance: changes in Chinese imports in other countries must be positively correlated with changes in imports from China in the US.
2) Randomness: increase in imports in other countries should be independent of unobservable characteristics of local labour markets under investigation in the US (e.g., they should be exogenous to demand shocks in that industry)
3) Exclusion restriction: changes in Chinese imports in other countries should affect local labour market outcomes in the US ONLY THROUGH the effect that the increase in Chinese imports in these countries has on increase in imports in US countries. This can be true if the the common within-industry component of rising Chinese imports to the US and other high-income countries stems only from China's rising comparative advantage and/or falling trade costs.

Therefore, in the Bartik instrument jargoon, the identification assumptions set forth by Autor et al. (2013) are based on the exogeneity of the shocks. On the other hand, Goldsmith-Pinkham et al. (2020) base the consistency of the Bartik estimator (under the assumption of homogeneous effects) on the exogeneity of the shifts claiming that the Bartik instrument is equivalent to a GMM estimator composed of a matrix of weights (the Rotemberg weights) and as many instruments as the shares of employment in each location. Precisely because of this definition of the Bartik instrument,  Goldsmith-Pinkham et al. (2020) require the shares, rather than the shifts (shocks) to be strictly exogenous.
*/

*(b) Which additional assumptions would be necessary to hold if import competition would afffiect manufacturing employment difffierently, depending on a set of factors?

/*
Assuming the presence of heterogenous effects that vary across either location or time, Goldsmith-Pinkham et al. 2020 discuss the interpretation of heterogenous effect of the Bartik instrument. Because this instrument combines multiple unordered instruments, the authors provide a definition of restricted heterogeneity (a linear heterogeneity), as monotonicity is not sufficient. 
So, the model allows for heterogeneous effect on employment across location, with constant effect within location. Goldsmith-Pinkham et al. 2020 provide a new specification with a coefficient that varies for each location (constant linear effects within a location). Then, the authors set forth the following assumptions to allow for the identification of the restricted-heterogeneous effect:
1) Monotonicity-analogous assumption: 
(i) for every industry, the coefficients of each industry-location specific first stage are all (weakly) of the same sign across all locations, an assumption which is very similar to the "standard" monotonicity assumption. 
(ii) conditional on controls, the expectation of the product between the industry-location specific instrument, the error term of the FS and the location-specific treatment effect is equal to 0. 
*/

* QUESTION 2

set matsize 2000

/*** AKM ADH Data **/
insheet using "ADHdata_AKM.csv", clear
gen year = 1990 + (t2=="TRUE")*10
drop t2

/*** BHJ SHARES **/
merge 1:m czone year using "Lshares.dta", gen(merge_shares)
/*** BHJ SHOCKS **/
merge m:1 sic87dd year using "shocks.dta", gen(merge_shocks)

rename ind_share share_emp_ind_bhj_
gen z_ = share_emp_ind_bhj_ * g
rename g g_
drop g_emp_ind-g_importsUSA
reshape wide share_emp_ind_bhj_ g z_, i(czone year) j(sic87dd)
egen z = rowtotal(z_*)


local controls reg_* l_sh_popedu_c l_sh_popfborn l_sh_empl_f l_sh_routine33 l_task_outsource l_shind_manuf_cbp t2
local weight weight

local y d_sh_empl_mfg 
local x shock
local z z


local ind_stub share_emp_ind_bhj_
local growth_stub g_

local time_var year
local cluster_var czone

levelsof `time_var', local(years)

/** g_2141 and g_3761 = 0 for all years **/
drop g_2141 `ind_stub'2141
drop g_3761 `ind_stub'3761

forvalues t = 1990(10)2000 {
	foreach var of varlist `ind_stub'* {
		gen t`t'_`var' = (year == `t') * `var'
		}
	foreach var of varlist `growth_stub'* {
		gen t`t'_`var'b = `var' if year == `t'
		egen t`t'_`var' = max(t`t'_`var'b), by(czone)
		drop t`t'_`var'b
		}
	}

tab division, gen(reg_)
drop reg_1
tab year, gen(t)
drop t1

drop if czone == .

foreach var of varlist `ind_stub'* {
	if regexm("`var'", "`ind_stub'(.*)") {
		local ind = regexs(1) 
		}
	tempvar temp
	qui gen `temp' = `var' * `growth_stub'`ind'
	qui regress `x' `temp' `controls' [aweight=`weight'], cluster(czone)
	local pi_`ind' = _b[`temp']
	qui test `temp'
	local F_`ind' = r(F)
	qui regress `y' `temp' `controls' [aweight=`weight'], cluster(czone)
	local gamma_`ind' = _b[`temp']
	drop `temp'
	}

foreach var of varlist `ind_stub'3571 `ind_stub'3944 `ind_stub'3651 `ind_stub'3661 `ind_stub'3577 {
	if regexm("`var'", "`ind_stub'(.*)") {
		local ind = regexs(1) 
		}
	tempvar temp
	qui gen `temp' = `var' * `growth_stub'`ind'
	ch_weak, p(.05) beta_range(-10(.1)10)   y(`y') x(`x') z(`temp') weight(`weight') controls(`controls') cluster(czone)
	disp r(beta_min) ,  r(beta_max)
	local ci_min_`ind' =string( r(beta_min), "%9.2f")
	local ci_max_`ind' = string( r(beta_max), "%9.2f")
	disp "`ind', `beta_`ind'', `t_`ind'', [`ci_min_`ind'', `ci_max_`ind'']"
	drop `temp'
	}


preserve
keep `ind_stub'* czone year `weight'
reshape long `ind_stub', i(czone year) j(ind)
gen `ind_stub'pop = `ind_stub'*`weight'
collapse (sd) `ind_stub'sd = `ind_stub' (rawsum) `ind_stub'pop `weight' [aweight = `weight'], by(ind year)
tempfile tmp
save `tmp'
restore

bartik_weight, z(t*_`ind_stub'*)    weightstub(t*_`growth_stub'*) x(`x') y(`y') controls(`controls'  ) weight_var(`weight')

mat beta = r(beta)
mat alpha = r(alpha)
mat gamma = r(gam)
mat pi = r(pi)
mat G = r(G)
qui desc t*_`ind_stub'*, varlist
local varlist = r(varlist)



clear
svmat beta
svmat alpha
svmat gamma
svmat pi
svmat G

gen ind = ""
gen year = ""
local t = 1
foreach var in `varlist' {
	if regexm("`var'", "t(.*)_`ind_stub'(.*)") {
		qui replace year = regexs(1) if _n == `t'
		qui replace ind = regexs(2) if _n == `t'
		}
	local t = `t' + 1
	}

/** Calculate Panel C: Variation across years in alpha **/
total alpha1 if year == "1990"
mat b = e(b)
local sum_1990_alpha = string(b[1,1], "%9.3f")
total alpha1 if year == "2000"
mat b = e(b)
local sum_2000_alpha = string(b[1,1], "%9.3f")

sum alpha1 if year == "1990"
local mean_1990_alpha = string(r(mean), "%9.3f")
sum alpha1 if year == "2000"
local mean_2000_alpha = string(r(mean), "%9.3f")

destring ind, replace
destring year, replace
merge 1:1 ind year using `tmp'
gen beta2 = alpha1 * beta1
gen indshare2 = alpha1 * (`ind_stub'pop/`weight')
gen indshare_sd2 = alpha1 * `ind_stub'sd
gen G2 = alpha1 * G1
collapse (sum) alpha1 beta2 indshare2 indshare_sd2 G2 (mean) G1 , by(ind)
gen agg_beta = beta2 / alpha1
gen agg_indshare = indshare2 / alpha1
gen agg_indshare_sd = indshare_sd2 / alpha1
gen agg_g = G2 / alpha1
rename ind sic
merge 1:1 sic using "sic_code_desc.dta"
rename sic ind
keep if _merge == 3
gen ind_name = subinstr(description, "Not Elsewhere Classified", "NEC", .)
replace ind_name = subinstr(ind_name, ", Except Dolls and Bicycles", "", .)

gsort -alpha1

* (a) Plot the distribution of Rotemberg weights associated to Autor et al. (2013) in a parametric and a non-parametric manner (overlay both graphs in a single plot).
hist alpha1, normal xtitle("Rotemberg Weights")
graph export "FigureA1.png", replace

*(b) Compile a LATEX table with 2 panels: panel A, identical to panel E in table A1 of Goldsmith-Pinkham et al. (2020); panel B, inspired in panel D of table A1 - the rows of the table should include those industries belonging to the top 5 industries in terms of Rotemberg weights; the columns should include information about: (i) /alpha_k, (ii) gk, (iii) /beta_k, (iv) 95% CIs, (v) Ind. Share and (vi) Share of overall /beta.

gen omega = alpha1*agg_beta
total omega
mat b = e(b)
local b = b[1,1]

gen label_var = ind 
gen beta_lab = string(agg_beta, "%9.3f")


gen abs_alpha = abs(alpha1) 
gen positive_weight = alpha1 > 0
gen agg_beta_pos = agg_beta if positive_weight == 1
gen agg_beta_neg = agg_beta if positive_weight == 0

/** Panel A:  Weighted Betas by alpha weights **/
preserve
	gen agg_beta_weight = agg_beta * alpha1

	collapse (sum) agg_beta_weight alpha1 (mean)  agg_beta, by(positive_weight)
	egen total_agg_beta = total(agg_beta_weight)
	gen share = agg_beta_weight / total_agg_beta
	gsort -positive_weight
	local agg_beta_pos = string(agg_beta_weight[1], "%9.3f")
	local agg_beta_neg = string(agg_beta_weight[2], "%9.3f")
	local agg_beta_pos2 = string(agg_beta[1], "%9.3f")
	local agg_beta_neg2 = string(agg_beta[2], "%9.3f")
	local agg_beta_pos_share = string(share[1], "%9.3f")
	local agg_beta_neg_share = string(share[2], "%9.3f")
restore

/** Panel B:  Weighted Betas by alpha weights **/


gen agg_beta_weight = agg_beta * alpha1
egen total_agg_beta = total(agg_beta_weight)
gen beta_share = agg_beta_weight / total_agg_beta


foreach ind in 3571 3944 3651 3661 3577 {
	
	*Mean of the alpha for a given Industry
	qui sum alpha1 if ind == `ind'
    local alpha_`ind' = string(r(mean), "%9.3f")
	*Mean of g for a given Industry
	qui sum agg_g if ind == `ind'	
	local g_`ind' = string(r(mean), "%9.3f")
	*Mean of beta for a given Industry
	qui sum agg_beta if ind == `ind'	
	local beta_`ind' = string(r(mean), "%9.3f")
	*Industry Share
	qui sum agg_indshare if ind == `ind'
	local share_`ind' = string(r(mean)*100, "%9.3f")
	*Beta Share
	qui sum beta_share if ind == `ind'
	local beta_share_`ind' = string(r(mean)*100, "%9.3f")
	* Save the name of the Industry
	tempvar temp
	qui gen `temp' = ind == `ind'
	gsort -`temp'
	local ind_name_`ind' = ind_name[1]
	drop `temp'
	}

/*** Create final table **/

capture file close fh
file open fh  using "ex_2_table.tex", write replace
file write fh "\begin{table}[]" _n
file write fh "\centeringe" _n

file write fh "\begin{tabular}{lllllll}" _n

/** Panel A **/
file write fh "\multicolumn{5}{l}{\textbf{Panel A: Estimates of $\beta_{k}$ for positive and negative weights} }\\" _n
file write fh  " &  &  &  & \multicolumn{1}{c}{$\alpha$-weighted Sum} & \multicolumn{1}{c}{Share of overall $\beta$} & \multicolumn{1}{c}{Mean} \\ \cline{5-7} " _n
file write fh  "Negative &  &  &  & `agg_beta_neg' & `agg_beta_neg_share'  & `agg_beta_neg2' \\" _n
file write fh  "Positive &  &  &  & `agg_beta_pos' & `agg_beta_pos_share' & `agg_beta_pos2' \\" _n



/** Panel B **/
file write fh "\multicolumn{7}{l}{\textbf{Panel B: Top 5 Rotemberg weight industries}} \\" _n
file write fh  " & \multicolumn{1}{c}{$\hat{\alpha}_{k}$} & \multicolumn{1}{c}{$ g_{k}$} & \multicolumn{1}{c}{$\hat{\beta}_{k}$} & \multicolumn{1}{c}{95 \% CI} & \multicolumn{1}{c}{Ind Share} & \multicolumn{1}{c}{Share of overall $\beta$ \%} \\ \cline{2-7} " _n
foreach ind in 3571 3944 3651 3661 3577 {
	if `ci_min_`ind'' != -10 & `ci_max_`ind'' != 10 {
		file write fh  "`ind_name_`ind'' & `alpha_`ind'' & `g_`ind'' & `beta_`ind'' & (`ci_min_`ind'',`ci_max_`ind'')  & `share_`ind'' & `beta_share_`ind''\\ " _n
		}
	else  {
		file write fh  "`ind_name_`ind'' & `alpha_`ind'' & `g_`ind'' & `beta_`ind'' & \multicolumn{1}{c}{N/A}  & `share_`ind'' & `beta_share_`ind'' \\ " _n
		}
	}
	
	
	
	
file write fh  "\end{tabular}" _n
file write fh  "\end{table}" _n
file close fh



*(c) Replicate Figures A2 and A3 from Section A of Goldsmith-Pinkham et al. (2020). Having both these figures into account, which type of TE heterogeneity seems to be present in Autor et al. (2013)? Does it preclude you from interpreting the IV estimates present in the paper as a LATE?

gen F = .
gen agg_pi = .
gen agg_gamma = .
levelsof ind, local(industries)
foreach ind in `industries' {
	capture replace F = `F_`ind'' if ind == `ind'
	capture replace agg_pi = `pi_`ind'' if ind == `ind'
	capture replace agg_gamma = `gamma_`ind'' if ind == `ind'		
	}

twoway (scatter agg_beta_pos agg_beta_neg F if F >= 5 [aweight=abs_alpha ], msymbol(Oh Dh) ), legend(label(1 "Positive Weights") label( 2 "Negative Weights")) yline(`b', lcolor(black) lpattern(dash)) xtitle("First stage F-statistic")  ytitle("{&beta}{subscript:k} estimate")
graph export "FigureA2.png", replace

gsort -alpha1
twoway (scatter F alpha1 if _n <= 5, mcolor(dblue) mlabel(ind_name  ) msize(0.5) mlabsize(2) ) (scatter F alpha1 if _n > 5, mcolor(dblue) msize(0.5) ), name(a, replace) xtitle("Rotemberg Weight") ytitle("First stage F-statistic") yline(10, lcolor(black) lpattern(dash)) legend(off)
graph export "FigureA3.png", replace


/*
WRITE COMMENT HERE
*/


* QUESTION 3 
*(a) State which are the identifying assumptions necessary for those IV estimates presented in Section V to be consistent. Discuss whether these assumptions are more or less plausible in this setting (relative to Autor et al., 2013).








