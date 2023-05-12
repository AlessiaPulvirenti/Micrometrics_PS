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
**************************************************************************************************************************************					
clear all

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

* Overall, looking at the three graphs, it seems that pre-treatment are parallel in all three groups of villages. A small exception is represented by villages in the second group, who are being treated in 1996. In that category, we see that treated units show an increase in the probability of having a health centre in the village before the treatment, with respect to the control group. 

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

* We recover the DiD estimator as [(P1T1 - P0T1) - (P1T0 - P0T0)] =  0.0122. Hence, having at least one school constructed in the village increases the probability of having a health center by approximately 1.22 percentage points


*(d)
*i - POOLED OLS
reg HEALTH_CENTER_VL POST_TREATED POST TREATED, vce(cluster idkab_num)
outreg2 using Table_2.xls, excel replace lab title("Table 2") ctitle("Pooled OLS") br bdec(4) addtext(Year FEs, NO, Village FEs, NO, Cluster, YES)

** Looking at the coefficient of the interaction, we can see that the DD estiamte is .0121682 exactly what we found in the previous point.


*ii - FE with xtreg
xtreg HEALTH_CENTER_VL POST_TREATED POST i.year, fe i(v_id) vce(cluster idkab_num)
outreg2 using Table_2.xls, excel append lab drop(i.year_rk) title("Table 2") ctitle("xtreg") br bdec(4) addtext(Year FEs, YES, Village FEs, YES, Cluster, YES)

** We cannot include the TREATED dummy since it is a village specific time constant variable already captured by the fixed effects. STATA would simply omit it due to multicollinearity problems. We can see that the the two way estimate is slightly larger compared to the OLS one (.0167212).

*iii - FE with xtreg
areg HEALTH_CENTER_VL POST_TREATED POST i.year, absorb(v_id) vce(cluster idkab_num)
outreg2 using Table_2.xls, excel append lab drop(i.year_rk) title("Table 2") ctitle("areg") br bdec(4) addtext(Year FEs, YES, Village  FEs, YES, Cluster, YES)


*(e)
gen INTENSITY = num_PSINPRES1980
gen POST_INTENSITY = INTENSITY*POST

xtreg HEALTH_CENTER_VL POST POST_INTENSITY i.year, fe i(v_id) vce(cluster idkab_num)

*This specification allows us to estimate the differential treatment intensity (depending on the number of INPRES schools constructed in the village), rather than estimating simply the treatment effect of having at least 1 INPRES school being constructed with the village (measured) with a dummy variable. That is, the coefficient on thw POST_INTENSITY variable represents the MARGINAL effect of constructing an extra school. Moreover, despite controlling for the POST dummy, since out Treatment variable in this case is not a binary variable, we are not estimating a saturated model, i.e., a model which has as many coefficients as the potential outcomes that the dependent variable can take. On the other hand, this is still a TWFE model, with village and year FE. Finally, the marginal effect of constructing an extra school according to the estimated model is 1.2 p.p. 

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

*When controlling also for the laggend dependent variable, we might introduce the so-called Nickel bias (Nickel, 1981). This can be seen for example, when considering a FE model in first difference (Angrist & Pischke 2009). In this case, we can notice that the residual is correlated with the lag of the oucome, introduced as an independent variable. The OLS trasformation in first difference is thus biased. This problem is further exhacerbated if the error terms are serially correlated across time (very likely in several applications). In this case, we might not be able to retrieve a consistent estimator of the treatment effect. 
 

*(h) 
* We included each of the unbalanced controls interacted with year dummies. 

foreach variable in dum_otrocop_pre num_bank_pre pedati_pre{
	foreach year in 1990 1993 1996 2000 2003{
		gen `variable'_`year' = 1*`variable' if year == `year'
		replace `variable'_`year' = 0 if missing(`variable'_`year')
}
}
xtreg HEALTH_CENTER_VL POST POST_INTENSITY dum_otrocop_pre_* num_bank_pre_* pedati_pre_* i.year, fe i(v_id) vce(cluster idkab_num)

*Most of the unbalanced baseline controls included have statistically insignificant effects, except for the few of them that are significant at 5% and have a positive effect. Concerning the variables previously included in (e), we see that their estimates are all more or less the same, therefore we can claim that our results remain robust despite the inclusion of unbalanced baseline controls. 

*Adding the lagged outcomes in (g), we notice that the variable controlling for year 1993 becomes insignificant, and that the more importance is assigned to the lagged variable, being the most significant one than to the rest of the variables in the model, whose effects have decreased. This is likley due to the Nickel Bias (Nickel, 1981), explained in question (g): the bias is due to the correlation between the error term and the lagged dependent outcome. Moreover, if the the error terms are serially correlated across we might not be able to retrieve a consistent estimator of the treatment effect. So, we can claim that the results are not robust to the inclusion of the lagged outcome. 


* (i)
preserve

drop if year>1990


gen PLACEBO_POST = 1 if year == 1990
replace PLACEBO_POST = 0 if year < 1990
gen PLACEBO_POST_INTENSITY = PLACEBO_POST*INTENSITY

xtreg HEALTH_CENTER_VL PLACEBO_POST PLACEBO_POST*INTENSITY, fe i(v_id) cluster(idkab_num)

restore

*The coefficient of interest is the one on PLACEBO_POST_INTENSITY, which is positive but has an associated p-value of 0.489, therefore it is insignificant. We are satisfied by this result which sustains our design. Indeed, there appears to be no effect in the pre-treatment years. Thus, it is unlikely that the previosuly estimated causal effect of interest is caused by Omitted Variable Bias, which would have been picked also up in this placebo test and disguised as a significant causal effect. Notice that year fixed effect (the dummy for 1990) have been excluded from the regression because of multicollinearity with PLACEBO_POST. ***

*

*(j)
* In a staggered DD design, such as the one implemented in Martinez-Bravo (2017), a potential obstacle to identification of the ATE are heterogenous treatment effects. 
*The Bacon decomposition theorem states that the two-way fixed effect (TWFE) estimator is a weighted average of all possible two-by-two DD estimates. Weights depend on the size and group variance in treatment and sum up to one.  Weights can also be negative - as discussed by de Chaisemartin and D'haultfoeuille (2020) - and in such case TWFE estimators may be biased even if the parallel trend assumption holds. As stated previously, the TWFE estimator is equal to the expectation of a weighted sum of the ATEs in all (g-t) cells, which compare the evolution of the outcome between treated and control groups at each point in time t. Negative weights arise because it may occur that the control group in some of the two-by-two DD may be treated in both periods (i.e., cases in which the control group has already been treated at a previous point in time - early adopters). In such case, treatment effect are differenced out in the second period due to the DiD specification, leading to negative weights. 
*Negative weights when coupled with treatment effects heterogeneity across groups and times are an issue for the identification of the ATE. For example, if many ATEs in (g-t) cells are weighted by negative weights, the TWFE estimate will be biased towards zero or even take negative values, preventing its correct interpretation as the ATE. 
*Martinez-Bravo (2017) explore heterogeneity across villages depending on their baseline level of quality of public good service. The authors find that measures of the INPRES program intensity are significantly higher for villages where the underlying demand for the specific public good was higher (because of the lower provision prior to the program). 
*Martinez-Bravo (2017) find evidence of heterogenous treatment effects, hence for the reasons states previously we cannot state a priori whether the TWFE estimates can be interpreted as average treatment effect. To do so, we must carry out futher tests to check the robustness of the estimates to heterogenous effects. 
  


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
*In our code above we have computed the weights and the standard deviation of the weights, performing the same procedure as Clément de Chaisemartin and Xavier D'Haultfoeuille suggest, both through the command twowayfeweights and manually. The command usually reports two summary measures of robustness to heterogeneity of treatment effect: the The first one is defined in point (i) of Corollary 1 in de Chaisemartin & D'Haultfoeuille (2020a).  It corresponds to the minimal value of the standard deviation of the treatment effect across the treated groups and time periods under which beta and the average treatment effect on the treated (ATT) could be of opposite signs. When that number is large, this means that beta and the ATT can only be of opposite signs if there is a lot of treatment effect heterogeneity across groups and time periods. When that number close to 0, this means that beta and the ATT can be of opposite signs even if there is not a lot of treatment effect heterogeneity across groups and time periods.  The second summary measure is defined in point (ii) of Corollary 1 in de Chaisemartin & D'Haultfoeuille (2020a).  It corresponds to the minimal value of the standard deviation of the treatment effect across the treated groups and time periods under which beta could be of a different sign than the treatment effect in all the treated group and time periods.

*However, since in our case the output of the command shows that, out of  15583 ATTs, all receive positive weights and non of them receive negative weight, we a fairly confident that the estimates obtained are robust to Treatment Effect Heterogeneity (de Chaisemartin & D'Haultfoeuille (2020a), page 2974), and so that the actual ATT and the the beta obrained from the TWFE estimation are not of different signs. Moreover, looking at the value of the standard error we observe as a result of both the command computation and the manual computation, 0.06796259, we can say that even if we had found some negative weights, the standard error magnitude is this large enough for us not to doubt that the estimated coefficient had flipped sign with respect to the actual "ATT".

*Finally, we note that the TWFE estimator computes an ATT, rather than an ATE. However, since Martinez-Bravo (2017) shows convincingly that village elections and their interaction with school construction intensity are quasi-random, we can assume that the ATT found in our case might not be significantly different than the ATE. 






