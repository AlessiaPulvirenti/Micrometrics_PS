											***************************
											***						***
											***		Problem Set 1	***
											***		GROUP ...		***
											***						***
											***************************
											
											
cd "/Users/tommasoroccuzzo/Library/Mobile Documents/com~apple~CloudDocs/Tommaso/Appunti/Bocconi/2021-2022/2° Semestre/Microeconometrics/Problem Sets/PS 1"

clear


************************************************	QUESTION 1 		************************************************

quietly{
	
use jtrain2.dta

***********    a    ***********

matrix balance = (.,.,.,.,.,.,.,.)

local i = 1

foreach var of varlist age educ black hisp nodegree re74 re75 {
	
	quietly sum `var' if train == 1
	
	matrix balance[`i',1]=r(mean)
	matrix balance[`i',3]=r(sd)

	
	quietly sum `var' if train == 0
	
	matrix balance[`i',2]=r(mean)
	matrix balance[`i',4]=r(sd)
	
	
	quietly regress `var' train, robust
	matrix rtable = r(table)

	matrix balance[`i',5] = rtable[1,1]
	matrix balance[`i',6] = rtable[2,1]
	matrix balance[`i',7] = rtable[3,1]
	matrix balance[`i',8] = rtable[4,1]
	
	local i = `i'+1
	
	if `i'<8  matrix balance=(balance \ .,.,.,.,.,.,.,.)
	
}

matrix rownames balance = age educ black hisp nodegree re74 re75
matrix colnames balance = mean_T mean_C StdDev_T StdDev_C Diff StdE_Diff t-stat p-value


matrix list balance

putexcel set TABLE_1.xls, replace

putexcel A1=matrix(balance), hcenter names


***********    b    ***********


reg re78 train, robust

scalar beta = _b[train]

scalar list beta


***********    c    ***********


qui reg re78 train, robust
estimates store reg1

qui reg re78 train age educ black hisp, robust
estimates store reg2

qui reg re78 train age educ black hisp re74 re75, robust
estimates store reg3

qui sum train if train == 0
scalar N_C = r(N)
qui sum train if train == 1
scalar N_T = r(N)

outreg2 [reg1 reg2 reg3] using TABLE_2.xls, nocons adds(# of units in Treatment, N_T, # of units in Control, N_C) excel replace


***********    d    ***********


qui reg re78 train age educ black hisp
dfbeta(train)

gen influence_train = _dfbeta_1
drop _dfbeta_1

sort influence_train

gen index =_n

drop if index == 3 | index == 5 | index == 10 | index == 445 | index == 440 | index == 448

reg re78 train age educ black hisp, robust

}


************************************************	QUESTION 2 		************************************************

quietly{
	
use jtrain3.dta, clear


***********    a    ***********


matrix balance2 = (.,.,.,.,.,.,.,.)

local i = 1

foreach var of varlist age educ black hisp re74 re75 {
	
	quietly sum `var' if train == 1, d
	
	matrix balance2[`i',1]=r(mean)
	matrix balance2[`i',3]=r(sd)

	
	quietly sum `var' if train == 0, d
	
	matrix balance2[`i',2]=r(mean)
	matrix balance2[`i',4]=r(sd)
	
	
	quietly regress `var' train, robust
	matrix rtable = r(table)

	matrix balance2[`i',5] = rtable[1,1]
	matrix balance2[`i',6] = rtable[2,1]
	matrix balance2[`i',7] = rtable[3,1]
	matrix balance2[`i',8] = rtable[4,1]
	
	local i = `i'+1
	
	if `i'<7  matrix balance2=(balance2 \ .,.,.,.,.,.,.,.)
	
}

matrix rownames balance2 = age educ black hisp re74 re75
matrix colnames balance2 = mean_T mean_C StdDev_T StdDev_C Diff StdE_Diff t-stat p-value

matrix list balance2

matrix top = balance2[1..4,1..8]
matrix list top
matrix bot = balance2[5..6,1..8]
matrix list bot


putexcel set TABLE_1.xls, modify
putexcel K1=matrix(top), hcenter colnames
putexcel K7=matrix(bot), hcenter


***********    b    ***********


set seed 29835

generate treated = .

generate random = runiform()

sort random

generate index = _n

qui sum age
scalar cutoff = r(N)/2

replace treated = 0 if index <= cutoff
replace treated = 1 if index > cutoff & index<=cutoff*2


***********    c    ***********


randtreat, generate(treated_2)

pwcorr treated treated_2, sig


***********    d    ***********


matrix balance3 = (.,.,.,.,.,.,.,.)

local i = 1

foreach var of varlist age educ black hisp re74 re75 {
	
	quietly sum `var' if treated == 1, d
	
	matrix balance3[`i',1]=r(mean)
	matrix balance3[`i',3]=r(sd)

	
	quietly sum `var' if treated == 0, d
	
	matrix balance3[`i',2]=r(mean)
	matrix balance3[`i',4]=r(sd)
	
	
	quietly regress `var' treated, robust
	matrix rtable = r(table)

	matrix balance3[`i',5] = rtable[1,1]
	matrix balance3[`i',6] = rtable[2,1]
	matrix balance3[`i',7] = rtable[3,1]
	matrix balance3[`i',8] = rtable[4,1]
	
	local i = `i'+1
	
	if `i'<7  matrix balance3=(balance3 \ .,.,.,.,.,.,.,.)
	
}

matrix rownames balance3 = age educ black hisp re74 re75
matrix colnames balance3 = mean_T mean_C StdDev_T StdDev_C Diff StdE_Diff t-stat p-value


matrix list balance3

matrix top = balance3[1..4,1..8]
matrix list top
matrix bot = balance3[5..6,1..8]
matrix list bot


putexcel set TABLE_1.xls, modify
putexcel T1=matrix(top), hcenter colnames
putexcel T7=matrix(bot), hcenter


***********    e    ***********


qui reg re78 treated, robust
estimates store reg1

qui reg re78 treated age educ black hisp, robust
estimates store reg2

qui reg re78 treated age educ black hisp re74 re75, robust
estimates store reg3

qui sum train if treated == 0
scalar N_C = r(N)
qui sum train if treated == 1
scalar N_T = r(N)

outreg2 [reg1 reg2 reg3] using TABLE_2.xls, nocons adds(# of units in Treatment, N_T, # of units in Control, N_C) excel append


***********    f    ***********


qui reg re78 train, robust
estimates store reg1

qui reg re78 train age educ black hisp, robust
estimates store reg2

qui reg re78 train age educ black hisp re74 re75, robust
estimates store reg3

qui sum train if train == 0
scalar N_C = r(N)
qui sum train if train == 1
scalar N_T = r(N)

outreg2 [reg1 reg2 reg3] using TABLE_2.xls, nocons adds(# of units in Treatment, N_T, # of units in Control, N_C) excel append
	
}


************************************************	QUESTION 3 		************************************************

quietly{


***********    a    ***********


***********    b    ***********


***********    c    ***********


use jtrain2.dta, clear

reg re78 train, robust
scalar myb = _b[train]
scalar list myb

generate index = .
generate random = .
generate treatment = .
scalar conta = 0

forvalues i=1(1)100000 {
	qui replace random = runiform()
		sort random
	qui replace index = _n
	qui replace treatment = 1 if index <= 185
	qui replace treatment = 0 if index > 185
	qui sum re78 if treatment == 0
		scalar mean_c = r(mean)
	qui sum re78 if treatment == 1
		scalar mean_t = r(mean)
		scalar Tave = mean_t - mean_c
	if abs(Tave) >= myb {
		scalar conta = conta + 1
	}

}

qui scalar p_value = conta/100000
scalar list p_value
	
	
}
