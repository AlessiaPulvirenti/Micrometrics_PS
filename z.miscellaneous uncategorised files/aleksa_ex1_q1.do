clear all
cd ""
use pset_2_q_1.dta
qui des
sort birthdate
vl average_education=.
foreach x in birthdate Education{
	qui sum Education
	average_education=average_education+r(mean)
}
list average_education
*collapse birthyear birthqtr Education (mean), by(birthdate)

