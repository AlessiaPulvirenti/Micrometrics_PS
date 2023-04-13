net install rdrobust, ///
from(https://raw.githubusercontent.com/rdpackages/rdrobust/master/stata) replace
net install rddensity, ///
from(https://raw.githubusercontent.com/rdpackages/rddensity/master/stata) replace
net install lpdensity, ///
from(https://raw.githubusercontent.com/nppackages/lpdensity/master/stata) replace

cd "C:\Users\elena\OneDrive\Desktop\ESS\2nd year\Microeconometrics\PS\problem set 3"
use pset_3.dta

**QX 1
rdplot T X, graph_options(title(T-X Discontinuity) legend(off)) 
graph rename T_X, replace



**QX2

*Gonzalez (2021) is a paper that studies the effect of cell phone coverage on electoral frauds.

use fraud_pcenter_final.dta

*The distance between the polling centers and their closest points with 2G coverage is titled "_dist"; the cell phone coverage indicator is titled "cov".

*(a)
rdplot cov _dist, graph_options(title(T-X Discontinuity) legend(off)) 
graph rename T_X, replace
///distance approx. above 25 from the closest points with 2G coverage positive coverage, above approx. 25 no coverage. 
rdrobust cov _dist

**fuzzy RD - the treatment status (coverage) is not a deterministic function of the running variable (distance from the closest points with 2G coverage). 

**identification assumptions:
*- potential outcome functions must be continuous in the treatment boundary, meaning centers' characteristics must not vary discountinuosly when transitioning from the coverage to the noncoverage side, the so-called treatment boundary.
*- CENTERS' SELECTION IN COVERAGE: assignment rule - cell phone coverage - must be free of manipulation. We have to show there is no endogenous sorting of the polling center locations in areas close to the boundary. 

*(b) Point out in which setting does having a proxy for longitude does not require you to change RD design. Hint: Read the "Additional Results" section of Gonzalez (2021) and reflect on which type of cell phone coverage boundary would deliver you this result.

*The cell phone coverage boundary becomes fuzzy because of i.e., day-to-day and even within-day variations in factors such as temperature, precipitation, and conductivity.

*From Gonzalez (2021): "Note that results using a sharp RD design in the paper are not necessarily affected by the potential fuzziness of the boundary. However, they should be interpreted as an intent-to-treat effect." <...> "one cannot implement a fuzzy RD design in this setting, because that would entail knowing exactly how much the boundary varied on election day. Instead, I replicate the main results using random shifts in the boundary within a 1 km bandwidth of the reported boundary. Specifically coverage) I do 100 random shifts inward (contraction in and 100 random shifts (coverage). outward (expansion in coverage)."



**(c)


