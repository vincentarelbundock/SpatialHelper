* TODO: exclusive/zero_loop

*******************
* Specific origin *
*******************
* Panel 
import delimited "data/pan.csv", clear 
local weights ik ki /* ki im mi mj jk kj */
foreach w of local weights{
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(source) time(year) link(`w') sename(wy)
  quietly export delimited using "data/pan_specific_origin_`w'.csv", replace
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(source) time(year) link(`w') sename(wy) norowst
  quietly export delimited using "data/pan_specific_origin_`w'_norowst.csv", replace
}

* Cross-section
import delimited "data/cs.csv", clear 
local weights ik ki /* mi kj ki jk im */
foreach w of local weights{
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(source) time(year) link(`w') sename(wy)
  quietly export delimited using "data/cs_specific_origin_`w'.csv", replace
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(source) time(year) link(`w') sename(wy) norowst
  quietly export delimited using "data/cs_specific_origin_`w'_norowst.csv", replace
}

************************
* Specific destination *
************************
* Panel 
import delimited "data/pan.csv", clear 
local weights jm mj /* ki im mi jk kj */
foreach w of local weights{
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(target) time(year) link(`w') sename(wy)
  quietly export delimited using "data/pan_specific_destination_`w'.csv", replace
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(target) time(year) link(`w') sename(wy) norowst
  quietly export delimited using "data/pan_specific_destination_`w'_norowst.csv", replace
}

* Cross-section
import delimited "data/cs.csv", clear 
local weights jm mj /* ki ik im mi jk kj */
foreach w of local weights{
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(target) time(year) link(`w') sename(wy)
  quietly export delimited using "data/cs_specific_destination_`w'.csv", replace
  quietly spspc y, weightvar(w) source(unit1) target(unit2) form(target) time(year) link(`w') sename(wy) norowst
  quietly export delimited using "data/cs_specific_destination_`w'_norowst.csv", replace
}


********************
* Aggregate origin *
********************
* Panel
import delimited "data/pan.csv", clear 
local weights ik ki /* im mi */
foreach w of local weights{
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(source) time(year) link(`w') sename(wy)
  quietly export delimited using "data/pan_aggregate_origin_`w'.csv", replace
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(source) time(year) link(`w') sename(wy) norowst
  quietly export delimited using "data/pan_aggregate_origin_`w'_norowst.csv", replace
}

* Cross-section
import delimited "data/cs.csv", clear 
local weights ik ki /* im mi */
foreach w of local weights{
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(source) link(`w') sename(wy)
  quietly export delimited using "data/cs_aggregate_origin_`w'.csv", replace
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(source) link(`w') sename(wy) norowst
  quietly export delimited using "data/cs_aggregate_origin_`w'_norowst.csv", replace
}

*************************
* Aggregate destination *
*************************
* Panel
import delimited "data/pan.csv", clear 
local weights jm mj /* jk kj */
foreach w of local weights{
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(target) time(year) link(`w') sename(wy)
  quietly export delimited using "data/pan_aggregate_destination_`w'.csv", replace
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(target) time(year) link(`w') sename(wy) norowst
  quietly export delimited using "data/pan_aggregate_destination_`w'_norowst.csv", replace
}

* Cross-section
import delimited "data/cs.csv", clear 
local weights jm mj /* jk kj */
foreach w of local weights{
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(target) link(`w') sename(wy)
  quietly export delimited using "data/cs_aggregate_destination_`w'.csv", replace
  quietly spagg y, weightvar(w) source(unit1) target(unit2) form(target) link(`w') sename(wy) norowst
  quietly export delimited using "data/cs_aggregate_destination_`w'_norowst.csv", replace
}
