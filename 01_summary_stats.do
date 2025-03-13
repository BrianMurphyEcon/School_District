* Sameer & Eva:
* Investigation: Means by State for frequency of debt issued using v01 data (12th April 2022)
* Modified: Feb-13, 2023 (by Eva)
*******************************************************************************
clear
set more off

gl name "BM" // EL, SAM, BM we can add the paths for all members 

if "${name}"=="EL" {
	gl user "C:/Users/edloaeza/Dropbox/SchoolDistrict2022"
	}
else if "${name}"=="SAM" {
	gl user "/Users/sam/Dropbox/Shared Folder/SchoolDistrict2022"	
	}
else if "${name}"=="BM" {
	gl user "C:/Users/bmmur/UH-ECON Dropbox/Brian Murphy/SchoolDistrict2022"
	}

gl temp "${user}/temp"
gl input "${user}/Replication/data"
gl tables "${user}/Tables"

******************************************************************************

*use "C:\Users\bsorense\Dropbox\SchoolDistrict2022\replication\data\schooldistrict_panel.dta"	
use "$input/schooldistrict_panel.dta", clear

xtset panelid timeid // declare panel

codebook id_govs, compact // check #SD


** TASK: Divide SDs into two groups: Low-frequency debt issued & High-frequency debt issued and run ECM on each if there is a difference **

* Create dummies for counting debt issued:
** How many SD issued debt
* Dummy for issued debt or not
* ssc install unique
gen dummy_issueddebt = totalltdissued>0
label variable dummy_issueddebt "=1 if issued debt>0"
egen yrs_issueddebt=total(dummy_issueddebt), by(id_govs)
unique id_govs if yrs_issueddebt==0
unique id_govs if yrs_issueddebt==40

* Means by state, because the rules can vary by state:
collapse (mean) yrs_issueddebt enrollment totaldebtoutstanding totalltdissued totalcapitaloutlays capital, by(id_govs fips_state state_name msa1993)
egen mean_yrsissueddebt = mean(yrs_issueddebt), by(fips_state)
egen numsd_state = count(id_govs), by(fips_state)

gl varlist mean_yrsissueddebt enrollment totaldebtoutstanding totalcapitaloutlays capital totalltdissued

* collapse by state
*collapse (mean) mean_yrsissueddebt numsd_state totaldebtoutstanding totalcapitaloutlays capital (sum) enrollment, by(fips_state state_name)
collapse (mean) numsd_state $varlist (sum) totenrollment = enrollment, by(fips_state state_name)

replace enrollment= round(enrollment,1)

gen lnenrollment=log(enrollment)

* Histograms
sum mean_yrsissueddebt, detail
local a = r(mean)
local b = r(p50)

* Dummy for Low-frequency debt issued & High-frequency debt issued by States:
g belowp50 = mean_yrsissueddebt<`b'
egen totsd=total(numsd_state), by(belowp50)
tab totsd belowp50

label var mean_yrsissueddebt "Number of Years Issued Debt"
label var enrollment "Enrollment"
label var totaldebtoutstanding "Debt Outstanding"
label var totalcapitaloutlays "Investment"
label var capital "Capital"
label var totalltdissued "Debt Issued"

*bys belowp50: sum mean_yrsissueddebt enrollment totaldebtoutstanding totalcapitaloutlays capital

// Summarize Table	
	
estpost tabstat $varlist, statistics(mean sd) columns(statistics) listwise by(belowp50) ///
	nototal missing casewise

esttab using "$tables/tStatsByDebtIssued.tex", ///
	noobs main(mean) aux(sd) nomtitle nonumber unstack label ///
	nostar nonote b(%9.0fc) mlabels(none) ///
	prehead(`"\multicolumn{3}{c}{High vs. Low frequency debt issued states} \\"') ///
	posthead(`"& \multicolumn{2}{c}{} \\ &  High Frequent & Low Frequent \\ \hline "') ///
	replace f
		
*histogram( mean_yrsissueddebt) if numsd_state>1, bin(10) frequency
*histogram( mean_yrsissueddebt), bin(10) frequency 
*histogram( numsd_state), bin(10) frequency

* Stats by State
tabstat mean_yrsissueddebt numsd_state, stats(mean) by( state ) notot

* ssc install blindschemes
set scheme plotplain 

label define belowp50 0 "High-frequency" 1 "Low-frequency"
label values belowp50 belowp50

label var totaldebtoutstanding "Debt Outstanding Per Student"
label var totalcapitaloutlays "Investment Per Student"
label var capital "Capital Per Student"
label var totalltdissued "Debt Issued Per Student"

gl finvars totaldebtoutstanding totalltdissued totalcapitaloutlays capital

foreach x in $finvars {
	twoway (scatter mean_yrsissueddebt `x' [w=totenrollment], msymbol(circle_hollow)) || ///
	(scatter mean_yrsissueddebt `x', mlabel(state) m(i)) || ///
	(lfit mean_yrsissueddebt `x' [w=totenrollment]), ///
	ytitle("Av. number of years SD issued debt") ///
	legend(off) ///
	note("Note: The size of the circle represents the average total enrollment for the state.")
	graph export "$tables/gScatter`x'.png", replace
}

foreach x in $finvars {
	twoway (scatter mean_yrsissueddebt `x' [w=totenrollment], msymbol(circle_hollow)) || ///
	(scatter mean_yrsissueddebt `x', mlabel(state) m(i)) || ///
	(lfit mean_yrsissueddebt `x' [w=totenrollment]), by(belowp50, legend(off)) ///
	ytitle("Av. number of years SD issued debt") legend(off)
	graph export "$tables/gScatterFreq`x'.png", replace
}

/*
twoway (scatter mean_yrsissueddebt numsd_state [w=totenrollment], msymbol(circle_hollow)) || ///
(scatter mean_yrsissueddebt numsd_state, mlabel(state) m(i)) || ///
lfit mean_yrsissueddebt numsd_state [w=totenrollment], ///
ytitle("Av. number of years SD issued debt") ///
xtitle("Number of SD in the state") legend(off) ///
note("Note: The size of the circle represents the average total enrollment for the state.")
graph export "${user}/Results/scatter.png", replace

twoway (scatter mean_yrsissueddebt enrollment [w=numsd_state], msymbol(circle_hollow)) || ///
(scatter mean_yrsissueddebt enrollment, mlabel(state) m(i)) || ///
lfit mean_yrsissueddebt enrollment [w=numsd_state], ///
ytitle("Av. number of years SD issued debt") ///
xtitle("Log Total Enrollment by State") legend(off)
graph export "${user}/Results/scatterb.png", replace

twoway (scatter mean_yrsissueddebt numsd_state, mlabel(state)) || ///
lfit mean_yrsissueddebt numsd_state, by(belowp50) ///
ytitle("Av. number of years SD issued debt") ///
xtitle("Number of SD in the state")

*/

keep fips_state belowp50
save "$temp\temp_below50.dta", replace



