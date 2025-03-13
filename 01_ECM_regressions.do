
* JAN 31, 2023
* Modified: March-13, 2025 (by Brian)

* (1) ECM and Cointegration tables
* (2) Run ECM for each group: Low-frequency debt issued & High-frequency debt issued

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
gl input "${user}/Replication"
gl tables "${user}/Tables"

******************************************************************************

*use "C:\Users\bsorense\Dropbox\SchoolDistrict2022\replication\data\schooldistrict_panel.dta"	
use "$input/data/schooldistrict_panel.dta", clear

xtset panelid timeid // declare panel

codebook id_govs, compact // check #SD is 8569 and #years is 40 (1979-2018)

gen lncapital = ln(capital+1)
gen lnincomecnty = ln(PerCapitaCntyIncome+1)
gen lntotcur= ln(totalcurrentoper+1)

//replace ln_capcur_ratio = ln((capital_r+1)/(totcur_r+1))

gen enrollpop= enrollment/popcnty
gen lnenrollpop=ln(enrollment/popcnty)
gen lnenrollment=ln(enrollment)
gen lnpop =ln(popcnty)

* Export main variables
export delimited year id_govs lncapital lnincomecnty lnenrollment lnpop ///
using "$input/Empirical_Application/sd_data.csv", novarnames replace
*using C:\Users\edloaeza\Dropbox\SchoolDistrict2022\04092\Empirical_Application\sd_data.csv", novarnames replace


** Tables (with 4 lags) **

* Cointegration Test:
xtcointtest pedroni lncapital lnincomecnty lntotcur lnenrollment lnpop, demean

collect create pedroni_table, replace
quietly: collect r(stats) r(p): xtcointtest pedroni lncapital lnincomecnty lntotcur lnenrollment lnpop, demean

collect layout (colname) (result)
//collect layout (colname["Pedroni test statistics" "p-values"]) (result[stats p]) 

** Print table of collection:
collect export "$tables/coint1.tex", name(pedroni_table) tableonly replace

*** Main Regressions  (Full Sample)

foreach y of varlist lncapital lntotcur {
	
	*ECM Capital:
	xtreg `y' lnincomecnty lnenrollment lnpop, fe 
	estimates store e`y'
	*outreg2 using "$tables/reg1.tex",  dec(3) replace ctitle(ln_capital) 

	predict resid, residuals

	*xtpedroni ln_capital ln_income_cnty ln_enrollment ln_pop, notdum notest mlags(3) 

	reg d.`y' l.resid d.l(0/4).lnincomecnty d.l(1/4).lncapital d.l(0/4).lnenrollment d.l(0/4).lnpop  // ECM-1
	estimates store ed`y'
	*outreg2 using "$tables/reg1.tex",  dec(3) append ctitle(ln_capital) 
	
	if "`y'"=="lncapital" {
		estout e`y' ed`y' using "$tables/reg_main_`y'.tex", replace drop(_cons) style(tex) ///
		cells(b(star fmt(3)) se(par fmt(3))) mlabels(none) collabels(none) ///
		label starlevels(* .1 ** .05 *** .01) prehead(`"& Log of Capital & $\Delta$ Log of Capital \\ \hline "') ///
		stats(N_g N , fmt(%9.0g %9.0g) labels("School Districts" "Observations"))
	}
	
	else {
		estout e`y' ed`y' using "$tables/reg_main_`y'.tex", replace drop(_cons) style(tex) ///
		cells(b(star fmt(3)) se(par fmt(3))) mlabels(none) collabels(none) ///
		label starlevels(* .1 ** .05 *** .01) prehead(`"& Log of Current Exp. & $\Delta$ Log of Current Exp. \\ \hline "') ///
		stats(N_g N , fmt(%9.0g %9.0g) labels("School Districts" "Observations"))
	
	}

	drop resid

}
*****************************************************************************
/*
*****************************************************************************
	*ECM Current Exp:
	xtreg lntotcur lnincomecnty lnenrollment lnpop, fe 
	estimates store lntotcur
	*outreg2 using "$tables/reg2.tex",  dec(3) replace ctitle(ln_totcur) 

	predict resid2, residuals

	reg d.lntotcur l.resid2 d.l(0/4).lnincomecnty d.l(1/4).lntotcur  d.l(0/4).lnenrollment d.l(0/4).lnpop  // ECM-2
	estimates store dlntotcur

	*outreg2 using "$tables/reg2.tex",  dec(3) append ctitle(ln_totcur) 
	*/
	
	
/* ======== ANALYSIS BY GROUP OF FREQUENT AND INFREQUENT DEBT ISSUED ======== */

*Run ECM for each group: Low-frequency debt issued & High-frequency debt issued **

* Adding belowp50 from the INTRAS06 file
merge m:1 fips_state using "$temp/temp_below50.dta", keep(match master) keepus(belowp50) nogen

*belowp50=1 "Low-frequency debt issued"
*belowp50=0 "High-frequency debt issued"


foreach y of varlist lncapital lntotcur {
	forvalues i=0/1 {

		xtcointtest pedroni lncapital lnincomecnty lntotcur lnenrollment lnpop if belowp50==`i', demean

		*ECM Capital:
		xtreg `y' lnincomecnty lnenrollment lnpop if belowp50==`i', fe 
		estimates store e`y'`i'
		*outreg2 using "$tables/reg3.tex",  dec(3) replace ctitle(ln_capital) 

		predict resid, residuals

		reg d.`y' l.resid d.l(0/4).lnincomecnty d.l(1/4).lncapital d.l(0/4).lnenrollment d.l(0/4).lnpop if belowp50==`i' // ECM-1
		estimates store ed`y'`i'
		*outreg2 using "$tables/reg3.tex",  dec(3) append ctitle(ln_capital)
		
		drop resid
			
		if "`y'"=="lncapital" {
			estout e`y'`i' ed`y'`i' using "$tables/reg_`y'_by_debt_frequency_`i'.tex", replace drop(_cons) style(tex) ///
			cells(b(star fmt(3)) se(par fmt(3))) mlabels(none) collabels(none) ///
			label starlevels(* .1 ** .05 *** .01) prehead(`"& Log of Capital & $\Delta$ Log of Capital \\ \hline "') ///
				stats(N_g N , fmt(%9.0g %9.0g) labels("School Districts" "Observations")) 
		}
		
		else {
			estout e`y'`i' ed`y'`i' using "$tables/reg_`y'_by_debt_frequency_`i'.tex", replace drop(_cons) style(tex) ///
		cells(b(star fmt(3)) se(par fmt(3))) mlabels(none) collabels(none) ///
		label starlevels(* .1 ** .05 *** .01) prehead(`"& Log of Current Exp. & $\Delta$ Log of Current Exp. \\ \hline"') ///
			stats(N_g N , fmt(%9.0g %9.0g) labels("School Districts" "Observations")) 
		}
		
		}
}

/*	
forvalues i=0/1 {
	
	*ECM Current Exp:
	xtreg lntotcur lnincomecnty lnenrollment lnpop if belowp50==`i', fe 
		*outreg2 using "$tables/reg4.tex",  dec(3) replace ctitle(ln_totcur) 

	predict resid2, residuals

	reg d.lntotcur l.resid2 d.l(0/4).lnincomecnty d.l(1/4).lntotcur d.l(0/4).lnenrollment d.l(0/4).ln_pop if belowp50==`i' // ECM-2
	*outreg2 using "$tables/reg4.tex",  dec(3) append ctitle(ln_totcur) 

	drop resid resid2	
	}
	
	
*restore

/* ------------------------ * High-frequency debt issued ------------------- */

preserve
keep if belowp50== 0

xtcointtest pedroni ln_capital ln_income_cnty ln_totcur ln_enrollment ln_pop, demean


*ECM Capital:

//rename ln_capital_r_ps cap 
//rename ln_income_r_cnty_pc inc
//xtpedroni cap inc ln_enrollment ln_pop


//rename  cap ln_capital_r_ps
//rename  inc ln_income_r_cnty_pc


xtreg ln_capital ln_income_cnty ln_enrollment ln_pop, fe 
outreg2 using "$tables/reg5.tex",  dec(3) replace ctitle(ln_capital) 


predict resid3, residuals

reg d.ln_capital l.resid3 d.l(0/4).ln_income_cnty d.l(1/4).ln_capital  d.l(0/4).ln_enrollment d.l(0/4).ln_pop  // ECM-1
outreg2 using "$tables/reg5.tex",  dec(3) append ctitle(ln_capital) 


*ECM Current Exp:
xtreg ln_totcur ln_income_cnty ln_enrollment ln_pop, fe 
outreg2 using "$tables/reg6.tex",  dec(3) replace ctitle(ln_totcur) 

predict resid4, residuals

reg d.ln_totcur l.resid4 d.l(0/4).ln_income_cnty d.l(1/4).ln_totcur  d.l(0/4).ln_enrollment d.l(0/4).ln_pop  // ECM-2
outreg2 using "$tables/reg6.tex",  dec(3) append ctitle(ln_totcur) 


restore


*/












