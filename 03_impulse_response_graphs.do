* Sameer	
* ECM, Cointegration, and Impulse Response
* March 13, 2025
** Modified by Brian



clear all
set more off

gl name "BM" // EL, MS, BM we can add the paths for all members 

if "${name}"=="EL" {
	gl user "C:/Users/edloaeza/Dropbox/SchoolDistrict2022/replication"
	}
else if "${name}"=="MS" {
	gl user "C:\Users\Mashrur\Dropbox\IntraSDProject"
	}
else if "${name}"=="BM" {
	gl user "C:/Users/bmmur/UH-ECON Dropbox/Brian Murphy/SchoolDistrict2022"
	}
	
	
gl temp "${user}/temp"
gl data "${user}/Replication/data"
gl tabfig "${user}/Tables"

* For graphs
ssc install schemepack, replace
set scheme white_tableau   
graph set window fontface "Arial Narrow"


*******************************************************************************
use "$data/schooldistrict_panel.dta", clear

xtset panelid timeid // declare panel

codebook id_govs, compact // check #SD is 8569 and #years is 40 (1979-2018)

/*
* Drop Voc or Tech SD in MA:
drop if id_govs == "225009906" | id_govs == "225009907" |id_govs == "225012901" // 
codebook id_govs, compact // check #SD is 824
*/


gen ln_capital = ln(capital+1)
gen ln_income_cnty = ln(PerCapitaCntyIncome+1)
gen ln_totcur= ln(totalcurrentoper+1)

//replace ln_capcur_ratio = ln((capital_r+1)/(totcur_r+1))

gen enrollpop= enrollment/popcnty
gen ln_enrollpop=ln(enrollment/popcnty)
gen ln_enrollment=ln(enrollment)
gen ln_pop =ln(popcnty)


/*
** TASK 1: ECM and Cointegration Tables (with 4 lags) **

* Cointegration Test:
xtcointtest pedroni ln_capital ln_income_cnty ln_totcur ln_enrollment ln_pop, demean


collect create pedroni_table, replace
quietly: collect r(stats) r(p): xtcointtest pedroni ln_capital ln_income_cnty ln_totcur ln_enrollment ln_pop, demean

collect layout (colname) (result)
//collect layout (colname["Pedroni test statistics" "p-values"]) (result[stats p]) 

** Print table of collection:
collect export coint1.tex, name(pedroni_table)  tableonly replace


//rename (ln_capital_r_ps ln_income_r_cnty_pc ln_totcur_r_ps ln_enrollment ln_pop) (cap inc curr enr pop)

//xtpedroni cap inc enr pop //,notest 

//rename (cap inc curr enr pop) (ln_capital_r_ps ln_income_r_cnty_pc ln_totcur_r_ps ln_enrollment ln_pop) 


*ECM Capital:
xtreg ln_capital ln_income_cnty ln_enrollment ln_pop, fe 

//rename (ln_capital_r_ps ln_income_r_cnty_pc ln_enrollment ln_pop) (capital income enroll pop)
//xtpedroni capital income enroll pop, notest  notdum 
//rename  (capital income enroll pop) (ln_capital_r_ps ln_income_r_cnty_pc ln_enrollment ln_pop)


outreg2 using reg1.tex,  dec(3) replace ctitle(ln_capital) 

predict resid, residuals

reg d.ln_capital l.resid d.l(0/4).ln_income_cnty d.l(1/4).ln_capital  d.l(0/4).ln_enrollment d.l(0/4).ln_pop  // ECM-1
outreg2 using reg1.tex,  dec(3) append ctitle(ln_capital) 

 
*ECM Current Exp:
xtreg ln_totcur ln_income_cnty ln_enrollment ln_pop, fe 
outreg2 using reg2.tex,  dec(3) replace ctitle(ln_totcur_r) 


predict resid2, residuals

reg d.ln_totcur l.resid2 d.l(0/4).ln_income_cnty d.l(1/4).ln_totcur  d.l(0/4).ln_enrollment d.l(0/4).ln_pop  // ECM-2
outreg2 using reg2.tex,  dec(3) append ctitle(ln_totcur_r) 


drop resid resid2
*/

** TASK 2: Impulse Response Functions and Plots **

xtreg ln_capital ln_income_cnty ln_enrollment ln_pop, fe // Spurious Regression

estimates store m1

** are these the long-run coefficients?
scalar alpha_inc = _b[ln_income_cnty]		// Storing the coeff. as scalars
scalar alpha_enroll = _b[ln_enrollment]
scalar alpha_pop = _b[ln_pop]

predict resid, residuals		// Taking residuals
ereturn list		// post the estimation result (residual)

gen lead = _n if _n<22 		// Lead years on the IRFs
replace lead = lead - 1 // 0-20 numbering

dis alpha_inc

// i=4(2)10
forvalues i=4(1)4 {
reg d.ln_capital l.resid d.l(0/`i').ln_income_cnty d.l(1/`i').ln_capital  d.l(0/`i').ln_enrollment d.l(0/`i').ln_pop // ECM-1

ereturn list 

scalar gamma = _b[L.resid]

forvalues j=1(1)`i' {
scalar zita_inc_0 = _b[D.ln_income_cnty]
scalar zita_inc_1 = _b[LD.ln_income_cnty]
scalar zita_inc_`j' = _b[L`j'D.ln_income_cnty]

scalar zita_enroll_0 = _b[D.ln_enrollment]
scalar zita_enroll_1 = _b[LD.ln_enrollment]
scalar zita_enroll_`j' = _b[L`j'D.ln_enrollment]

scalar zita_pop_0 = _b[D.ln_pop]
scalar zita_pop_1 = _b[LD.ln_pop]
scalar zita_pop_`j' = _b[L`j'D.ln_pop]
}

*
scalar inc_0 = 0
scalar k_0 = 0
scalar enroll_0 = 0
scalar pop_0 = 0

scalar del_inc_1 = .1       // 10% shock
scalar del_enroll_1 = .1
scalar del_pop_1 = .1

local variable ln_income_cnty ln_enrollment ln_pop
foreach var of local variable {
	forvalues k=1(1)`i' {
scalar `var'_`k' = .1 
}
}

local _variable _inc _enroll _pop  
foreach x of local _variable {
	forvalues m=1(1)`i' {
	    local r = `m'+1
		display "r=`r'"
	scalar del_k`x'_1 = zita`x'_0*del`x'_1
	scalar k`x'_1 = del_k`x'_1 + k_0 
	display "Line 70_`m'"

scalar del_k`x'_`r' = gamma*(k`x'_`m' - alpha`x'*.1) + zita`x'_`m'*del`x'_1
display "Line 75_`m'"
scalar k`x'_`r' = k`x'_`m' + del_k`x'_`r' 
display "Line k`x'_`r'"
}

forvalues p=`i'(1)18 {
    local u=`p'+2
	local s=`p'+1
    display "u=`u'"
	scalar k`x'_`u' = k`x'_`s' + gamma*(k`x'_`s'-alpha`x'*.1)
}


gen k`x'_L`i'=.
forval q=1/20 {
	replace k`x'_L`i'=scalar(k`x'_`q') in `q'
}
gen cap`x'_L`i' = k`x'_L`i' 
replace cap`x'_L`i' = k`x'_L`i'[_n-1]
replace cap`x'_L`i'=0 if _n==1
drop k`x'_L`i'
rename cap`x'_L`i' k`x'_L`i' 
}


twoway connected k_inc_L`i' lead, sort name(gr_k_inc_L`i') ///
xtitle("") xlabel(, labsize(medlarge)) ///
ytitle(k (in decimal), size(medium)) ylabel(, angle(0) labsize(medium)) yline(`=alpha_inc')
*subtitle("Impulse response of capital per student (k)" "due to 10% increase in per-capita income by county") ///
*yline(0.0947) // yline(0.10) 
graph export "$tabfig/FImpRes_k_income.png", as(png) replace

twoway connected k_enroll_L`i' lead, sort name(gr_k_enroll_L`i') ///
xtitle("") xlabel(, labsize(medlarge)) ///
ytitle(k (in decimal),size(medium)) ylabel(, angle(0) labsize(medium)) yline(`=alpha_enroll')
* subtitle("Impulse response of capital per student (k)" "due to 10% increase in enrollment") ///
*yline(-0.0421) // yline(-0.0176) 
graph export "$tabfig/FImpRes_k_enrollment.png", as(png) replace

twoway connected k_pop_L`i' lead, sort name(gr_k_pop_L`i') ///
xtitle(Years, size(large)) xlabel(, labsize(medlarge)) ///
ytitle(k (in decimal), margin(medium) size(medium)) ylabel(, angle(0) labsize(medium)) yline(`=alpha_pop')
*subtitle("Impulse response of capital per student (k)" "due to 10% increase in population") ///
* yline(0.0538) // yline(0.0405) 
graph export "$tabfig/FImpRes_k_pop.png", as(png) replace

gr combine gr_k_inc_L`i' gr_k_enroll_L`i' gr_k_pop_L`i', name(gr_k_L`i') ///
subtitle("`i' Lags in ECM") ///
note("Note: The shock (10% increase in an explanatory variable) is given at period 1 (lead=1 here) only." "	Initial values of all variables are set at 0.")
graph export "$tabfig/FImpulseResposne.png", as(png) replace
} 
  

drop resid lead




****

*twoway connected k_inc_L`i' lead, sort name(gr_k_inc_L`i') xtitle("") ///
*ytitle(k (in decimal), margin(medium) size(medium)) ylabel(, angle(0)) ///
*subtitle("Impulse response of capital per student (k)" "due to 10% increase in per-capita income by county") ///
*yline(0.0947) // yline(0.10) 













