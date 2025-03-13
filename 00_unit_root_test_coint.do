/*==========================================================================
* Author: Eva Loaeza
* Date: April 2, 2023
* Unit Root Tests, ECM, and Co-integration with the correct data 

========================================================================== */
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

** For latex table
ssc install texdoc, replace

*******************************************************************************
use "$input/schooldistrict_panel.dta", clear

xtset panelid timeid // declare panel

codebook id_govs cntyfips msa1993, compact // check #SD is 2609 , #years is 40 (1979-2018) , #MSA is 61 , amd #cnty is 344

/*
* Variables (per student):
gen lncapital = ln(capital+1)
gen lnincomecnty = ln(PerCapitaCntyIncome+1)
gen lntotcur= ln(totalcurrentoper+1)

//replace ln_capcur_ratio = ln((capital_r+1)/(totcur_r+1))

gen enrollpop= enrollment/popcnty
gen lnenrollpop=ln(enrollment/popcnty)
gen lnenrollment=ln(enrollment)
gen lnpop =ln(popcnty)
*/

** variable debtretired_r_ps is not in data
gl debtvars "totalltdissued totaldebtoutstanding bondfdcashsec sinkingfdcashsec interestongendebt"

*gen local_revenue_r= totlocaligrev_r + genrevownsources_r //Local Revenue (includes taxes and other fees)
*gen local_rev_r_ps = local_revenue_r /enrollment
*gen totalrevenue_r_ps = (generalrevenue_r /enrollment) // Fed + State + Local + Gen.Own.Revenue

gl capvars "capital totalcapitaloutlays totalcurrentoper" 
gl revenuevars "totalfedigrevenue totalstateigrevenue totlocaligrev totalrevenue PerCapitaCntyIncome"
gl taxvars "totaltaxes propertytax totsalesgrrectax totalincometaxes"

* Variables in logs (per student), we have added '+1' because there are some zeros:
foreach var in $capvars $debtvars $revenuevars $taxvars {

gen  ln_`var' =ln(`var'+1)

}

*replace ln_capcur_ratio = ln((capital_r+1)/(totalcurrentoper_r+1))

gen ln_enrollpop=ln(enrollment/popcnty)
gen ln_enrollment=ln(enrollment)
gen ln_pop =ln(popcnty)

label var enrollment "Enrollment"
label var popcnty "County Population"
label var PerCapitaCntyIncome "Per Capita Income (county level)"

** -------------------- Panel unit root tests ---------------------- **

/* First, we want to test if the variables of interest are non-stationary (has unit root)
If the variables are non-stationary, the resulting correlation from OLS 
estimates would be spurious.

The Levin–Lin–Chu (2002), Harris–Tzavalis (1999), Breitung (2000; Breitung and Das 2005), 
Im–Pesaran–Shin (2003), and Fisher-type (Choi 2001) tests have as the null hypothesis 
that all the panels contain a unit root.

Because the Levin–Lin–Chu test requires that the ratio of the number of 
panels to time periods tend to zero asymptotically (T>N), it is not well suited to 
datasets with a large number of panels and relatively few time periods (T<N). 
Thus, we use the Harris–Tzavalis test, which assumes that the number of panels 
tends to infinity while the number of time periods is fixed
*/

** If p-value<0.05, we reject the null hypothesis of a unit root and therefore conclude that the variable is stationary.
foreach var in $capvars PerCapitaCntyIncome enrollment pop {
	xtunitroot ht ln_`var'
	
	* Store statistics from test
	local rho_`var' =  r(rho)
	local z_`var' = r(z)
	local pval_`var' = r(p)

	local rho_`var': di %6.4f `rho_`var''
	local z_`var': di %6.4f `z_`var''
	local pval_`var': di %6.4f `pval_`var''
	
	* store variable names
	gl lbe_`var': var label `var'
}

/* --------------------------------------------------- */
** Create latex table
cd "$tables"
texdoc init tunitroot.tex, replace force
tex \begin{tabular}{lccc} \hline \\
tex Variable & Statistic & z & p-value \\
tex \addlinespace \hline \\
foreach var in $capvars PerCapitaCntyIncome enrollment pop {
	tex ${lbe_`var'} & `rho_`var'' & `z_`var'' & `pval_`var'' \\
}
tex \hline \\
tex \multicolumn{4}{l}{\begin{tabular}[c]{@{}c@{}}\footnotesize{Notes: The null hypothesis is that panels contain unit roots.}\end{tabular}}
tex \end{tabular}
texdoc close
/* --------------------------------------------------- */
/* Optional: 
texdoc init tunitroot.tex, replace force
tex \begin{tabular}{lccc} \toprule \toprule \\
tex \end{tabular}

*/


** -------------------- AR(1) regressions: ---------------------- **
* $capvars $debtvars $revenuevars $taxvars
eststo clear 
foreach var in $capvars PerCapitaCntyIncome enrollment pop   {
eststo: quietly reg ln_`var' L.ln_`var' , cluster(id_govs) 
}

esttab using unitroots.csv, se star(* 0.10 ** 0.05 *** 0.01) 

reg ln_enrollpop L.ln_enrollpop,cluster(id_govs) 
reg ln_enroll L.ln_enroll,cluster(id_govs) 
reg ln_pop L.ln_enrollpop,cluster(id_govs) 


/* Unit root tests by each panel (school district)

Ho: the variable contains a unit root (non-stationary)
Ha: the variable was generated by a stationary process

trend specifies that a trend term has been included in the associated regression and
that the process under the null hypothesis is a random walk, perhaps with drift??		
*/


* To store the p-values from the test: 
foreach var in $capvars PerCapitaCntyIncome enrollment pop  {
	gen p_ln_`var'=0 
}


* Augmented Dickey-Fuller Unit Root Test for each SD:
sum panelid
local scd = r(max)
foreach var in $capvars PerCapitaCntyIncome enrollment pop  {
			display `var'
	forvalues i=1(1)`scd' {
		display `i'
	dfuller ln_`var' if panelid==`i', trend lags(2) 
	replace p_ln_`var'=`r(p)' if panelid==`i'
	}
}


* Dummy=1 if p value<0.05, thus reject Ho:
foreach var in $capvars PerCapitaCntyIncome enrollment pop  {

gen d_ln_`var'=p_ln_`var'<0.05
}

* #SD that contains unit root (out of 827):
foreach var in $capvars $debtvars $revenuevars $taxvars  {
	display "ln_`var'"
	unique id_govs if d_ln_`var'==0
}


* #SD that does not contains unit root (for which we reject the null):
foreach var in $capvars $debtvars $revenuevars $taxvars  {
unique id_govs if d_ln_`var'==1
}

* Taking the above 8 variables as unit roots, we can test for cointegration.

* For the cointegrated variables, we can run ECM.





xtcointtest pedroni ln_capital_r_ps ln_income_r_cnty_pc, demean 

xtreg ln_capital_r_ps ln_income_r_cnty_pc, fe
predict residual, residuals

reg d.ln_capital_r_ps residual d.ln_income_r_cnty_pc d.l.ln_income_r_cnty_pc d.l.ln_capital_r_ps


xtreg ln_enrollpop l.ln_enrollpop,fe

xtcointtest pedroni ln_capital_r ln_income_r_cnty_pc ln_enrollpop, demean 
xtreg ln_capital_r_ps ln_income_r_cnty_pc ln_enrollpop, fe
predict resid, residuals
reg d.ln_capital_r_ps residual d.ln_income_r_cnty_pc d.l.ln_income_r_cnty_pc d.l.ln_capital_r_ps d.ln_enrollpop d.l.ln_enrollpop

xtcointtest pedroni ln_capital_r ln_income_r_cnty_pc ln_enrollpop, demean 
xtreg ln_capital_r_ps ln_income_r_cnty_pc ln_enrollpop, fe
predict resid, residuals
reg d.ln_capital_r_ps resid d.ln_income_r_cnty_pc d.l.ln_income_r_cnty_pc d.l.ln_capital_r_ps d.ln_enrollpop d.l.ln_enrollpop




xtcointtest pedroni ln_capital_r ln_income_r_cnty_pc ln_enroll ln_pop, demean 
xtreg ln_capital_r_ps ln_income_r_cnty_pc ln_enroll ln_pop, fe
predict resid2,residuals
reg d.ln_capital_r_ps l.resid2  d.l(1/2).ln_income_r_cnty_pc d.l(1/2).ln_capital_r_ps  d.l(1/2).ln_enroll d.l(1/2).ln_pop


xtcointtest pedroni ln_capital_r ln_income_r_cnty_pc ln_enroll ln_pop ln_state_aid_r_ps, demean 
xtreg ln_capital_r_ps ln_income_r_cnty_pc ln_enroll ln_pop ln_state_aid_r_ps, fe
predict resid3,residuals
reg d.ln_capital_r_ps l.resid3  d.l(1/2).ln_income_r_cnty_pc d.l(1/2).ln_capital_r_ps  d.l(1/2).ln_enroll d.l(1/2).ln_pop d.l(1/2).ln_state_aid_r_ps



* try 3,4,5 lags
reg d.ln_capital_r_ps l.resid3  d.l(1/5).ln_income_r_cnty_pc d.l(1/5).ln_capital_r_ps  d.l(1/5).ln_enroll d.l(1/5).ln_pop d.l(1/5).ln_state_aid_r_ps


xtcointtest pedroni ln_capital_r ln_income_r_cnty_pc ln_enroll ln_pop ln_state_aid_r_ps ln_totaldebtout_r_ps, demean 
xtreg ln_capital_r_ps ln_income_r_cnty_pc ln_enroll ln_pop ln_state_aid_r_ps ln_totaldebtout_r_ps, fe
predict resid4,residuals
reg d.ln_capital_r_ps l.resid4  d.l(1/4).ln_income_r_cnty_pc d.l(1/4).ln_capital_r_ps  d.l(1/4).ln_enroll d.l(1/4).ln_pop d.l(1/4).ln_state_aid_r_ps d.l(1/4).ln_totaldebtout_r_ps


* Impulse response fns.

// Fit a VAR model
pvar ln_capital_r  ln_enroll
ln_income_r_cnty_pc
 ln_pop ln_state_aid_r_ps ln_totaldebtout_r_ps

// Create impulse–response function myirf and IRF file myirfs.irf
pvarirf create myirf, set(myirfs)

// Graph orthogonalized impulse–response function for dependent variables y1 and y2 given a shock to y1
pvarirf graph oirf, impulse(ln_income_r_cnty_pc) response(ln_capital_r ln_income_r_cnty_pc)












