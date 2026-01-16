**************************************************************************
****************           TABLES          *******************************
**************************************************************************

clear
set more off
set mem 2g
ssc install outreg2, replace
ssc install estout, replace


****** TABLE 1********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\table1.dta"
****Col 1
tab true_nat
****Col 2
codebook patnum if usa==1
codebook patnum if germany==1
codebook patnum if usa==0 & germany==0

****** TABLE 2********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta"

forvalues x=1876/1939 {
	gen td_`x'=0
	qui replace td_`x'=1 if grn==`x'
	}

xtreg count_usa treat count_for_2 td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, replace
xtreg count_usa treat count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa treat td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa count_cl count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa count_cl count_cl_2 count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa count_cl td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa year_conf year_conf_2 count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa year_conf count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append
xtreg count_usa year_conf  td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table2.xls, append

* --- RÉPLICATION DU TABLEAU 2 ---
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear

forvalues x=1876/1939 {
    quietly gen td`x' = (grn == `x')
}

quietly xtreg count_usa treat td*, fe i(class_id) robust cluster(class_id)
estimates store col1

quietly xtreg count_usa treat count_for td*, fe i(class_id) robust cluster(class_id)
estimates store col2

quietly xtreg count_usa count_cl td*, fe i(class_id) robust cluster(class_id)
estimates store col3

quietly xtreg count_usa count_cl count_for td*, fe i(class_id) robust cluster(class_id)
estimates store col4

quietly xtreg count_usa count_cl count_cl_2 count_for td*, fe i(class_id) robust cluster(class_id)
estimates store col5

quietly xtreg count_usa year_conf td*, fe i(class_id) robust cluster(class_id)
estimates store col6

quietly xtreg count_usa year_conf count_for td*, fe i(class_id) robust cluster(class_id)
estimates store col7

quietly xtreg count_usa year_conf year_conf_2 count_for td*, fe i(class_id) robust cluster(class_id)
estimates store col8

esttab col1 col2 col3 col4 col5 col6 col7 col8, ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    keep(treat count_cl count_cl_2 year_conf year_conf_2 count_for) ///
    order(treat count_cl count_cl_2 year_conf year_conf_2 count_for) ///
    stats(N r2, fmt(%9.0f %9.3f) labels("Observations" "R-squared")) ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)") ///
    title("Table 2: OLS Regressions - Patents by US Inventors") ///
    addnote("Robust standard errors clustered at subclass level in parentheses" ///
            "*** p<0.01, ** p<0.05, * p<0.10" ///
            "All regressions include subclass and year fixed effects")

* Export vers Excel avec meilleur format
esttab col1 col2 col3 col4 col5 col6 col7 col8 using "table2_replicated.rtf", ///
    replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    keep(treat count_cl count_cl_2 year_conf year_conf_2 count_for) ///
    order(treat count_cl count_cl_2 year_conf year_conf_2 count_for) ///
    stats(N r2, fmt(%9.0f %9.3f) labels("Observations" "R-squared")) ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)") ///
    title("Table 2: OLS Regressions - Patents by US Inventors") ///
    addnote("Robust standard errors clustered at subclass level in parentheses" ///
            "*** p<0.01, ** p<0.05, * p<0.10" ///
            "All regressions include subclass and year fixed effects")

* Ou export vers LaTeX
esttab col1 col2 col3 col4 col5 col6 col7 col8 using "table2_replicated.tex", ///
    replace ///
    se star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) se(%9.3f) ///
    keep(treat count_cl count_cl_2 year_conf year_conf_2 count_for) ///
    order(treat count_cl count_cl_2 year_conf year_conf_2 count_for) ///
    stats(N r2, fmt(%9.0f %9.3f) labels("Observations" "R-squared")) ///
    mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)") ///
    booktabs ///
    title("Table 2: OLS Regressions - Patents by US Inventors\label{tab:table2}")

****** TABLE 3********
xtreg count_usa count_cl_itt count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using itt_table3.xls, replace
xtreg count_usa count_cl_itt  td*, fe i(class_id) robust cluster(class_id)
outreg2 using itt_table3.xls, append
xtreg count_usa year_conf_itt count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using itt_table3.xls, append
xtreg count_usa year_conf_itt  td*, fe i(class_id) robust cluster(class_id)
outreg2 using itt_table3.xls, append

****** TABLE 4********
xtreg count_cl count_cl_itt  td*, fe i(class_id)  robust
outreg2 using iv_table4.xls, replace
xtreg year_conf year_conf_itt   td*, fe i(class_id) robust 
outreg2 using iv_table4.xls, append
xtivreg count_usa (count_cl= count_cl_itt) td*, fe i(class_id) 
outreg2 using iv_table4.xls, append
xtivreg count_usa (year_conf= year_conf_itt)  td*, fe i(class_id) 
outreg2 using iv_table4.xls, append

****** TABLE 5********
xi: xtreg count_usa treat count_for i.main*i.grn, fe i(class_id) robust cluster(class_id)
xi: xtreg count_usa count_cl count_for i.main*i.grn, fe i(class_id) robust cluster(class_id)
xi: xtreg count_usa year_conf count_for i.main*i.grn, fe i(class_id) robust cluster(class_id)

****** TABLE 6 *******
sort uspto_class grn
bys uspto: gen ccc=sum(count)
foreach var in count_usa count  {
	qui replace `var'=. if ccc==0 
	}
gen aaa=1 if ccc==0 & grn==1919
bys uspto: egen bbb=max(aaa)
drop if bbb==1
drop if ccc==0
drop aaa bbb ccc

xtreg count_usa treat count_for_2 td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, replace
xtreg count_usa treat count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa treat td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa count_cl count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa count_cl count_cl_2 count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa count_cl  td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa year_conf year_conf_2 count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa year_conf count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append
xtreg count_usa year_conf  td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table6.xls, append

****** TABLE 7 *******
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_primaryclassesdataset.dta", clear

forvalues x=1876/1939 {
	gen td_`x'=0
	qui replace td_`x'=1 if grn==`x'
	}

xtreg count_usa treat count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table7.xls, replace
xtreg count_usa count_cl count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table7.xls, append
xtreg count_usa year_conf count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table7.xls, append

****** TABLE 8 *******
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_indigodataset.dta", clear

forvalues x=1876/1939 {
	gen td_`x'=0
	qui replace td_`x'=1 if grn==`x'
	}

xtreg count_usa treat count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table8.xls, replace
xtreg count_usa count_cl count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table8.xls, append
xtreg count_usa year_conf count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using ols_table8.xls, append

****** TABLE 9 *******
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\dupont_data.dta", clear
xtreg patents treat_NO_dupont treat_dupont count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using table9.xls, replace
xtreg patents treat_NO_dupont treat_dupont td*, fe i(class_id) robust cluster(class_id)
outreg2 using table9.xls, append
xtreg patents count_NO_dupont count_dupont count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using table9.xls, append
xtreg patents count_NO_dupont count_dupont td*, fe i(class_id) robust cluster(class_id)
outreg2 using table9.xls, append
xtreg patents year_conf_NO_dupont year_conf_dupont count_for td*, fe i(class_id) robust cluster(class_id)
outreg2 using table9.xls, append
xtreg patents year_conf_NO_dupont year_conf_dupont td*, fe i(class_id) robust cluster(class_id)
outreg2 using table9.xls, append


**************************************************************************
****************           FIGURES          ******************************
**************************************************************************

clear
set more off
set mem 950m
********* FIGURE 1 ********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\fig1.dta", clear
reg count_ger td* if licensed_class==0, noco
reg count_ger td* if licensed_class==1, noco

reg count_ger td* if licensed_class==1, noco
estimates store treated_vals

reg count_ger td* if licensed_class==0, noco
estimates store untreated_vals

preserve
    clear
    set obs 65 
    gen year = 1874 + _n
    
    gen patents_treated = .
    gen patents_untreated = .
    
    forvalues y = 1875/1939 {
        estimates restore treated_vals
        capture replace patents_treated = _b[td`y'] if year == `y'
        if _rc != 0 capture replace patents_treated = _b[td_`y'] if year == `y'
        estimates restore untreated_vals
        capture replace patents_untreated = _b[td`y'] if year == `y'
        if _rc != 0 capture replace patents_untreated = _b[td_`y'] if year == `y'
    }

    gen patents_all = patents_treated + patents_untreated

    twoway ///
        (line patents_all year, ///
            lcolor("28 78 128") ///
            lpattern(longdash) /// 
            lwidth(medthick)) ///
        (line patents_treated year, ///
            lcolor("189 50 50") /// 
            lpattern(solid) /// 
            lwidth(medthick)) ///
        , ///
        graphregion(color(white)) bgcolor(white) /// 
        ylabel(0(100)700, angle(0) grid glcolor(gs15)) ///
        xlabel(1875(5)1939, labsize(small)) ///
        xline(1914 1918, lpattern(dot) lcolor(gs8)) /// 
        text(650 1916 "WWI", place(c) size(small) color(gs8)) /// 
        legend(order(1 "All subclasses (Total)" 2 "Treated subclasses") ///
               position(11) ring(0) region(lstyle(none) color(none))) /// 
        title("Figure 1: Collapse & Recovery of German Patents", color(black) size(medium)) ///
        subtitle("Replication of Moser & Voena (2012) with historical context", size(small) color(gs8)) ///
        ytitle("Number of US patents by German inventors") xtitle("")

restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure1_replication.pdf", replace


********* FIGURE 2 ********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta"
preserve
keep if grn==1930
browse count_cl
restore

********* RÉPLIQUE FIGURE 2 *********
preserve

keep if grn == 1919
keep if count_cl > 0

gsort -count_cl
gen rank = _n

twoway (bar count_cl rank, ///
            barwidth(1) /// 
            fcolor("28 78 128") /// 
            lcolor("28 78 128") /// 
            lwidth(none)), ///
        title("Figure 2: Distribution of Licenses per Subclass", color(black) size(medium)) ///
        subtitle("Number of enemy patents licensed to US firms (1919-1926)", size(small) color(gs8)) ///
        ytitle("Number of licensed patents") ///
        xtitle("Treated USPTO subclasses (sorted by intensity)") ///
        xlabel(none) /// 
        ylabel(0(2)16, angle(0) grid glcolor(gs15)) /// 
        graphregion(color(white)) bgcolor(white)
restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure2_replication.pdf", replace


********* FIGURE 3 ********
preserve
keep if grn==1930
browse year_conf
restore

********* RÉPLIQUE FIGURE 3 *********

preserve
keep if treat == 1
collapse (mean) year_conf, by(class_id)
gsort -year_conf
gen rank = _n

twoway (bar year_conf rank, ///
            barwidth(1) /// 
            fcolor("28 78 128") /// 
            lcolor("28 78 128") ///
            lwidth(none)), ///
 
        title("Figure 3: Remaining Patent Life per Subclass", color(black) size(medium)) ///
        subtitle("Total years of remaining life for licensed patents (1919)", size(small) color(gs8)) ///
        ytitle("Remaining lifetime (Years)") ///
        xtitle("Treated USPTO subclasses (sorted by duration)") ///
 
        xlabel(none) /// 
        ylabel(0(20)180, angle(0) grid glcolor(gs15)) /// 
        graphregion(color(white)) bgcolor(white)

restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure3_replication_replication.pdf", replace


***********FIGURE 4 *****************
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear
forvalues x=1875/1918 {
	gen td_`x'=0
	qui replace td_`x'=1 if grn==`x'
	}

foreach var in treat  {
forvalues x=1875/1918 {
	cap gen `var'_`x'=`var' if grn==`x'
	qui replace `var'_`x'=0 if grn!=`x'
	}
}
drop td_1900 treat_1900
xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

* RÉPLICATION EXACTE FIGURE 4 : 

use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta" , clear

forvalues x=1875/1939 {
	gen untreat_`x'=1 if licensed==0 & grn==`x'
	replace untreat_`x'=0 if untreat_`x'==.
	gen treat_`x'=1 if licensed==1 & grn==`x'
	replace treat_`x'=0 if treat_`x'==.
	}

drop *treat_1900

cap log close
log using pre.log, replace

xtreg count_usa treat_* untreat_*, fe i(class_id) robust cluster(class_id)

capture drop year_axis b_treat low_treat high_treat b_untreat low_untreat high_untreat
gen year_axis = .
gen b_treat = .
gen low_treat = .
gen high_treat = .
gen b_untreat = .
gen low_untreat = .
gen high_untreat = .

local i = 1
forvalues x = 1875/1918 {
    replace year_axis = `x' in `i'

    * Cas de l'année de référence (1900) : tout est à 0
    if `x' == 1900 {
        replace b_treat = 0 in `i'
        replace low_treat = 0 in `i'
        replace high_treat = 0 in `i'
        replace b_untreat = 0 in `i'
        replace low_untreat = 0 in `i'
        replace high_untreat = 0 in `i'
    }
    else {
        * --- GROUPE TRAITÉS (treat_) ---
        capture scalar coef_t = _b[treat_`x']
        capture scalar se_t = _se[treat_`x']
        if _rc == 0 {
            replace b_treat = coef_t in `i'
            replace low_treat = coef_t - 1.96*se_t in `i'
            replace high_treat = coef_t + 1.96*se_t in `i'
        }

        * --- GROUPE NON-TRAITÉS (untreat_) ---
        capture scalar coef_u = _b[untreat_`x']
        capture scalar se_u = _se[untreat_`x']
        if _rc == 0 {
            replace b_untreat = coef_u in `i'
            replace low_untreat = coef_u - 1.96*se_u in `i'
            replace high_untreat = coef_u + 1.96*se_u in `i'
        }
    }
    local i = `i' + 1
}

twoway ///
    (rarea low_untreat high_untreat year_axis if year_axis <= 1918, color(gs14)) ///
    (rarea low_treat high_treat year_axis if year_axis <= 1918, color(gs12)) ///
    (line b_untreat year_axis if year_axis <= 1918, lcolor(gs8) lpattern(dash) lwidth(medium)) ///
    (line b_treat year_axis if year_axis <= 1918, lcolor(black) lpattern(solid) lwidth(thick)), ///
    yline(0, lcolor(black) lw(thin)) ///
    xline(1900, lcolor(black) lp(dot)) ///
    legend(order(3 "Untreated (Coeff)" 4 "Treated (Coeff)" 1 "95% CI Untreated" 2 "95% CI Treated") pos(6) rows(2)) ///
    xtitle("Year") ///
    ytitle("Coefficients estimates") ///
    title("Pre-TWEA Time Trends (Figure 4)") ///
    graphregion(color(white)) bgcolor(white)
	
********* FIGURE 5 ********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\fig5.dta", clear
sum share if share>=0 & share<0.1 & licensed_class==0
sum share if share>=0.1 & share<0.2 & licensed_class==0
sum share if share>=0.2 & share<0.3 & licensed_class==0
sum share if share>=0.3 & share<0.4 & licensed_class==0
sum share if share>=0.4 & share<0.5 & licensed_class==0
sum share if share>=0.5 & share<0.6 & licensed_class==0
sum share if share>=0.6 & share<0.7 & licensed_class==0
sum share if share>=0.7 & share<0.8 & licensed_class==0
sum share if share>=0.8 & share<0.9 & licensed_class==0
sum share if share>=0.9 & share<1 & licensed_class==0
sum share if share==1 & licensed_class==0

sum share if share>=0 & share<0.1 & licensed_class==1
sum share if share>=0.1 & share<0.2 & licensed_class==1
sum share if share>=0.2 & share<0.3 & licensed_class==1
sum share if share>=0.3 & share<0.4 & licensed_class==1
sum share if share>=0.4 & share<0.5 & licensed_class==1
sum share if share>=0.5 & share<0.6 & licensed_class==1
sum share if share>=0.6 & share<0.7 & licensed_class==1
sum share if share>=0.7 & share<0.8 & licensed_class==1
sum share if share>=0.8 & share<0.9 & licensed_class==1
sum share if share>=0.9 & share<1 & licensed_class==1
sum share if share==1 & licensed_class==1

* RÉPLICATION FIGURE 5 : 

use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\fig5.dta", clear

gen share_clean = floor(share * 10 + 0.001) / 10
replace share_clean = 1 if share >= 1

histogram share_clean if licensed_class == 0, ///
    width(0.1) start(-0.05) /// 
    frequency ///
    gap(15) ///
    fcolor("28 78 128") lcolor(none) /// 
    ylabel(0(500)2000, angle(0) grid glcolor(gs15)) ///
    xlabel(0(0.1)1, labsize(small)) ///
    title("Panel A: Untreated subclasses", color(black) size(medium)) ///
    ytitle("Number of subclasses", size(small)) ///
    xtitle("Share of US inventors 1902-1918", size(small)) ///
    graphregion(color(white)) bgcolor(white) ///
    name(panelA, replace)

histogram share_clean if licensed_class == 1, ///
    width(0.1) start(-0.05) ///
    frequency ///
    gap(15) ///
    fcolor("189 50 50") lcolor(none) /// 
    ylabel(0(40)240, angle(0) grid glcolor(gs15)) ///
    xlabel(0(0.1)1, labsize(small)) ///
    title("Panel B: Treated subclasses", color(black) size(medium)) ///
    ytitle("Number of subclasses", size(small)) ///
    xtitle("Share of US inventors 1902-1918", size(small)) ///
    graphregion(color(white)) bgcolor(white) ///
    name(panelB, replace)

graph combine panelA panelB, ///
    cols(2) xsize(12) ysize(6) ///
    graphregion(color(white)) ///
    title("Figure 5: Pre-TWEA Shares of Domestic Inventors", color(black)) ///
    subtitle("Replication Moser & Voena (2012)", size(small))

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure5_replication.pdf", replace

*******************************************Figure 6*************************************

use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear

capture drop treat_* td_*
forvalues x=1875/1939 {
    gen td_`x' = 0
    qui replace td_`x' = 1 if grntyr == `x'
}

local treat_var "licensed_class" 
forvalues x=1919/1939 {
    gen treat_`x' = `treat_var' * td_`x'
}

xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

preserve
    clear
    set obs 21 // 
    gen year = 1918 + _n
    
    gen coef = .
    gen ci_low = .
    gen ci_up = .

    forvalues y = 1919/1939 {
        capture lincom treat_`y'
        if _rc == 0 {
            replace coef = r(estimate) if year == `y'
            replace ci_low = r(lb) if year == `y'
            replace ci_up = r(ub) if year == `y'
        }
    }
    twoway ///
        (line coef year, ///
            lcolor("189 50 50")) ///  
        (line ci_low year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)) ///
        (line ci_up year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)), ///
        yline(0, lcolor(black) lwidth(thin)) /// 
        xlabel(1919(5)1939) ///
        ylabel(, angle(0) grid glcolor(gs15)) ///
        title("Figure 6: Annual Treatment Effects", color(black) size(medium)) ///
        subtitle("Impact of compulsory licensing (Binary treatment)", size(small) color(gs8)) ///
        ytitle("Increase in patents (Treatment effect)") xtitle("") ///
        legend(off) ///
        graphregion(color(white)) bgcolor(white)
		
restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure6_replication.pdf", replace

******************************************************************************************

********* FIGURE 7 ********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear
forvalues x=1876/1939 {
	gen td_`x'=0
	qui replace td_`x'=1 if grn==`x'
	}

foreach var in treat count_cl year_conf {
forvalues x=1919/1939 {
	cap gen `var'_`x'=`var' if grn==`x'
	qui replace `var'_`x'=0 if grn!=`x'
	}
}
xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

************REPLICATION FIGURE 7****************************

capture drop treat_* td_*
forvalues x=1875/1939 {
    gen td_`x' = 0
    qui replace td_`x' = 1 if grntyr == `x'
}

forvalues x=1919/1939 {
    gen treat_`x' = count_cl * td_`x'
}

xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

preserve
    clear
    set obs 21 // 
    gen year = 1918 + _n
    
    gen coef = .
    gen ci_low = .
    gen ci_up = .

    forvalues y = 1919/1939 {
        capture lincom treat_`y'
        if _rc == 0 {
            replace coef = r(estimate) if year == `y'
            replace ci_low = r(lb) if year == `y'
            replace ci_up = r(ub) if year == `y'
        }
    }
    twoway ///
        (line coef year, ///
            lcolor("189 50 50") ///
            lwidth(thick)) ///
        (line ci_low year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)) ///
        (line ci_up year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)), ///
        yline(0, lcolor(black) lwidth(thin)) /// 
        ylabel(, angle(0) grid glcolor(gs15)) ///
        xlabel(1919(5)1939) ///
        title("Figure 7: Marginal Effect of Licensing", color(black) size(medium)) ///
        subtitle("Annual increase in patents per additional license", size(small) color(gs8)) ///
        ytitle("Marginal effect (Coef)") xtitle("") ///
        legend(off) ///
        graphregion(color(white)) bgcolor(white)
		
restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure7_Replication.pdf", replace


********* FIGURE 8 ********
xtreg count_usa count_cl_* count_for td*, fe i(class_id) robust cluster(class_id)

*******************REPLICATION FIGURE 8**************************

local life_var "year_conf" 
capture confirm variable `life_var'
if _rc != 0 {
    display as error "Attention : la variable 'year_conf' n'est pas trouvée. Vérifiez son nom avec 'lookfor year' ou 'lookfor life'."
    exit
}

capture drop treat_* td_*
forvalues x=1875/1939 {
    gen td_`x' = 0
    qui replace td_`x' = 1 if grntyr == `x'
}

forvalues x=1919/1939 {
    gen treat_`x' = `life_var' * td_`x'
}

xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

preserve
    clear
    set obs 21 
    gen year = 1918 + _n
    
    gen coef = .
    gen ci_low = .
    gen ci_up = .

    forvalues y = 1919/1939 {
        capture lincom treat_`y'
        if _rc == 0 {
            replace coef = r(estimate) if year == `y'
            replace ci_low = r(lb) if year == `y'
            replace ci_up = r(ub) if year == `y'
        }
    }
    twoway ///
        (line coef year, ///
            lcolor("189 50 50") /// 
            lwidth(thick)) ///
        (line ci_low year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)) ///
        (line ci_up year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)), ///
        yline(0, lcolor(black) lwidth(thin)) /// 
        ylabel(, angle(0) grid glcolor(gs15)) ///
        xlabel(1919(5)1939) ///
        title("Figure 8: Effect of Patent Duration", color(black) size(medium)) ///
        subtitle("Marginal effect of 1 additional year of remaining patent life", size(small) color(gs8)) ///
        ytitle("Marginal effect (Coef)") xtitle("") ///
        legend(off) ///
        graphregion(color(white)) bgcolor(white)
restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure8_replication.pdf", replace


********* FIGURE 9 ********
xtreg count_usa year_conf_* count_for td*, fe i(class_id) robust cluster(class_id)

*******************REPLICATION FIGURE 9**********************

expand 2, gen(is_usa)
gen patents = .
replace patents = count_usa if is_usa == 1
replace patents = count_for_noger if is_usa == 0
local treat_var "licensed_class"
gen usa_x_treat = is_usa * `treat_var'

forvalues x=1919/1939 {
    gen td_`x' = (grntyr == `x')
    gen usa_td_`x' = is_usa * td_`x'
    gen treat_td_`x' = `treat_var' * td_`x
    gen ddd_`x' = is_usa * `treat_var' * td_`x'
}

areg patents is_usa usa_x_treat ddd_* usa_td_* treat_td_* td_*, absorb(class_id) cluster(class_id)

preserve
    clear
    set obs 21
    gen year = 1918 + _n
    gen coef = .
    gen ci_low = .
    gen ci_up = .

    forvalues y = 1919/1939 {
        capture lincom ddd_`y'
        if _rc == 0 {
            replace coef = r(estimate) if year == `y'
            replace ci_low = r(lb) if year == `y'
            replace ci_up = r(ub) if year == `y'
        }
    }
    twoway ///
        (line coef year, ///
            lcolor("189 50 50") /// 
            lwidth(thick)) ///
        (line ci_low year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)) ///
        (line ci_up year, ///
            lcolor("189 50 50") ///
            lpattern(dash) lwidth(thin)), ///
        yline(0, lcolor(black) lwidth(thin)) /// 
        ylabel(, angle(0) grid glcolor(gs15)) /// 
        xlabel(1919(5)1939) ///
        title("Figure 9: Triple Difference Estimates", color(black) size(medium)) ///
        subtitle("Differential gain for US inventors vs. Non-German inventors", size(small) color(gs8)) ///
        ytitle("DDD Estimate (US surplus)") xtitle("") ///
        legend(off) ///
        graphregion(color(white)) bgcolor(white)
		
restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure9_replication.pdf", replace

********* FIGURE 10 

use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\fig10.dta", clear

xtreg y usa_treat_td1919-usa_treat_td1939 usa_td* usa_treat treat_td* usa td_*, fe i(class_id) robust cluster(class_id)

capture drop year_axis b_untreat low_untreat high_untreat b_treat low_treat high_treat
gen year_axis = .
gen b_untreat = .
gen low_untreat = .
gen high_untreat = .
gen b_treat = .
gen low_treat = .
gen high_treat = .

local i = 1

forvalues y = 1919/1939 {
    replace year_axis = `y' in `i'
    capture lincom td_`y'
    if _rc == 0 {
        replace b_untreat = r(estimate) in `i'   
        replace low_untreat = r(lb) in `i'       
        replace high_untreat = r(ub) in `i'      
    }
    capture lincom td_`y' + treat_`y'
    if _rc == 0 {
        replace b_treat = r(estimate) in `i'
        replace low_treat = r(lb) in `i'
        replace high_treat = r(ub) in `i'
    }
    
    local i = `i' + 1
}

twoway ///
    (rarea low_untreat high_untreat year_axis, color(gs14)) ///
    (rarea low_treat high_treat year_axis, color(gs12%50)) ///
    (line b_untreat year_axis, lcolor(gs8) lpattern(dash) lwidth(medium)) ///
    (line b_treat year_axis, lcolor(black) lpattern(solid) lwidth(medium)), ///
    yline(0, lcolor(black) lw(thin)) ///
    legend(order(3 "Untreated (Placebo)" 4 "Treated (Placebo)" 1 "95% CI" 2 "95% CI") pos(6) rows(2)) ///
    xtitle("Year") ///
    ytitle("Coefficients estimates") ///
    title("Figure 10: Placebo Test Results") ///
    graphregion(color(white)) bgcolor(white)
	
*********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear

********* FIGURE 11 ********
gen trend=licensed_class*grn
xi: xtreg count_usa treat_* td* trend count_for, fe i(class_id) robust cluster(class_id)

capture drop year_axis b_untreat b_treat
gen year_axis = .
gen b_untreat = .
gen b_treat = .

local i = 1
forvalues y = 1915/1939 {
    replace year_axis = `y' in `i'
    scalar coef_td = 0
    capture scalar coef_td = _b[td_`y']
    replace b_untreat = coef_td in `i'
    scalar coef_treat = 0
    capture scalar coef_treat = _b[treat_`y']
    
    replace b_treat = coef_td + coef_treat in `i'
    
    local i = `i' + 1
}

twoway ///
    (line b_untreat year_axis if year_axis >= 1915 & year_axis <= 1939, ///
        lcolor(gs10) lpattern(dash) lwidth(medium)) ///
    (line b_treat year_axis if year_axis >= 1915 & year_axis <= 1939, ///
        lcolor(black) lpattern(solid) lwidth(thick)), ///
    yline(0, lcolor(black) lw(thin)) ///
    xline(1919, lcolor(black) lp(dot)) ///
    legend(order(1 "Untreated subclasses" 2 "Treated subclasses") pos(6)) ///
    xtitle("Year") ///
    ytitle("Coefficients for year dummies") ///
    title("Figure 11: Annual Treatment Effects") ///
    subtitle("Controlling for Linear Time Trends") ///
    graphregion(color(white)) bgcolor(white)
restore

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Placebo_Replication.pdf", replace

********* FIGURE 12 ********
*********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_indigodataset.dta", clear

xi: xtreg count_usa treat_* td* i.class_id*grn, fe i(class_id) robust cluster(class_id)

********************************************************************************
* CORRECTION ERREUR "REPEATED TIME VALUES" - FIGURE 12 (INDIGO)
********************************************************************************

* 1. CRÉATION DE L'ID UNIQUE DE SOUS-CLASSE
* On utilise 'uspto_class' (ex: 008/094...) pour créer un ID numérique unique
* Si 'subcl' existe, c'est pareil, mais 'uspto_class' est sûr d'après votre image.
capture drop sub_id
egen sub_id = group(uspto_class)

* 2. DÉCLARATION DU PANEL (C'est là que ça bloquait)
xtset sub_id grntyr

* 3. RÉGRESSION (Avec sub_id comme variable de cluster/FE)
* Note : On utilise count_for (brevets étrangers) comme contrôle si dispo
xtreg count_usa treat_1919-treat_1939 td_* count_for, fe i(sub_id) cluster(sub_id) robust

* 4. EXTRACTION ET GRAPHIQUE
preserve
    clear
    set obs 21
    gen year = 1918 + _n
    gen coef = .
    gen ci_low = .
    gen ci_up = .

    forvalues y = 1919/1939 {
        capture lincom treat_`y'
        if _rc == 0 {
            replace coef = r(estimate) if year == `y'
            replace ci_low = r(lb) if year == `y'
            replace ci_up = r(ub) if year == `y'
        }
    }

    twoway ///
        (rcap ci_up ci_low year, lcolor(black) lpattern(dash)) ///
        (line coef year, lcolor(black) lwidth(thick)) ///
        , ///
        yline(0, lcolor(black) lwidth(thin)) ///
        ylabel(-0.1(0.05)0.3, angle(0) grid) ///
        xlabel(1919(5)1939) ///
        title("Figure 12: Indigo Patents", color(black)) ///
        ytitle("Treatment Effect") xtitle("") ///
        legend(off) graphregion(color(white)) bgcolor(white)
        
    graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure12_replication.pdf", replace
restore
****** FIGURE  13 *******
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\dupont_data.dta", clear

xtreg patents treat_NO_dupont_???? treat_dupont_???? td*, fe i(class_id) robust cluster(class_id)

capture drop year_axis b_dupont low_dupont high_dupont b_other low_other high_other
gen year_axis = .
gen b_dupont = .
gen low_dupont = .
gen high_dupont = .
gen b_other = .
gen low_other = .
gen high_other = .

local i = 1

forvalues y = 1919/1939 {
    replace year_axis = `y' in `i'
    capture scalar coef_dp = _b[treat_dupont_`y']
    capture scalar se_dp = _se[treat_dupont_`y']
    
    if _rc == 0 {
        replace b_dupont = coef_dp in `i'
        replace low_dupont = coef_dp - 1.96 * se_dp in `i'
        replace high_dupont = coef_dp + 1.96 * se_dp in `i'
    }
    capture scalar coef_no = _b[treat_NO_dupont_`y']
    capture scalar se_no = _se[treat_NO_dupont_`y']
    
    if _rc == 0 {
        replace b_other = coef_no in `i'
        replace low_other = coef_no - 1.96 * se_no in `i'
        replace high_other = coef_no + 1.96 * se_no in `i'
    }
    
    local i = `i' + 1
}
twoway ///
    (rarea low_other high_other year_axis, color(gs14%50) lw(none)) ///
    (rarea low_dupont high_dupont year_axis, color(gs12%50) lw(none)) ///
    (line b_other year_axis, lcolor(gs8) lpattern(dash) lwidth(medium)) ///
    (line b_dupont year_axis, lcolor(black) lpattern(solid) lwidth(thick)), ///
    yline(0, lcolor(black) lw(thin)) ///
    legend(order(4 "Du Pont subclasses" 3 "Non Du Pont subclasses" 2 "95% CI (Du Pont)" 1 "95% CI (Others)") pos(6) rows(2)) ///
    xtitle("Year") ///
    ytitle("Annual treatment effect") ///
    title("Figure 13: Year-Specific Treatment Effects (Du Pont)") ///
    graphregion(color(white)) bgcolor(white)
	

*******************ADDITIONAL ANALYSIS*************************

* --- PRÉPARATION ---
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear

* Création des interactions (tout le monde vs traités)
* On nettoie d'abord pour éviter les erreurs
capture drop td_* treat_* trend
forvalues x = 1876/1939 {
    gen td_`x' = (grn == `x')
    gen treat_`x' = td_`x' * treat
}
* Variable de tendance linéaire spécifique aux traités

* --- RÉGRESSION FIG 11 ---
* On omet 1900 comme base pour éviter la colinéarité si nécessaire, ou on laisse Stata gérer.
xtreg count_usa treat_* td_* trend count_for, fe i(class_id) robust cluster(class_id)

* --- CONSTRUCTION DU GRAPHIQUE ---
capture drop y_untreated y_treated year_axis
gen year_axis = .
gen y_untreated = .
gen y_treated = .

local i = 1
forvalues y = 1915/1939 {
    replace year_axis = `y' in `i'
    
    * 1. Courbe Non-Traités (Base) : Coefficient td_année
    scalar b_base = 0
    capture scalar b_base = _b[td_`y']
    replace y_untreated = b_base in `i'
    
    * 2. Courbe Traités : Base + Effet Annuel (SANS LA TENDANCE)
    scalar b_diff = 0
    capture scalar b_diff = _b[treat_`y']
    
    * C'est ici l'astuce : on additionne la base et le choc, mais on ignore 'trend'
    replace y_treated = b_base + b_diff in `i'
    
    local i = `i' + 1
}

* --- TRACÉ ---
twoway (line y_untreated year_axis if year_axis>=1915 & year_axis<=1939, lcolor(gs10) lpattern(dash)) ///
       (line y_treated year_axis if year_axis>=1915 & year_axis<=1939, lcolor(black) lwidth(thick)), ///
       xline(1919, lcolor(black) lp(dot)) yline(0) ///
       legend(order(1 "Untreated" 2 "Treated") pos(6)) ///
       title("Figure 11 Replication") subtitle("Net of Linear Trends") ///
       graphregion(color(white))
	   
restore
	   
graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\first_additional_analysis.dta", replace
	   
*********************other**************************


use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta", clear

* On nettoie tout pour être sûr
capture drop td_* treat_* trend ger_prewar total_ger_pre high_ger_influence

* 2. CRÉATION DES VARIABLES TEMPORELLES (1876-1939)
forvalues x = 1876/1939 {
    * Dummy Année (td)
    gen td_`x' = (grn == `x')
    
    * Interaction Traitement (treat)
    gen treat_`x' = td_`x' * treat
}

* Variable de tendance linéaire (indispensable pour cette spécification)

* 3. CRÉATION DE LA VARIABLE DE DISTINCTION (High vs Low German Stock)
* On compte les brevets allemands avant-guerre (1900-1914)
gen ger_prewar = count_germany if grn >= 1900 & grn <= 1914
replace ger_prewar = 0 if ger_prewar == .

* On somme par classe
bysort class_id: egen total_ger_pre = total(ger_prewar)

* On coupe l'échantillon à la médiane
quietly summarize total_ger_pre, detail
gen high_ger_influence = (total_ger_pre > r(p50))
* (Optionnel : vous pouvez essayer > 0 pour voir les classes avec au moins 1 brevet allemand)

* 4. PRÉPARATION DU GRAPHIQUE
capture drop year_ax b_high b_low
gen year_ax = .
gen b_high = .
gen b_low = .

* 5. RÉGRESSION 1 : FORTE INFLUENCE ALLEMANDE
* On exclut 1900 (base) pour éviter la colinéarité
capture drop td_1900 treat_1900
quietly xtreg count_usa treat_* td_* trend count_for if high_ger_influence == 1, fe i(class_id) robust cluster(class_id)

local i = 1
forvalues y = 1915/1939 {
    replace year_ax = `y' in `i'
    scalar b_td = 0
    capture scalar b_td = _b[td_`y']
    scalar b_tr = 0
    capture scalar b_tr = _b[treat_`y']
    replace b_high = b_td + b_tr in `i'
    local i = `i' + 1
}

* 6. RÉGRESSION 2 : FAIBLE INFLUENCE ALLEMANDE
quietly xtreg count_usa treat_* td_* trend count_for if high_ger_influence == 0, fe i(class_id) robust cluster(class_id)

local i = 1
forvalues y = 1915/1939 {
    scalar b_td = 0
    capture scalar b_td = _b[td_`y']
    scalar b_tr = 0
    capture scalar b_tr = _b[treat_`y']
    replace b_low = b_td + b_tr in `i'
    local i = `i' + 1
}

twoway (line b_low year_ax if year_ax >= 1915 & year_ax <= 1939, ///
        lcolor(gs10) lpattern(dash) lwidth(medium)) ///
       (line b_high year_ax if year_ax >= 1915 & year_ax <= 1939, ///
        lcolor(navy) lpattern(solid) lwidth(thick)), ///
       yline(0, lcolor(black) lw(thin)) ///
       xline(1919, lcolor(black) lp(dot)) ///
       legend(order(2 "High Pre-War German Patents" 1 "Low Pre-War German Patents") pos(6)) ///
       xtitle("Year") ytitle("Treatment Effect (Net of Trends)") ///
       title("Original Analysis: The Role of Knowledge Stock") ///
       subtitle("Impact depends on pre-existing German patents") ///
       graphregion(color(white)) bgcolor(white)
	   
graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\second_aditional_analysis.pdf"