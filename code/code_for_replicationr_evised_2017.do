**************************************************************************
****************           TABLES          *******************************
**************************************************************************

clear all
set more off
set mem 2g
set maxvar 20000
set matsize 11000

****** TABLE 1********
use "table1.dta"
****Col 1
tab true_nat
****Col 2
codebook patnum if usa==1
codebook patnum if germany==1
codebook patnum if usa==0 & germany==0

****** TABLE 2********
use "chem_patents_maindataset.dta"

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
xi: xtreg count_usa year_conf count_for i.main*i.grn, fe i(class_id) robust cluster(class_id)*/

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
use "chem_patents_primaryclassesdataset.dta", clear

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
use "chem_patents_indigodataset.dta", clear

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
use "dupont_data.dta", clear
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
use "fig1.dta", clear
reg count_ger td* if licensed_class==0, noco
reg count_ger td* if licensed_class==1, noco

********* FIGURE 2 ********
use "chem_patents_maindataset.dta", clear
preserve
keep if grn==1930
browse count_cl
restore
********* FIGURE 3 ********
preserve
keep if grn==1930
browse year_conf
restore

***********FIGURE 4 *****************

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
use "fig5.dta", clear
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


********* FIGURE 6 and 7 ********
use "chem_patents_maindataset.dta", clear
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

********* FIGURE 6 ********
xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

********* FIGURE 7 ********
xtreg count_usa count_cl_1919-count_cl_1939 count_for td*, fe i(class_id) robust cluster(class_id)

********* FIGURE 8 ********
xtreg count_usa year_conf_1919-year_conf_1939 count_for td*, fe i(class_id) robust cluster(class_id)

********* FIGURE 9 ********
use "fig10.dta", clear
xtreg y usa_treat_td1919-usa_treat_td1939 usa_td* usa_treat treat_td* usa td_*, fe i(class_id) robust cluster(class_id)

*********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_maindataset.dta" , clear

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

********* FIGURE 10 ********
xtreg count_france treat_* td*, fe i(class_id) robust cluster(class_id)

* --- ETAPE 1 : Préparer des variables vides pour le graphique ---
* On supprime les anciennes versions si elles existent pour éviter les erreurs
capture drop year_axis b_untreat low_untreat high_untreat b_treat low_treat high_treat
gen year_axis = .
gen b_untreat = .
gen low_untreat = .
gen high_untreat = .
gen b_treat = .
gen low_treat = .
gen high_treat = .

* --- ETAPE 2 : La boucle magique qui récupère vos résultats ---
local i = 1

* On parcourt les années de votre régression (1919 à 1939)
forvalues y = 1919/1939 {
    replace year_axis = `y' in `i'
    
    * A. RÉCUPÉRER LA COURBE GRISE (Non-Traités)
    * On demande à Stata : "Donne-moi le coef et l'IC de la variable td_`y'"
    * capture permet de ne pas planter si une année a été omise par la régression
    capture lincom td_`y'
    if _rc == 0 {
        replace b_untreat = r(estimate) in `i'   // Le coefficient
        replace low_untreat = r(lb) in `i'       // Borne basse de l'IC
        replace high_untreat = r(ub) in `i'      // Borne haute de l'IC
    }
    
    * B. RÉCUPÉRER LA COURBE NOIRE (Traités Placebo)
    * On demande : "Calcule la somme td_`y' + treat_`y' et son IC"
    capture lincom td_`y' + treat_`y'
    if _rc == 0 {
        replace b_treat = r(estimate) in `i'
        replace low_treat = r(lb) in `i'
        replace high_treat = r(ub) in `i'
    }
    
    local i = `i' + 1
}

* --- ETAPE 3 : Tracer le graphique avec ces nouvelles variables ---
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

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure10_replication.pdf", replace

	
********* FIGURE 11 ********
gen trend=licensed_class*grn
xi: xtreg count_usa treat_* td* trend count_for, fe i(class_id) robust cluster(class_id)

* 1. Préparation des variables de stockage
capture drop year_axis b_untreat b_treat
gen year_axis = .
gen b_untreat = .
gen b_treat = .

local i = 1

* 2. Boucle pour récupérer les coefficients (1915-1939 pour le zoom habituel)
* Note : On calcule pour toutes les années disponibles dans la régression
forvalues y = 1915/1939 {
    replace year_axis = `y' in `i'
    
    * --- A. COURBE UNTREATED (Non-Traités) ---
    * C'est simplement le coefficient de la dummy année (td_y)
    scalar coef_td = 0
    capture scalar coef_td = _b[td_`y']
    replace b_untreat = coef_td in `i'
    
    * --- B. COURBE TREATED (Traités) ---
    * C'est la somme : Dummy Année (td_y) + Interaction (treat_y)
    * IMPORTANT : On N'AJOUTE PAS la variable 'trend' ici. 
    * On veut voir l'effet du traitement "net" de la tendance linéaire.
    scalar coef_treat = 0
    capture scalar coef_treat = _b[treat_`y']
    
    replace b_treat = coef_td + coef_treat in `i'
    
    local i = `i' + 1
}

* 3. Tracer le graphique
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
	

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure11_replication.pdf", replace


******** FIGURE 12 ********
*********
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_indigodataset.dta", clear

forvalues x=1876/1939 {
	gen td_`x'=0
	replace td_`x'=1 if grn==`x'
	}

foreach var in treat {
forvalues x=1919/1939 {
	cap gen `var'_`x'=`var' if grn==`x'
	replace `var'_`x'=0 if grn!=`x'
	}
}
xtreg count_us treat_* count_for td*, fe i(class_id) robust cluster(class_id)

********************************************************************************
* RÉPLICATION FIGURE 12 : DIAGNOSTIC ET AFFICHAGE
********************************************************************************

* 1. CHARGEMENT
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\chem_patents_indigodataset.dta", clear

* 2. NETTOYAGE PRÉALABLE (Pour éviter "variable already defined")
capture drop td_*
capture drop treat_*

* 3. CRÉATION DES VARIABLES
* Vérification du nom de la variable année
capture confirm variable grn
if _rc != 0 {
    rename grntyr grn
}

* Dummies Années
forvalues x=1876/1939 {
    gen td_`x'=0
    replace td_`x'=1 if grn==`x'
}

* Interactions Traitement
foreach var in treat {
    forvalues x=1919/1939 {
        gen `var'_`x' = 0
        replace `var'_`x' = `var' if grn==`x'
    }
}

* 4. RÉGRESSION
display "Lancement de la régression..."
xtreg count_usa treat_* count_for td*, fe i(class_id) robust cluster(class_id)

* 5. EXTRACTION ET DIAGNOSTIC
preserve
    clear
    set obs 21 // 1919 à 1939
    gen year = 1918 + _n
    
    gen coef = .
    gen ci_low = .
    gen ci_up = .

    display "Extraction des coefficients..."
    forvalues y = 1919/1939 {
        * On capture l'erreur pour voir si lincom marche
        capture lincom treat_`y'
        if _rc == 0 {
            replace coef = r(estimate) if year == `y'
            replace ci_low = r(lb) if year == `y'
            replace ci_up = r(ub) if year == `y'
        }
        else {
            display "ERREUR: Coefficient treat_`y' non trouvé !"
        }
    }

    * --- ÉTAPE DE VÉRIFICATION ---
    * Regardez la fenêtre de résultats : si cette liste est vide ou pleine de points (.), c'est que lincom échoue.
    list year coef ci_low ci_up in 1/21

    * 6. TRACÉ DU GRAPHIQUE (Sans export immédiat pour affichage)
    twoway ///
        (rcap ci_up ci_low year, lcolor(black) lpattern(dash)) /// 
        (line coef year, lcolor(black) lwidth(thick)) /// 
        , ///
        yline(0, lcolor(black) lwidth(thin)) /// 
        ylabel(-0.1(0.05)0.3, angle(0) grid glcolor(gs15)) ///
        xlabel(1919(5)1939) ///
        title("Figure 12: Indigo Patents", color(black)) ///
        ytitle("Treatment Effect") xtitle("") ///
        legend(off) ///
        graphregion(color(white)) bgcolor(white) name(Fig12_Debug, replace)

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure12_replication.pdf", replace
		
restore

****** FIGURE  13 *******
use "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\sources\dupont_data.dta", clear
xtreg patents treat_NO_dupont_???? treat_dupont_???? td*, fe i(class_id) robust cluster(class_id)

* 1. Préparation des variables de stockage
capture drop year_axis b_dupont low_dupont high_dupont b_other low_other high_other
gen year_axis = .
* Pour Du Pont (Ligne noire épaisse sur le graphe original)
gen b_dupont = .
gen low_dupont = .
gen high_dupont = .
* Pour les Autres firmes (Ligne grise/fine sur le graphe original)
gen b_other = .
gen low_other = .
gen high_other = .

local i = 1

* 2. Boucle pour extraire les coefficients (1919-1939)
forvalues y = 1919/1939 {
    replace year_axis = `y' in `i'
    
    * --- A. EFFET DU PONT (treat_dupont_*) ---
    * On récupère le coefficient et l'erreur standard directement
    capture scalar coef_dp = _b[treat_dupont_`y']
    capture scalar se_dp = _se[treat_dupont_`y']
    
    if _rc == 0 {
        replace b_dupont = coef_dp in `i'
        replace low_dupont = coef_dp - 1.96 * se_dp in `i'
        replace high_dupont = coef_dp + 1.96 * se_dp in `i'
    }
    
    * --- B. EFFET AUTRES FIRMES (treat_NO_dupont_*) ---
    * Attention au nom exact de la variable dans votre base (NO_dupont vs nodupont)
    capture scalar coef_no = _b[treat_NO_dupont_`y']
    capture scalar se_no = _se[treat_NO_dupont_`y']
    
    if _rc == 0 {
        replace b_other = coef_no in `i'
        replace low_other = coef_no - 1.96 * se_no in `i'
        replace high_other = coef_no + 1.96 * se_no in `i'
    }
    
    local i = `i' + 1
}

* 3. Tracer le graphique (Réplication Figure 13)
* Du Pont en Noir (Solide), Autres en Gris/Pointillés
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
	

graph export "C:\Users\lucas\OneDrive\Desktop\COURS M2 DADEE\S1\ECONOMETRIE EVALUATION D'IMPACT\projet\data\final\Figure13_replication_replication.pdf", replace

	
	
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
gen trend = treat * grn

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
gen trend = treat * grn

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
    * On récupère l'effet net (sans la trend)
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

* 7. LE GRAPHIQUE FINAL
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