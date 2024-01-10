
*********************************
*
* 	Preferences and strategic behavior 
*
*   Created: //2016
*	Updated: 26/10/2016
*
*********************************



*ability across treatments
clear
capture log close
log using "C:\Users\marco\Dropbox\Research\Grandjean_Lefebvre_Mantovani\Experiment\Analysis\GLManalysis.log",replace
use "C:\Users\marco\Dropbox\Research\Grandjean_Lefebvre_Mantovani\Experiment\Analysis\GLMdataset.dta"

*use "/Users/mathieulefebvre/Dropbox/Grandjean_Lefebvre_Mantovani (1)/Experiment/Analysis/GLMdataset.dta", clear

/*
Treat = 0 stands for RAND, 1 for PREF and 2 for STRAT
cont_reppgg is the individual contributions in the repeated game
Type = 0 stands for FR, 1 for UC, 2 for CC and 3 for others
High = 1 if ability is higher than the median
ability is the average score in the three ability scores
*/
g raceability = bilevel*20
g crtability = crt*(100/3)
xtset id period
g laggroupcont=L.groupcont
g lagothercont=L.repothercont/2
g relativecut = (cont)/laggroupcont
cumul crtab, gen(cumcrtability) equal
gen crtHigh = 1 if cumcrtability>.5
replace crtHigh =0 if cumcrtability<=.5
cumul raceab, gen(cumraceability) equal
gen raceHigh = 1 if cumraceability>.5
replace raceHigh =0 if cumraceability<=.5
cumul guessab, gen(cumguessability) equal
gen guessHigh = 1 if cumguessability>.5
replace guessHigh =0 if cumguessability<=.5

g bi3=0
replace bi3=1 if bilevel>1
replace bi3=2 if bilevel>2
replace bi3=3 if bilevel>3

g perc_cont= 100*cont_ /20
g perc_unco=100*unco_cont/200
g dif_cont2=perc_cont-perc_unco

******************************
*
* CLASSIFICATION
*
******************************

*TABLE 1
tab type treat if period==1, col  
table treat if period==1,contents (m unco m avgsm )
table treat if period==1,contents (m crtability m raceab m guessability m ability sum High)

*Tests reported around the presentation of Table 1

prtest CC if treat<2 & period==1, by(treat)
prtest CC if treat!=1 & period==1, by(treat)
prtest CC if treat>0 & period==1, by(treat)
prtest FR if treat<2 & period==1, by(treat)
prtest FR if treat!=1 & period==1, by(treat)
prtest FR if treat>0 & period==1, by(treat)
ranksum unco if treat<2 & period==1, by(treat)
ranksum unco if treat!=1 & period==1, by(treat)
ranksum unco if treat>0 & period==1, by(treat)
ranksum avgsm if treat<2 & period==1, by(treat)
ranksum avgsm if treat!=1 & period==1, by(treat)
ranksum avgsm if treat>0 & period==1, by(treat)
prtest High if treat<2 & period==1, by(treat)
prtest High if treat!=1 & period==1, by(treat)
prtest High if treat>0 & period==1, by(treat)
ranksum ability if treat<2 & period==1, by(treat)
ranksum ability if treat!=1 & period==1, by(treat)
ranksum ability if treat>0 & period==1, by(treat)



*TABLE 2
table type if period==1 , contents(m ability sum High m crtability m raceab m guessability)
pwcorr crtab raceab guessab unco_cont avgsm  choiceguess if period==1, sig
alpha crtab raceab guessab
alpha crtab raceab 

*Tests reported around the presentation of Table 2
prtest High if (type==0 | type ==2) & period==1, by(type)
ranksum ability if (type==0 | type ==2) & period==1, by(type)

kwallis crtscore if period==1, by(type)
ranksum crtscore if period==1 & (type==0|type==2), by(type)
ranksum crtscore if period==1 & (type==0|type==3), by(type)
ranksum crtscore if period==1 & (type==2|type==3), by(type)
kwallis bilevel if period==1, by(type)
ranksum bilevel if period==1 & (type==0|type==2), by(type)
ranksum bilevel if period==1 & (type==0|type==3), by(type)
ranksum bilevel if period==1 & (type==2|type==3), by(type)
kwallis guessab if period==1, by(type)
ranksum guessab if period==1 & (type==0|type==2), by(type)
ranksum guessab if period==1 & (type==0|type==3), by(type)
ranksum guessab if period==1 & (type==2|type==3), by(type)

table type High, contents(m risk)
kwallis risk if period==1,by(type)
ranksum risk if period==1,by(High)

************************************************************************** 
*Correlations top vs bottom 30
**************************************************************************
preserve
drop if decile>3 & decile<8
table type if period==1 , contents(m ability sum High m crtability m raceab m guessability)
pwcorr crtab raceab guessab unco_cont avgsm  choiceguess if period==1, sig
alpha crtab raceab guessab
alpha crtab raceab 
restore

**************************************
*FIGURE 1
***********************************

preserve
keep if period  ==1
reshape long cond, i(id ) j(other_var)
replace other_var = 0 if other_var == 1
replace other_var = 90 if other_var == 10
replace other_var = 190 if other_var == 20
replace other_var = 200 if other_var == 21
forvalues i=2/9 {
replace other_var = `i'*10 - 10 if other_var == `i'
}
forvalues i=11/19 {
replace other_var = `i'*10 - 10 if other_var == `i'
}

bysort type other_var: egen cond_contr = mean(cond)
bysort type other_var allCC allH: egen cond_group= mean(cond)

twoway (connected cond_cont other_var if  type == 0, sort msymbol(X) lcolor(gs6) mcolor(gs6)) /*
*/(connected cond_cont other_var if  type == 1, sort msymbol(triangle) lcolor(gs10) mcolor(gs10))/*
*/ (connected cond_cont other_var if  type == 2, sort  msymbol(circle) lcolor(gs2) mcolor(gs2)) /*
*/, ytitle(Conditional contribution) xtitle(Others contributions)legend(label(1 FR)label(2 UC)label(3 CC)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) 


************************************
*FIGURE A3: distribution of ability
*************************************

histogram ability, fcolor(gs11) lcolor(black) frac kdensity addplot(pci 0 43.5 .1 43.5, lcolor(black)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))


*******************************************************
*HYPOTHESIS 1*
*******************************************************

*************
**FIGURE 2***
******************
preserve
expand 2
replace type=4 in 2881/L 
drop if period>1
graph bar dif_cont if type==0|type==2|type==4, over(High ) asyvars over(type) bar(1,fcolor(gs3) lcolor(black)) bar(2,fcolor(gs11) lcolor(black)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) ytitle(1st period - unconditional contribution) 
restore

*******************************
**FIGURE 2 top vs bottom 30***
*******************************
preserve
expand 2
replace type=4 in 2881/L 
drop if period>1
drop if decile>3 & decile<8
graph bar dif_cont if type==0|type==2|type==4, over(High ) asyvars over(type) bar(1,fcolor(gs3) lcolor(black)) bar(2,fcolor(gs11) lcolor(black)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) ytitle(1st period - unconditional contribution) 
restore


*statistics presented around Figure 2 and TABLE A.1: TESTS OF H1
preserve 
drop if period>1
sum  perc_unco  perc_cont dif_cont2 
table High  , contents(m perc_unco m perc_cont m dif_cont2)
table type   , contents(m perc_unco m perc_cont m dif_cont2)
ranksum dif_cont, by(High)
ranksum unco_cont, by(High)
ranksum cont_rep, by(High)
kwallis dif_cont , by(type)
restore

* TOP VS BOTTOM 30*
preserve 
drop if decile>3 & decile<8
drop if period>1
sum  perc_unco  perc_cont dif_cont2 
table type High  , contents(m perc_unco m perc_cont m dif_cont2)
table High  , contents(m perc_unco m perc_cont m dif_cont2)
table type   , contents(m perc_unco m perc_cont m dif_cont2)
ranksum dif_cont2, by(High)
ranksum unco_cont, by(High)
ranksum cont_rep, by(High)
restore

* TESTS OF H1 ON EACH ABILITY MEASURE*
preserve 
drop if period>1
table crtHigh  , contents(m perc_unco m perc_cont m dif_cont2)
table raceHigh  , contents(m perc_unco m perc_cont m dif_cont2)
table guessHigh  , contents(m perc_unco m perc_cont m dif_cont2)
table crtHigh if crtscore==0|crtscore==3 , contents(m perc_unco m perc_cont m dif_cont2)
table raceHigh if bi3==0|bi3==3, contents(m perc_unco m perc_cont m dif_cont2)
ranksum dif_cont2, by(crtHigh)
ranksum dif_cont2 if crtscore==0|crtscore==3, by(crtHigh)
ranksum dif_cont2, by(raceHigh)
ranksum dif_cont2 if bi3==0|bi3==3, by(raceHigh)
drop decile
gen decile=1 if cumguessability<=.1
replace decile=2 if cumguessability <= .2 & decile==.
replace decile=3 if cumguessability <= .3 & decile==.
replace decile=4 if cumguessability <= .4 & decile==.
replace decile=5 if cumguessability <= .5 & decile==.
replace decile=6 if cumguessability <= .6 & decile==.
replace decile=7 if cumguessability <= .7 & decile==.
replace decile=8 if cumguessability <= .8 & decile==.
replace decile=9 if cumguessability <= .9 & decile==.
replace decile=10 if cumguessability <= 1 & decile==.
ranksum dif_cont2, by(guessHigh)
drop if decile>3 & decile<8
*top30 vs bottom 30%*
table guessHigh  , contents(m perc_unco m perc_cont m dif_cont2)
ranksum dif_cont2, by(guessHigh)
restore


*******************************************************
*HYPOTHESIS 2*
*******************************************************

********************************************
**********FIGURE 3*****************
****************************************
preserve
drop if period>12
replace laggr=round(lagothercont, 2)
collapse cont,by(High laggroupcont)
g relativecut = cont/laggroupcont
table High, contents(m relativecut)
twoway (line relativecut  laggroupcont if High == 0 &lagg>4, sort msymbol(X) lcolor(gs2) mcolor(gs2))/*
*/(line relativecut  laggroupcont if High == 1 &lagg>4, sort  msymbol(triangle)  lcolor(gs9) mcolor(gs9)) /*
*/ ,xlabel (6(2)20) ylabel(0.5(0.1)1.1) legend(label(1 Low-ability subjects)label(2 High-ability subjects))graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Own contribution/Others' average contribution in t-1) xtitle(Others' average contribution in t-1) name(cr, replace)
restore 


********************************************
**********FIGURE 3 top vs bottom 30*****************
****************************************
preserve
drop if period>12
replace laggr=round(lagothercont, 2)
drop if decile>3 & decile<8
collapse cont,by(High laggroupcont)
g relativecut = cont/laggroupcont
table High, contents(m relativecut)

twoway (line relativecut  laggroupcont if High == 0 &lagg>4, sort msymbol(X) lcolor(gs2) mcolor(gs2))/*
*/(line relativecut  laggroupcont if High == 1 &lagg>4, sort  msymbol(triangle)  lcolor(gs9) mcolor(gs9)) /*
*/ ,xlabel (6(2)20) ylabel(0.5(0.1)1.5) legend(label(1 Low-ability subjects)label(2 High-ability subjects))graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Own contribution/Others' average contribution in t-1) xtitle(Others' average contribution in t-1) name(cr, replace)
restore 
	

	
******************************************************************************************************************************************
***********************************************************MAIN REGRESSION TABLES**********************************************************
********************************************************************************************************************************************
	
	
***************************************************
*********** TABLE 3 ******************************
***********************************************++

xtset id period
eststo clear
eststo: xtreg relativecut CC High lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC High lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont_rep period CC##treat, re cluster(idgroup)
contrast r.treat@CC
eststo: xtreg cont_rep period High##treat, re cluster(idgroup)
contrast r.treat@High
test 2.treat+1.High#2.treat=0
eststo: xtreg cont_rep period High##treat CC female age risk i.field i.national, re cluster(idgroup)
eststo: xtreg cont CC##High lagother period if period<=12, cluster(idgroup)
contrast r.High@CC
contrast r.CC@High
eststo: xtreg cont CC##High lagother period age female i.field i.national risk if period<=12, cluster(idgroup)
*esttab using indivcont2.tex, replace se compress nobaselevels fragment star(* 0.1 ** 0.05 *** 0.01)
*esttab using indivcont2.rtf, replace se nobaselevels star(* 0.1 ** 0.05 *** 0.01) b(3) se(3)
eststo clear


******************************************************************
*********** TABLE 3 top vs bottom 30 ******************************
****************************************************************
preserve
drop if decile>3 & decile<8
*tab High CC if tag==1, col
xtset id period
eststo clear
eststo: xtreg relativecut CC High lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC High lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont_rep period High##treat, re cluster(idgroup)
contrast r.treat@High
test 2.treat+1.High#2.treat=0
eststo: xtreg cont_rep period High##treat CC female age risk i.field i.national, re cluster(idgroup)
eststo: xtreg cont CC##High lagother period if period<=12, cluster(idgroup)
contrast r.High@CC
contrast r.CC@High
eststo: xtreg cont CC##High lagother period age female i.field i.national risk if period<=12, cluster(idgroup)
esttab using indivcont30.tex, replace se compress nobaselevels fragment star(* 0.1 ** 0.05 *** 0.01)
*esttab using indivcont2.rtf, replace se nobaselevels star(* 0.1 ** 0.05 *** 0.01) b(3) se(3)
eststo clear
restore


******************************************************************
*********** TABLE A.2 test of H2 on each ability task ******************************
****************************************************************


xtset id period
eststo clear
eststo: xtreg cont CC crtHigh lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC i.crtscore lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC raceHigh lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC i.bi3 lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC bilevel  lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC guessHigh lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC guessab lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont CC choiceguess lagothercont period if period<=12, cluster(idgroup)

esttab using indivcont_measures.tex, replace se compress fragment star(** 0.05 *** 0.01)
esttab using indivcont_measures.rtf, replace se star(** 0.05 *** 0.01) b(3) se(3)
eststo clear

*********************************************************
*why not test treatment effects on 50vs50 in each ability task?
********************************************************
pwcorr crtab raceab guessab ability if period==1, sig
pwcorr crtab raceab guessab ability if period==1 &(decile<=3|decile>=8), sig
tab crtHigh High if period==1, row col
tab raceHigh High if period==1, row col
tab guessHigh High if period==1, row col
xtreg cont_rep period crtHigh##treat, re cluster(idgroup)
test 2.treat+2.treat#1.crtHigh=0
xtreg cont_rep period raceHigh##treat, re cluster(idgroup)
test 2.treat+2.treat#1.raceHigh=0
xtreg cont_rep period guessHigh##treat, re cluster(idgroup)
test 2.treat+2.treat#1.guessHigh=0

****************************************************************************************
*TABLE A.3 TESTS of H2, H3, H5a for top vs bottom 30 in each ability task*
***********************************************************************************
**********CRT**********************************
preserve
tab crtscore
drop if crtscore==1|crtscore==2
*top20 vs bottom 33%*
tab crtHigh High if period==1, row col
ranksum dif_cont2 if period==1, by(crtHigh)
xtset id period
eststo clear
eststo: xtreg cont CC crtHigh lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont_rep period crtHigh##treat, re cluster(idgroup)
contrast r.treat@crtHigh
test 2.treat+1.crtHigh#2.treat=0
eststo: xtreg cont CC##crtHigh lagother period if period<=12, cluster(idgroup)
contrast r.crtHigh@CC
contrast r.CC@crtHigh
restore


**********BI**********************************
preserve
tab bilevel
drop if bilevel==2|bilevel==3
*top20 vs bottom 50%*
tab raceHigh High if period==1 , row col
ranksum dif_cont2 if period==1, by(raceHigh)
xtset id period
eststo: xtreg cont CC raceHigh lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont_rep period raceHigh##treat, re cluster(idgroup)
contrast r.treat@raceHigh
test 2.treat+1.raceHigh#2.treat=0
eststo: xtreg cont CC##raceHigh lagother period if period<=12, cluster(idgroup)
contrast r.raceHigh@CC
contrast r.CC@raceHigh
restore

**********BEAUTY CONTEST**********************************
preserve
drop decile
gen decile=1 if cumguessability<=.1
replace decile=2 if cumguessability <= .2 & decile==.
replace decile=3 if cumguessability <= .3 & decile==.
replace decile=4 if cumguessability <= .4 & decile==.
replace decile=5 if cumguessability <= .5 & decile==.
replace decile=6 if cumguessability <= .6 & decile==.
replace decile=7 if cumguessability <= .7 & decile==.
replace decile=8 if cumguessability <= .8 & decile==.
replace decile=9 if cumguessability <= .9 & decile==.
replace decile=10 if cumguessability <= 1 & decile==.
drop if decile>3 & decile<8
*top30 vs bottom 30%*
tab guessHigh High if period==1, row col
ranksum dif_cont2 if period==1, by(guessHigh)
xtset id period
eststo: xtreg cont CC guessHigh lagothercont period if period<=12, cluster(idgroup)
eststo: xtreg cont_rep period guessHigh##treat, re cluster(idgroup)
contrast r.treat@guessHigh
test 2.treat+1.guessHigh#2.treat=0
eststo: xtreg cont CC##guessHigh lagother period if period<=12, cluster(idgroup)
contrast r.guessHigh@CC
contrast r.CC@guessHigh
esttab using indivcont_measures30.tex, replace se compress nobaselevels fragment star(* 0.1 ** 0.05 *** 0.01)
*esttab using indivcont2.rtf, replace se nobaselevels star(* 0.1 ** 0.05 *** 0.01) b(3) se(3)
eststo clear
restore



*****************************************
* APPENDIX: TABLE A.5: Tobit
*****************************************
eststo: tobit relativecut CC High lagothercont period if period<=12, ll(0) ul(20) vce(cl idgroup)
eststo: tobit cont CC High lagothercont period if period<=12, ll(0) ul(20) vce(cl idgroup)
eststo: tobit cont_rep period CC##treat, ll(0) ul(20) vce(cl idgroup)
eststo: tobit cont_rep period High##treat CC, ll(0) ul(20) vce(cl idgroup)
eststo: tobit cont_rep period High##treat CC female age risk i.field i.national, ll(0) ul(20)  vce(cl idgroup)
eststo: tobit cont CC##High lagother period if period<=12, vce(cl idgroup)
eststo: tobit cont CC##High lagother period age female i.field i.national risk if period<=12, vce(cl idgroup)
esttab using indivtobit2.tex, replace se compress nobaselevels fragment b(3) se(3)  star(* 0.10 ** 0.05 *** 0.01)



************************************************************
******************LAST PERIOD and ENDGAME EFFECTS****************
***********************************************************

preserve 
drop if period<15
g zero=0
replace zero=1 if cont==0
sum  perc_unco  perc_cont dif_cont2 
table type High , contents(m zero)
table type, contents(m zero)
table High , contents(m zero)
table High  , contents(m perc_unco m perc_cont m dif_cont2)
table type   , contents(m perc_unco m perc_cont m dif_cont2)
ranksum dif_cont2, by(High)
bysort CC: ranksum cont_rep, by(High)
ranksum dif_cont2, by(High)
ranksum cont_rep, by(High)
kwallis dif_cont2 , by(type)
ranksum dif_cont2 if type==0|type==2, by(type)
ranksum cont_rep if type==0|type==2, by(type)
bysort High: ranksum cont_rep if type==0|type==2, by(type)
eststo clear
eststo: reg cont i.type  if period==15, cluster(idgroup)
test 1.type 2.type 3.type
eststo: reg cont High  if period==15, cluster(idgroup)
eststo: reg cont CC High  if period==15, cluster(idgroup)
eststo: reg cont CC High lagother if period==15, cluster(idgroup)
eststo: reg cont CC##High  if period==15, cluster(idgroup)
eststo: reg cont CC##High lagother if period==15, cluster(idgroup)
esttab using lastperiodreg.tex, replace se compress nobaselevels fragment star(* 0.1 ** 0.05 *** 0.01)
restore

g end=0
replace end=1 if period>12
eststo clear
eststo: xtreg relativecut CC end##High lagothercont period , cluster(idgroup)
eststo: xtreg relativecut CC end##High lagothercont period if decile<=3|decile>=8, cluster(idgroup)
eststo: xtreg cont period periodsq if bi3==0, cluster(idgroup)
predict quadlo, xb
eststo: xtreg cont period periodsq if bi3==3, cluster(idgroup)
predict quadhi, xb
eststo: xtreg cont period periodsq if High==0 & (decile<=3|decile>=8), cluster(idgroup)
eststo: xtreg cont period periodsq if High==1 & (decile<=3|decile>=8), cluster(idgroup)
* TABLE
esttab using endgame.tex, replace se compress nobaselevels fragment star(* 0.1 ** 0.05 *** 0.01)

*Figure
twoway  (connected quadlo period, sort msymbol(O) lcolor(gs6) mcolor(gs6))/*
	*/(connected quadhi period, sort msymbol(X) lcolor(gs2) mcolor(gs2)) /*

	*/ ,  legend(label(1 Backward induction<2)label(2 Backard Induction>3)) graphregion(color(white)) plotregion(margin(small) color(white)) ytitle(Contribution) xtitle(Period) name(endgame, replace)



*******************************************************************************************************
*** HYPOTHESIS 3***********************************
********************************************************************************************************

*statistics prsented before H3
tab allCC treat if period==1 
tab allFR treat if period==1 
tab allH treat if period==1 
tab allLow treat if period==1 
preserve
drop if period>=12
collapse cont idgroup treat,by(id)
bysort idgroup: egen varcont= sd(cont_rep)
bysort idgroup: egen meangroup=mean(cont_)
bysort treat: egen varacross=sd(meangroup) 
table treat, contents(m varcont m varacross)
restore

****************************************
*FIGURES 4 and 5***********************
*************************************

bysort type treat period: egen avg_cont_type2 = mean(cont_reppgg)
twoway (connected avg_cont_type2 period if type == 2 & treat==0, sort msymbol(X) lcolor(gs2) mcolor(gs2))/*
*/(connected avg_cont_type2 period if type == 0 & treat==0, sort  msymbol(triangle)  lcolor(gs8) mcolor(gs8)) /*
*/ ,ylabel(0(4)21) legend(label(1 Conditional cooperators )label(2 Free riders)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Contribution) title(RAND) name(cr, replace) 
twoway (connected avg_cont_type2 period if type == 2 & treat==1, sort msymbol(X) lcolor(gs2) mcolor(gs2))/*
*/(connected avg_cont_type2 period if type == 0 & treat==1, sort  msymbol(triangle)  lcolor(gs8) mcolor(gs8))/*
*/ ,ylabel(0(4)21) legend(label(1 Conditional cooperators )label(2 Free riders)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Contribution)title(PREF) name(cp, replace) 
twoway (connected avg_cont_type2 period if type == 2 & treat==2, sort msymbol(X) lcolor(gs2) mcolor(gs2)) /*
*/(connected avg_cont_type2 period if type == 0 & treat==2, sort  msymbol(triangle)  lcolor(gs8) mcolor(gs8))/*
*/ , ylabel(0(4)21) legend(label(1 Conditional cooperators )label(2 Free riders)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Contribution) title(STRAT) name(cs, replace)

grc1leg cr cp cs, legendfrom(cr) graphregion(fcolor(white))


twoway (connected avg_cont_ability period if High == 1 & treat==0, sort msymbol(X) lcolor(gs2) mcolor(gs2)) /*
*/(connected avg_cont_ability period if High == 0 & treat==0, sort  msymbol(triangle)  lcolor(gs8) mcolor(gs8)) /*
*/ ,ylabel(0(4)21) legend(label(1 High-ability subjects )label(2 Low-ability subjects ))graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Contribution) title(RAND) name(hr, replace) 
twoway (connected avg_cont_ability period if High == 1 & treat==1, sort msymbol(X) lcolor(gs2) mcolor(gs2))/*
*/(connected avg_cont_ability period if High == 0 & treat==1, sort  msymbol(triangle)  lcolor(gs8) mcolor(gs8))/*
*/ ,ylabel(0(4)21) legend(label(1 High-ability subjects )label(2 Low-ability subjects ))graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Contribution)title(PREF) name(hp, replace) 
twoway (connected avg_cont_ability period if High == 1 & treat==2, sort msymbol(X) lcolor(gs2) mcolor(gs2)) /*
*/(connected avg_cont_ability period if High == 0 & treat==2, sort  msymbol(triangle)  lcolor(gs8) mcolor(gs8))/*
*/ , ylabel(0(4)21) legend(label(1 High-ability subjects )label(2 Low-ability subjects )) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white))ytitle(Contribution) title(STRAT) name(hs, replace)

grc1leg hr hp hs, legendfrom(hr) graphregion(fcolor(white))

*******************************************************************
*FIGURE A7: contributions by treatment*************************************
*****************************************************************

twoway (connected avg_cont_all period if treat==0, sort msymbol(X) lcolor(gs6) mcolor(gs6)) /*
*/(connected avg_cont_all period if treat==1, sort msymbol(triangle) lcolor(gs10) mcolor(gs10))/*
*/ (connected avg_cont_all period if treat==2, sort  msymbol(circle) lcolor(gs2) mcolor(gs2)) /*
*/, ytitle(Contribution)legend(label(1 RAND)label(2 PREF)label(3 STRAT)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) 


**********************************************************************************************
****HYPOTHESIS 5b****************************************************************************
**********************************************************************************************


******************************************
***************FIGURE 6*************************
*********************************************
twoway (connected avg_cont_compo period if allH == 1 & allCC==1, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) msymbol(circle) lcolor(gs2) mcolor(gs2))/*
*/ (connected avg_cont_compo period if allH == 1 & allCC==0, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) msymbol(X) lcolor(gs8) mcolor(gs8)) /*
*/(connected avg_cont_compo period if allH == 0 & allCC==1, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) lpattern(dash) msymbol(triangle) lcolor(gs2) mcolor(gs2))/*
*/(connected avg_cont_compo period if allH == 0 & allCC==0, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) lpattern(dash) msymbol(diamond) lcolor(gs8) mcolor(gs8))/*
*/ ,legend(label(1 High-ability conditional cooperators)label(2 Other high-ability groups)label(3 Other groups of conditional cooperators)label(4 Other groups) size(small)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) 

******************************************************
***************FIGURE 6 top vs bottom 30*************************
*********************************************
preserve
drop avg_cont_compo
drop if decile>3 &decile<8
bysort session period group: egen survival=count(id)
drop if survival<3
bysort allH allCC period: egen avg_cont_compo=mean(cont_rep)
twoway (connected avg_cont_compo period if allH == 1 & allCC==1, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) msymbol(circle) lcolor(gs2) mcolor(gs2))/*
*/ (connected avg_cont_compo period if allH == 1 & allCC==0, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) msymbol(X) lcolor(gs8) mcolor(gs8)) /*
*/(connected avg_cont_compo period if allH == 0 & allCC==1, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) lpattern(dash) msymbol(triangle) lcolor(gs2) mcolor(gs2))/*
*/(connected avg_cont_compo period if allH == 0 & allCC==0, sort ylabel(0(4)21) ytitle(Contribution) xtitle(Period) lpattern(dash) msymbol(diamond) lcolor(gs8) mcolor(gs8))/*
*/ ,legend(label(1 High-ability conditional cooperators)label(2 Other high-ability groups)label(3 Other groups of conditional cooperators)label(4 Other groups) size(small)) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) 
restore

************************************************************************************
*Test of H5b*
***********************************************************************************
g grouptype=0
replace  grouptype= 1 if allH==0 &allCC==1
replace  grouptype= 2 if allH==1 &allCC==0
replace  grouptype= 3 if allH==1 &allCC==1
preserve 
tab cont grouptype
drop if period==15
collapse cont grouptype n_H n_cc, by(idgroup)
kwallis cont, by(grouptype)
ranksum cont if grouptype==1|grouptype==0, by(grouptype) 
ranksum cont if grouptype==2|grouptype==0, by(grouptype) 
ranksum cont if grouptype==3|grouptype==0, by(grouptype) 
ranksum cont if grouptype==2|grouptype==1, by(grouptype) 
ranksum cont if grouptype==3|grouptype==1, by(grouptype)
ranksum cont if grouptype==3|grouptype==2, by(grouptype) 
restore

*top30 vs bottom30*
preserve
drop if period==15
drop if decile>3 & decile<8
bysort session period group: egen survival=count(id)
drop if survival<3
collapse cont grouptype n_H n_cc, by(idgroup)
kwallis cont, by(grouptype)
ranksum cont if grouptype==1|grouptype==0, by(grouptype) 
ranksum cont if grouptype==2|grouptype==0, by(grouptype) 
ranksum cont if grouptype==3|grouptype==0, by(grouptype) 
ranksum cont if grouptype==2|grouptype==1, by(grouptype) 
ranksum cont if grouptype==3|grouptype==1, by(grouptype)
ranksum cont if grouptype==3|grouptype==2, by(grouptype) 
restore



***********************************************************************
*APPENDIX D: COMPARISON WITH BURLANDO AND GUALA**
***********************************************************************

append using "C:\Users\marco\Dropbox\Research\Grandjean_Lefebvre_Mantovani\Experiment\Analysis\bg_long.dta", force

gen bg=0
replace bg=1 if treatmentnumber==.

*FIGURE A7*
twoway (connected cond_cont other_var if  type == 2 & bg==1, sort msymbol(X) lcolor(gs6) mcolor(gs6)) /*
*/(connected cond_cont other_var if  type == 2 & bg==0, sort msymbol(triangle) lcolor(gs10) mcolor(gs10))/*
*/(line other_var other_var, sort)/*
*/, ytitle(Conditional contribution) xtitle(Others contributions)legend(label(1 Burlando & Guala) label(2 Our sample )) graphregion(fcolor(white)) plotregion(margin(small) fcolor(white)) 


egen idtreat=group(bg id)
drop avgsm 
bysort idtreat:egen avgsm= mean(cond)
xtset idtreat
xtreg cond c.other_var##bg if type==0, re


*TABLE A.4 model 1*
eststo clear
eststo: xtreg cond c.other_var##bg if type==2,re

collapse cond type, by(id bg)
ranksum cond, by(bg)
ranksum cond if type==2, by(bg)

restore

****TABLE A.4 models2-5
xtset id period
bysort idgroup: egen groupcoop =mean(avgsm)
 gen supergroup=0
replace supergroup=1 if (groupcoop*3-avgsm)/2>70 & allCC==1 /*(above the median CC in our sample)*/
gen morecoop=0
replace morecoop=1 if avgsm>70 /*(above the median CC in our sample)*/
eststo:xtreg cont_rep period i.allCC if CC==1 , re cluster(idgroup)
eststo: xtreg cont_rep period i.allCC if CC==1 & morecoop==1 , re cluster(idgroup)
eststo: xtreg cont_rep period i.supergroup if CC==1, re cluster(idgroup)
eststo: xtreg cont_rep period i.supergroup if CC==1 & morecoop==1 , re cluster(id)
esttab using discussionreg.tex, replace se b(3) se(3) compress nobaselevels fragment star(* 0.1 ** 0.05 *** 0.01)






