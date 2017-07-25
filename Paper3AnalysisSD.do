********************************************************************************
**Data Analysis do-file, "The importance of family and friends on human capital development in the mid-childhood and early adolesence"
********************************************************************************

cd "/users/jBook/Desktop/School/Paper3/"
use p3datasd, clear
capture log close
tsset, clear
set more off 


local yl csesscore csesscoret avgros avgylpar avgfriend ageyr border hhsize female typesite adverse chheight zppvt chprob zhfa ppvtraw agemon CASHSH CASHCL CASHWK CCLTRG CEMBBK CWRUNI FEAY03R4 FEAY07R4 FEAY10R4 FEAY13R4 FEAY19R4 FEAY21R4 FEAY25R4 FEAY29R4 $ses 
	foreach x of local yl {
		rename `x' `x'1
		}

rename  FEAS02R4 FEAY03R42
rename  FEAS06R4 FEAY07R42
rename  FEAS08R4 FEAY10R42 
rename  FEAS11R4 FEAY13R42 
rename  FEAS15R4 FEAY19R42
rename  FEAS17R4 FEAY21R42
rename  FEAS20R4 FEAY25R42
rename  FEAS23R4 FEAY29R42
	
local b ASHCL ASHWK ASHSH CLTRG EMBBK WRUNI
foreach x of local b  {
	rename S`x' C`x'2
	}

rename sib_zhfa	zhfa2	
rename S1HGHT chheight2
rename zsppvt zppvt2
rename sibparscore ylparscore2
rename ylparscore ylparscore1
rename sibfriendscore ylfriendscore2
rename ylfriendscore ylfriendscore1
rename sros rosses2
rename rosses rosses1
rename sdborder border2
rename sdgend female2
rename ssesscore csesscore2
rename ssesscoret csesscoret2
rename avgsros avgros2
rename avgsibpar avgylpar2
rename avgsibfriend avgfriend2
rename sppvt ppvtraw2


keep childcode mdsum srosfull rosfull   avgros* avgylpar* avgfriend* wi wq2-wq5 poor caredu typesite adverse1 r2 r3 hhsize agemon1 agemon2  csesscore** psesscore*  ageyr1 ageyr2 border* female* chheight* zppvt* chprob* ppvtraw* zhfa* rosses* ylfriendscore* ylparscore* carepri caresec careter clustid round

reshape long csesscore avgros avgylpar avgfriend csesscoret ageyr border female chheight zppvt chprob rosses agemon ylfriendscore zhfa ylparscore ppvtraw, i(childcode) j(sib)
egen cid_num=group(childcode)
tsset cid_num sib
label var ylparscore "Parent Relationship Score"
label var ylfriendscore "Peer Relationship Score"
replace female=1 if female==.
qui sum ageyr
replace ageyr=r(mean) if ageyr==.
	
gen sibsamp=0
	replace sibsamp=1
gen wquint=0
	replace wquint=1 if wq2==0 | wq3==0 | wq4==0 | wq5==0 
	replace wquint=2 if wq2==1 
	replace wquint=3 if wq3==1
	replace wquint=4 if wq4==1
	replace wquint=5 if wq5==1
	
rename hhsize1 hhsize
rename typesite1 typesite
rename adverse1 adverse
label var csesscoret "Child's Pride Raw Score"
label var avgros "Rosenberg Self-Esteem Raw Score"
label var avgylpar "Parent Relationship Raw Score"
label var avgfriend "Friend Relationship Raw Score"
label var ageyr "Age in Years"	
label var ppvtraw "Raw PPVT Score"
label var chheight "Child's Height (cm)"

drop zppvt
bysort ageyr: egen sdam=sd(ppvt) 
bysort ageyr: egen mppvt=mean(ppvt)

gen double zppvt=(ppvt-mppvt)/sdam
label var zppvt "Age standardised PPVT score"
	
qui xtivreg2 rosses ylparscore ylfriendscore female border ageyr zppvt chheight, fe cluster(clustid)
gen ws=e(sample)
keep if ws==1	
duplicates tag childcode, gen(a)	
keep if a==1 
		
save p3sibs, replace

**503 pairs, 5/7/17**

********************************************************************************
**Summary Tables**
********************************************************************************
********************************************************************************
**Determining Gaps:  By terciles of wealth**
********************************************************************************


use p3sibs, clear

estpost sum csesscoret avgros psesscoret avgylpar avgfriend ageyr female hhsize typesite carepri caresec careter adverse ppvt chheight  if round==4 & sibsamp==1
est store sumpanela

estpost sum csesscoret avgros psesscoret avgylpar avgfriend ageyr female ppvt chheight  if round==4 & sib==1 & sibsamp==1
est store sumpanel1
estpost sum csesscoret avgros psesscoret avgylpar avgfriend ageyr female  ppvt chheight  if round==4 & sib==2 & sibsamp==1
est store sumpanel2

estpost sum csesscoret psesscoret avgros avgylpar avgfriend hhsize typesite carepri caresec careter adverse if round==4 & wquint==5 & sibsamp==1
est store sumpanel4wq5
estpost sum csesscoret psesscoret avgros avgylpar avgfriend hhsize typesite carepri caresec careter adverse if round==4 & wquint==4 & sibsamp==1
est store sumpanel4wq4
estpost sum csesscoret psesscoret avgros avgylpar avgfriend hhsize typesite carepri caresec careter adverse if round==4 & wquint==3 & sibsamp==1
est store sumpanel4wq3
estpost sum csesscoret psesscoret avgros avgylpar avgfriend hhsize typesite carepri caresec careter adverse if round==4 & wquint==2 & sibsamp==1
est store sumpanel4wq2
estpost sum csesscoret psesscoret avgros avgylpar avgfriend hhsize typesite carepri caresec careter adverse if round==4 & wquint==1 & sibsamp==1
est store sumpanel4wq1


**Table 1**
esttab sumpanela sumpanel1 sumpanel2 using sibsum.rtf, ///
mtitles( "Sibling Sample" "Older Siblings" "Younger Siblings") title("Descriptive Statistics By Round")  cells(" mean(fmt(2)) sd")   ///
nogaps onecell replace compress label noobs 

esttab sumpanela sumpanel1 sumpanel2 using sibsum.tex, ///
mtitles( "Sibling Sample" "Older Siblings" "Younger Siblings") title("Descriptive Statistics By Round")  cells(" mean(fmt(2)) sd")   ///
nogaps onecell replace compress label noobs 

use p3sibs, clear
gen qt=1 if wquint==1 
replace qt=0 if wquint==5 
gen qt53=1 if wquint==3
replace qt53=0 if wquint==5
gen qt31=1 if wquint==1
replace qt31=0 if wquint==3

gen q=1 if wquint==1
replace q=2 if wquint==5

order csesscoret avgros psesscoret avgylpar avgfriend hhsize typesite carepri caresec careter adverse

foreach x in csesscoret-adverse {
	estpost ttest `x' if round==4, by(qt) 
	est store tab8sib
	}

foreach x in csesscoret-adverse {
	estpost ttest `x' if round==4, by(qt53) 
	est store tab9sib
	}
	
foreach x in csesscoret-adverse {
	estpost ttest `x' if round==4, by(qt31) 
	est store tab10sib
	}	

**Table 2**

esttab sumpanel4wq5 sumpanel4wq4 sumpanel4wq3 sumpanel4wq2 sumpanel4wq1 tab8sib tab9sib tab10sib using sibwqdiff.rtf, ///
	mtitles( "Q5" "Q4" "Q3" "Q2" "Q1" "Q5 v Q1" "Q5 v Q3" "Q3 v Q1" ) title("Round 4 Descriptives By Quintile of Wealth")  ///
	cells("mean(fmt(2) ) & sd(par)" "b(star fmt(2)) & se(par)") addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001. Groups divided by quintiles of wealth") onecell ///
	mgroups("Summary Statistics" "Difference In Means", pattern(1 0 0 0 1 0 0)) nogaps replace compress label	 

esttab sumpanel4wq5 sumpanel4wq4 sumpanel4wq3 sumpanel4wq2 sumpanel4wq1 tab8sib tab9sib tab10sib using sibwqdiff.tex, ///
	mtitles( "Q5" "Q4" "Q3" "Q2" "Q1" "Q5 v Q1" "Q5 v Q3" "Q3 v Q1" ) title("Round 4 Descriptives By Quintile of Wealth")  ///
	cells("mean(fmt(2) ) & sd(par)" "b(star fmt(2)) & se(par)") addnotes("* p < 0.05, ** p < 0.01, *** p < 0.001. Groups divided by quintiles of wealth") ///
	mgroups("Summary Statistics" "Difference In Means", pattern(1 0 0 0 1 0 0)) nogaps replace compress label	 

********************************************************************************
**Non-parametrics**
********************************************************************************

use p3sibs, clear



foreach x in csesscore rosses ylparscore ylfriendscore {
	bysort ageyr: egen mean1`x'=mean(`x') if wquint==1 & round==4 
	by ageyr: egen sd1`x'=sd(`x') if wquint==1 & round==4 
	by ageyr: egen mean2`x'=mean(`x') if wquint==5 & round==4 
	by ageyr: egen sd2`x'=sd(`x') if wquint==5 & round==4 
	}
**Creating upper and lower bounds for CI's**
foreach x in csesscore rosses ylparscore ylfriendscore {
	bysort ageyr: gen ic`x'd1=mean1`x' - 1.96*(sd1`x')/(304^(1/2)) if round==4
	bysort ageyr: gen ic`x'u1=mean1`x' + 1.96*(sd1`x')/(304^(1/2)) if round==4
	bysort ageyr: gen ic`x'd2=mean2`x' - 1.96*(sd2`x')/(134^(1/2)) if round==4
	bysort ageyr: gen ic`x'u2=mean2`x' + 1.96*(sd2`x')/(134^(1/2)) if round==4
}

foreach x in rosses csesscore ylparscore ylfriendscore {
		forvalues t=1/2 {
			qui lowess mean`t'`x' ageyr if round==4, gen(`x'sm`t') nodraw
			qui lowess ic`x'd`t' ageyr if round==4, gen(ic`x'sm`t'd) nodraw
			qui lowess ic`x'u`t' ageyr if round==4, gen(ic`x'sm`t'u) nodraw
		}
	}


**Figure 1**	
lpoly rossessm1 ageyr, bwidth(1) nosc addplot(lpoly rossessm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Upper quintile"))   || lpoly icrossessm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly icrossessm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly icrossessm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly icrossessm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Self-Esteem: Z-Score by age and Wealth)ytitle("Z-Score") legend(label(1 "Lower quintile" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 

lpoly csesscoresm1 ageyr, bwidth(1) nosc addplot(lpoly csesscoresm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Upper quintile"))   || lpoly iccsesscoresm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly iccsesscoresm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly iccsesscoresm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly iccsesscoresm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Pride: Z-Score by age and Wealth)ytitle("Z-Score") legend(label(1 "Lower quintile" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 

lpoly ylparscoresm1 ageyr, bwidth(1) nosc addplot(lpoly ylparscoresm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Upper quintile"))   || lpoly icylparscoresm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly icylparscoresm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly icylparscoresm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly icylparscoresm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Parent Relationship: Z-Score by age and Wealth)ytitle("Z-Score") legend(label(1 "Lower quintile" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 

lpoly ylfriendscoresm1 ageyr, bwidth(1) nosc addplot(lpoly ylfriendscoresm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Upper quintile"))   || lpoly icylfriendscoresm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly icylfriendscoresm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly icylfriendscoresm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly icylfriendscoresm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Peer Relationship: Z-Score by age and Wealth)ytitle("Z-Score") legend(label(1 "Lower quintile" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 


use p3sibs, clear


*keep if ageyr<=150

foreach x in csesscore rosses ylparscore ylfriendscore {
	bysort ageyr: egen mean1`x'=mean(`x') if typesite==0 & round==4 
	by ageyr: egen sd1`x'=sd(`x') if typesite ==0 & round==4 
	by ageyr: egen mean2`x'=mean(`x') if typesite ==1 & round==4 
	by ageyr: egen sd2`x'=sd(`x') if typesite ==1 & round==4 
	}
**Creating upper and lower bounds for CI's**
foreach x in csesscore rosses ylparscore ylfriendscore {
	bysort ageyr: gen ic`x'd1=mean1`x' - 1.96*(sd1`x')/(304^(1/2)) if round==4
	bysort ageyr: gen ic`x'u1=mean1`x' + 1.96*(sd1`x')/(304^(1/2)) if round==4
	bysort ageyr: gen ic`x'd2=mean2`x' - 1.96*(sd2`x')/(134^(1/2)) if round==4
	bysort ageyr: gen ic`x'u2=mean2`x' + 1.96*(sd2`x')/(134^(1/2)) if round==4
}

foreach x in rosses csesscore ylparscore ylfriendscore {
		forvalues t=1/2 {
			qui lowess mean`t'`x' ageyr if round==4, gen(`x'sm`t') nodraw
			qui lowess ic`x'd`t' ageyr if round==4, gen(ic`x'sm`t'd) nodraw
			qui lowess ic`x'u`t' ageyr if round==4, gen(ic`x'sm`t'u) nodraw
		}
	}

**Figure 2**	
lpoly rossessm1 ageyr, bwidth(1) nosc addplot(lpoly rossessm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Rural"))   || lpoly icrossessm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly icrossessm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly icrossessm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly icrossessm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Self-Esteem: Z-Score by age and Locality)ytitle("Z-Score") legend(label(1 "Urban" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 

lpoly csesscoresm1 ageyr, bwidth(1) nosc addplot(lpoly csesscoresm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Rural"))   || lpoly iccsesscoresm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly iccsesscoresm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly iccsesscoresm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly iccsesscoresm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Pride: Z-Score by age and Locality)ytitle("Z-Score") legend(label(1 "Urban" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 

lpoly ylparscoresm1 ageyr, bwidth(1) nosc addplot(lpoly ylparscoresm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Rural"))   || lpoly icylparscoresm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly icylparscoresm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly icylparscoresm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly icylparscoresm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Parent Relationship: Z-Score by age and Locality)ytitle("Z-Score") legend(label(1 "Urban" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 

lpoly ylfriendscoresm1 ageyr, bwidth(1) nosc addplot(lpoly ylfriendscoresm2 ageyr, bwidth(1) lpattern(dash) legend(label(2 "Rural"))   || lpoly icylfriendscoresm1d ageyr, bwidth(1) lpattern(dot) lcolor(blue) legend(label(3 "95% CI"))  || lpoly icylfriendscoresm1u ageyr, bwidth(1) lpattern(dot) lcolor(blue) || lpoly icylfriendscoresm2d ageyr, bwidth(1) lpattern(dot) lcolor(red) || lpoly icylfriendscoresm2u ageyr, bwidth(1) lpattern(dot) lcolor(red) legend(label(5 "95% CI"))) ///
ti(Children's Peer Relationship: Z-Score by age and Locality)ytitle("Z-Score") legend(label(1 "Urban" ) order(1 2 3 5)) ylabel(-0.4 (0.2) 0.4) 


********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
**Regressions**
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

use p3sibs, clear
set more off

***Pooled Regressions***
global sibcon "ageyr border hhsize female typesite adverse zppvt chheight"
capture log close
log using sibs, text replace

ivreg2 csesscore psesscore wq2-wq5 caredu sib $sibcon r2 r3 , cluster(clustid) 
est store sibpool01
ivreg2 csesscore psesscore wq2-wq5 caredu ylparscore ylfriendscore sib $sibcon r2 r3 , cluster(clustid)
est store sibpool1
b1x2 csesscore, x1all(wq2-wq5 sib $sibcon ylfriendscore r2 r3) x2all(psesscore caredu) x2delta(g1 = psesscore :g2= caredu )  x1only(wq5)

ivreg2 zppvt wq2-wq5 csesscore rosses caredu ylparscore ylfriendscore ageyr border hhsize female typesite adverse chheight r2 r3 , cluster(clustid) 
est store sibpool2
boottest {ylparscore} {ylfriendscore}
b1x2 zppvt, x1all(wq2-wq5 ylparscore ylfriendscore caresec careter sib ageyr border hhsize female typesite adverse chheight  r2 r3) x2all(psesscore csesscore rosses) x2delta(g1 = psesscore : g2= csesscore rosses )  x1only(wq5)

ivreg2 rosses psesscore wq2-wq5 caredu ylparscore ylfriendscore sib $sibcon r2 r3 , cluster(clustid) 
est store sibpool03
ivreg2 rosses psesscore wq2-wq5 caredu ylparscore ylfriendscore sib $sibcon r2 r3 , cluster(clustid) 
est store sibpool3
b1x2 rosses, x1all(wq2-wq5 sib $sibcon ylparscore r2 r3) x2all(psesscore caredu ylfriendscore  ) x2delta(g1 = psesscore :g2= caredu : g3=ylfriendscore)  x1only(wq5)

ivreg2 rosses wq2-wq5 caredu psesscore ylparscore ylfriendscore $sibcon r2 r3 if sib==1 , cluster(clustid) 
est store sib1ros
ivreg2 rosses wq2-wq5 caredu psesscore ylparscore ylfriendscore $sibcon r2 r3 if sib==2, cluster(clustid) 
est store sib2ros

ivreg2 csesscore wq2-wq5 caredu psesscore ylparscore ylfriendscore $sibcon r2 r3 if sib==1 , cluster(clustid) 
est store  sib1ses
ivreg2 csesscore wq2-wq5 caredu psesscore ylparscore ylfriendscore $sibcon r2 r3 if sib==2, cluster(clustid) 
est store sib2ses


**Subample regressions**
ivreg2 csesscore wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if female==0, cluster(clustid)
est store fem0c 
ivreg2 csesscore wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if female==1, cluster(clustid)
est store fem1c

ivreg2 csesscore wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if typesite==0, cluster(clustid)
est store urbc
ivreg2 csesscore wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if typesite==1 , cluster(clustid)   
est store rurc

ivreg2 rosses wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if female==0, cluster(clustid)
est store fem0 
ivreg2 rosses wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if female==1, cluster(clustid)
est store fem1

ivreg2 rosses wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if typesite==0, cluster(clustid)
est store urb
ivreg2 rosses wq2-wq5 psesscore caredu ylparscore ylfriendscore sib $sibcon r2 r3 if typesite==1 , cluster(clustid)   
est store rur



/*OLS Regression Tables*/

**Table 3**
esttab sibpool1 sib1ses sib2ses using sibols.rtf, replace ///
compress nonum onecell ///
mtitles("Pooled" "Older Sibling" "Younger Sibling" ) ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum nonotes noobs ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab sibpool3 sib1ros sib2ros using sibols.rtf, replace append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum onecell ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab sibpool1 sib1ses sib2ses using sibols.tex, replace ///
compress nonum ///
mtitles("Pooled" "Sibling" "Younger Sibling" ) ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum nonotes noobs ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab sibpool3 sib1ros sib2ros using sibols.tex, append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)


**Table A1**
esttab fem0c fem1c urbc rurc using sibhet.rtf, replace ///
compress nonum nonotes noobs mtitles("Males" "Females" "Urban" "Rural") ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum onecell ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab fem0 fem1 urb rur using sibhet.rtf, replace append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum onecell ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab fem0c fem1c urbc rurc using sibhet.tex, replace ///
compress nonum nonotes noobs mtitles("Males" "Females" "Urban" "Rural") ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab fem0 fem1 urb rur using sibhet.tex, append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)


********************************************************************************
**Interactions, additional material available on request**
********************************************************************************
gen rdum=0 
	replace rdum=1 if sib==1
	label var rdum "Time Dummy"

foreach x of varlist wq2 wq3 wq4 wq5 psesscore ylparscore ylfriendscore {
	gen `x'oldsib=`x' * rdum
	}

tab ageyr, gen(age)
foreach x of varlist wq2 wq3 wq4 wq5 psesscore ylparscore ylfriendscore {
	foreach j of numlist 1/7 {
	gen `x'age`j' =  `x'*age`j'
}
}

ivreg2 csesscore psesscore wq2-wq5 wq2oldsib wq3oldsib wq4oldsib wq5oldsib psesscoreoldsib ylparscore ylparscoreoldsib ylfriendscore ylfriendscoreoldsib rdum caredu $sibcon r2 r3 , r
est store grad1
ivreg2 rosses psesscore wq2-wq5 wq2oldsib wq3oldsib wq4oldsib wq5oldsib psesscoreoldsib ylparscore ylparscoreoldsib ylfriendscore ylfriendscoreoldsib rdum caredu $sibcon r2 r3 , r
est store grad2
ivreg2 csesscore psesscore wq2-wq5 ylparscore ylfriendscore age2 age3 age4 age5 age6 age7  caredu sib $sibcon r2 r3 if round==4 , r
est store grad3
ivreg2 rosses psefscore wq2-wq5 ylparscore ylfriendscore age2 age3 age4 age5 age6 age7 caredu sib $sibcon r2 r3 if round==4, r
est store grad4
ivreg2 csesscore psesscore ylparscore ylfriendscore wq2-wq5 wq2age2 wq3age2 wq4age2 wq5age2 wq2age3 wq3age3 wq4age3 wq5age3 wq2age4 wq3age4 wq4age4 wq5age4 wq2age5 wq3age5 wq4age5 wq5age5 wq2age6 wq3age6 wq4age6 wq5age6 wq2age7 wq3age7 wq4age7 wq5age7 age2 age3 age4 age5 age6 age7 caredu $sibcon r2 r3 if round==4 , r
est store grad5
ivreg2 rosses psesscore ylparscore ylfriendscore wq2-wq5 wq2age2 wq3age2 wq4age2 wq5age2 wq2age3 wq3age3 wq4age3 wq5age3 wq2age4 wq3age4 wq4age4 wq5age4 wq2age5 wq3age5 wq4age5 wq5age5 wq2age6 wq3age6 wq4age6 wq5age6 wq2age7 wq3age7 wq4age7 wq5age7 age2 age3 age4 age5 age6 age7 caredu $sibcon r2 r3 if round==4 , r
est store grad6
ivreg2 csesscore psesscore ylparscore ylfriendscore ylparscoreage5 ylfriendscoreage5  wq2-wq5 age5 caredu $sibcon r2 r3 if round==4 , r
est store grad7
ivreg2 rosses psesscore ylparscore ylfriendscore ylparscoreage4 ylfriendscoreage4  wq2-wq5 age4 caredu $sibcon r2 r3 if round==4 , r
est store grad8

esttab fem0c fem1c urbc rurc grad1 using sibhet.rtf, replace ///
compress nonum nonotes noobs mtitles("Males" "Females" "Urban" "Rural" "Old/Young Gradient") ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum onecell ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)

esttab fem0 fem1 urb rur grad2 using sibhet.rtf, replace append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum onecell ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon r2 r3)  label b(a2)


********************************************************************************
**Sibling Differences**
********************************************************************************

xtivreg2 rosses ylparscore ylfriendscore female border ageyr zppvt chheight, fe cluster(clustid)
est store sibdiffros
*boottest {ylparscore} {ylfriendscore}
xtivreg2 csesscore ylparscore ylfriendscore female border ageyr zppvt chheight, fe cluster(clustid)
est store sibdiffpride
*boottest {ylparscore} {ylfriendscore}
xtivreg2 zppvt rosses csesscore ylparscore ylfriendscore female ageyr chheight, fe cluster(clustid)
est store sibdiffzppvt

***Heterogeneous effects***
xtivreg2 csesscore ylparscore ylfriendscore border ageyr zppvt chheight if female==0, fe cluster(clustid)
est store sibdiffbrop
xtivreg2 rosses ylparscore ylfriendscore border ageyr zppvt chheight if female==0, fe cluster(clustid)
est store sibdiffbros
xtivreg2 csesscore ylparscore ylfriendscore border ageyr zppvt chheight if female==1, fe cluster(clustid)
est store sibdiffsisp
xtivreg2 rosses ylparscore ylfriendscore border ageyr zppvt chheight if female==1, fe cluster(clustid)
est store sibdiffsiss

gen diffsibs=female-l.female
bysort cid_num: replace diffsibs=diffsibs[_n+1] if diffsibs==.

/*Female is oldest sibling*/
xtivreg2 csesscore ylparscore ylfriendscore female border ageyr zppvt chheight if diffsibs!=0, fe cluster(clustid)
est store sibdiffsoldp
xtivreg2 rosses ylparscore ylfriendscore female border ageyr zppvt chheight if diffsibs!=0, fe cluster(clustid)
est store sibdiffsolds

/*Female is oldest sibling*/
xtivreg2 csesscore ylparscore ylfriendscore border ageyr zppvt chheight if diffsibs==-1, fe cluster(clustid)
est store sibdiffboldp
xtivreg2 rosses ylparscore ylfriendscore border ageyr zppvt chheight if diffsibs==-1, fe cluster(clustid)
est store sibdiffbolds

/*Urban/Rural*/
xtivreg2 csesscore ylparscore ylfriendscore border ageyr zppvt chheight if typesite==0, fe cluster(clustid)
est store sibdiffurp
xtivreg2 rosses ylparscore ylfriendscore border ageyr zppvt chheight if typesite==0, fe cluster(clustid)
est store sibdiffburs
xtivreg2 csesscore ylparscore ylfriendscore border ageyr zppvt chheight if typesite==1, fe cluster(clustid)
est store sibdiffrup
xtivreg2 rosses ylparscore ylfriendscore border ageyr zppvt chheight if typesite==1, fe cluster(clustid)
est store sibdiffrus

foreach x of varlist csesscore rosses ylparscore ylfriendscore female border ageyr zppvt chheight {
	gen `x'sd=`x'-l.`x'
	}
reg csesscore psesscore wq2-wq5 caredu ylparscore ylfriendscore sib $sibcon r2 r3 , 
est store a
reg csesscoresd ylparscoresd ylfriendscoresd femalesd bordersd ageyrsd zppvtsd chheightsd, 
est store b
reg rosses psesscore wq2-wq5 caredu ylparscore ylfriendscore sib $sibcon r2 r3 , 
est store c
reg rossessd ylparscoresd ylfriendscoresd femalesd bordersd ageyrsd zppvtsd chheightsd, 
est store d

suest a b, cluster(clustid)
test [a_mean]ylparscore=[b_mean]ylparscoresd
test [a_mean]ylfriendscore=[b_mean]ylfriendscoresd

suest c d, cluster(clustid)
test [c_mean]ylparscore=[d_mean]ylparscoresd
test [c_mean]ylfriendscore=[d_mean]ylfriendscoresd

/*Sib Differences*/
/*Table 5*/
esttab sibpool1 sibdiffpride  using sibsd.rtf, replace ///
compress nonum onecell ///
mtitles("Pooled" "Sibling Difference") ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum nonotes noobs ///
order(wq5 ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon caredu psesscore r2 r3)  label b(a2)

esttab sibpool3 sibdiffros using sibsd.rtf, replace append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum ///
order(wq5 ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon caredu psesscore r2 r3)  label b(a2)

esttab sibpool1 sibdiffpride  using sibsd.tex, replace ///
compress nonum ///
mtitles("Pooled" "Sibling Difference") ///
refcat(wq5 "\emph{Panel A: Pride}", nol) nonum nonotes noobs ///
order(wq5 ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon caredu psesscore r2 r3)  label b(a2)

esttab sibpool3 sibdiffros using sibsd.tex, append ///
compress nonum ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) nomtitles nonum ///
order(wq5 ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons wq2 wq3 wq4 sib $sibcon caredu psesscore r2 r3)  label b(a2)

/*Table 6*/
esttab sibdiffbrop sibdiffsisp sibdiffsoldp sibdiffurp sibdiffrup  using sibrob.rtf, replace ///
compress nonum onecell mtitles("Brothers" "Sisters" "Differing Pairs" "Urban" "Rural") ///
refcat(ylparscore "\emph{Panel A: Pride}", nol) nonum nonotes ///
order(ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(border ageyr zppvt chheight)  label b(a2)

esttab sibdiffbros sibdiffsiss sibdiffsolds  sibdiffburs sibdiffrus using sibrob.rtf, append replace ///
compress nonum onecell mtitles("Brothers" "Sisters" "Urban" "Rural") ///
refcat(ylparscore "\emph{Panel B: Self-Esteem}", nol) nonum noobs ///
order(ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(border ageyr zppvt chheight)  label b(a2)

esttab sibdiffbrop sibdiffsisp sibdiffsoldp sibdiffurp sibdiffrup  using sibrob.tex, replace ///
compress nonum mtitles("Brothers" "Sisters" "Differing Pairs" "Urban" "Rural") ///
refcat(ylparscore "\emph{Panel A: Pride}", nol) nonum nonotes ///
order(ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(border ageyr zppvt chheight)  label b(a2)

esttab sibdiffbros sibdiffsiss sibdiffsolds  sibdiffburs sibdiffrus using sibrob.tex, append ///
compress nonum mtitles("Brothers" "Sisters" "Urban" "Rural") ///
refcat(ylparscore "\emph{Panel B: Self-Esteem}", nol) nonum noobs ///
order(ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(border ageyr zppvt chheight)  label b(a2)

esttab sibdiffsoldp sibdiffboldp  using sibextra.rtf, replace ///
compress nonum onecell mtitles("Older Sister" "Older Brother" ) ///
refcat(ylparscore "\emph{Panel A: Pride}", nol) nonum noobs ///
order(ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(border ageyr zppvt chheight)  label b(a2)

esttab  sibdiffsolds sibdiffbolds using sibextra.rtf, replace append ///
compress nonum onecell mtitles("Older Sister" "Older Brother" ) ///
refcat(ylparscore "\emph{Panel B: Self-Esteem}", nol) nonum noobs ///
order(ylparscore ylfriendscore) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(border ageyr zppvt chheight)  label b(a2)


log close

********************************************************************************
**Quantile Regressions**
********************************************************************************

use p3sibs, clear

/*qui qreg csesscore wq2-wq5 psesscore caredu $con r2 r3 if round==4,
grqreg psesscore wq2-wq5, ci ols olsci reps(100)
qui qreg csefscore wq2-wq5 psefscore caredu $con r2 r3 if round==4, 
grqreg psefscore wq2-wq5, ci ols olsci reps(100)
qui qreg csesscore wq2-wq5 psesscore caredu $con r2 r3 if round==3, 
grqreg psesscore wq2-wq5, ci ols olsci reps(100)
qui qreg csefscore wq2-wq5 psefscore caredu $con r2 r3 if round==3, 
grqreg psefscore wq2-wq5, ci ols olsci reps(100)*/


ivreg2 csesscore wq2-wq5 psesscore ylparscore ylfriendscore caredu sib $sibcon r2 r3 if round==4, cluster(clustid) 
est store a0
mat bsesr4 =e(b)
svmat double bsesr4, names(matcol)

ivreg2 csesscore wq2-wq5 ylparscore ylfriendscore psesscore caredu $sibcon round r2 r3, cluster(clustid) 
est store a0p

ivreg2 csefscore wq2-wq5 ylparscore ylfriendscore psefscore caredu sib $sibcon r2 r3 if round==4, cluster(clustid) 
est store a04
mat bsefr4 =e(b)
svmat double bsefr4, names(matcol)

ivreg2 csefscore wq2-wq5 psefscore caredu sib $sibcon round r2 r3 , cluster(clustid) 
est store a04p

set seed 1234

qui rifreg csesscore wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, quantile(0.9) bootstrap reps(100) 
est store a1
qui rifreg csesscore wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, quantile(0.5) bootstrap reps(100) 
est store a2
qui rifreg csesscore wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, quantile(0.1) bootstrap reps(100) 
est store a3
qui rifreg csesscore wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, var bootstrap reps(100) 
est store a07


qui rifreg rosses wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, quantile(0.9) bootstrap reps(100) 
est store a08
qui rifreg rosses wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, quantile(0.5) bootstrap reps(100) 
est store a9
qui rifreg rosses wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, quantile(0.1) bootstrap reps(100) 
est store a10
qui ivreg2 rosses wq2-wq5 ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, cluster(clustid)  
est store a11

/*Table 4*/
esttab a0 a3 a2 a1 using sibsrif.rtf, replace ///
refcat(wq5 "\emph{Panel A: Pride}", nol) compress nonum nonotes noobs ///
mtitles("OLS" "10th Percentile" "50 Percentile" "90th Percentile") ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons sib wq2 wq3 wq4 $sibcon r2 r3)  label b(a2) onecell

esttab a11 a10 a9 a08 using sibsrif.rtf, replace append ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) compress nonum nomtitles ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons sib wq2 wq3 wq4 $sibcon r2 r3)  label b(a2) onecell

esttab a0 a3 a2 a1 using sibsrif.tex, replace ///
refcat(wq5 "\emph{Panel A: Pride}", nol) compress nonum nonotes noobs ///
mtitles("OLS" "10th Percentile" "50 Percentile" "90th Percentile") ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons sib wq2 wq3 wq4 $sibcon r2 r3)  label b(a2)

esttab a11 a10 a9 a08 using sibsrif.tex, append ///
refcat(wq5 "\emph{Panel B: Self-Esteem}", nol) compress nonum nomtitles ///
order(wq5 psesscore ylparscore ylfriendscore caredu) ci(a1) star(* 0.1 ** 0.05 *** 0.01) drop(_cons sib wq2 wq3 wq4 $sibcon r2 r3)  label b(a2)


********************************************************************************
/*Oaxaca-Blinder, poor*/
********************************************************************************
cd "/users/jBook/Desktop/School/Paper3/"
use p3sibs, clear
set more off

kdensity csesscore if poor==0, gen(evalnp1 densnp1) width(0.10) nograph 
kdensity csesscore if poor==1, gen(evalp1 densp1) width(0.10) nograph 
set scheme s1color
label var evalp1 "Self-Esteem"
label var evalnp1 "Self-Esteem"
graph twoway  (connected densp1 evalp1, msymbol(i) lpattern(dash) clwidth(medium) lc(red) )	///
   (connected densnp1  evalnp1, msymbol(i) lpattern(longdash) clwidth(medium) lc(blue) )  ///
     , ytitle("Density")ylabel(0.0 0.2 0.4 0.6 0.8 1.0) ///
      xlabel(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)  ///
      legend(pos(7) col(2) lab(1 "Poor")  lab(2 "Non-poor")   /// 
      symxsize(8) keygap(1) textwidth(34) ) ///
      saving(cses_dens,replace)

forvalues qt = 10(40)90 {	
   gen rif_`qt'ses=.
}
pctile eval1=csesscore if poor==1 , nq(100) 
kdensity csesscore if poor==1, at(eval1) gen(evalp densp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalp[`qt']+`qc'/densp[`qt'] if csesscore>=evalp[`qt'] & poor==1
 replace rif_`qt'=evalp[`qt']-(1-`qc')/densp[`qt'] if csesscore<evalp[`qt'] & poor==1
}
pctile eval2=csesscore if poor==0, nq(100) 
kdensity csesscore if poor==0, at(eval2) gen(evalnp densnp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'ses=evalnp[`qt']+`qc'/densnp[`qt'] if csesscore>=evalnp[`qt'] & poor==0
 replace rif_`qt'ses=evalnp[`qt']-(1-`qc')/densnp[`qt'] if csesscore<evalnp[`qt']& poor==0
}
sort poor
by poor: sum rif_10ses rif_50ses rif_90ses

forval qt = 10(40)90 {
di "doing quantile " `qt'
oaxaca rif_`qt'ses ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4, by(poor) w(1) 
est store rif`qt'
matrix Rt`qt'=e(b)
}

matrix DR9010=Rt90-Rt10
matrix DR9050=Rt90-Rt50
matrix DR5010=Rt50-Rt10
matrix list DR9010
matrix list DR9050
matrix list DR5010

use p3sibs, clear
keep if round==4

forvalues qt = 10(40)90 {	
   gen rif_`qt'ses=.
}
pctile eval1=rosses if poor==1 , nq(100) 
kdensity rosses if poor==1, at(eval1) gen(evalp densp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalp[`qt']+`qc'/densp[`qt'] if rosses>=evalp[`qt'] & poor==1
 replace rif_`qt'=evalp[`qt']-(1-`qc')/densp[`qt'] if rosses<evalp[`qt'] & poor==1
}
pctile eval2=rosses if poor==0, nq(100) 
kdensity rosses if poor==0, at(eval2) gen(evalnp densnp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'ses=evalnp[`qt']+`qc'/densnp[`qt'] if rosses>=evalnp[`qt'] & poor==0
 replace rif_`qt'ses=evalnp[`qt']-(1-`qc')/densnp[`qt'] if rosses<evalnp[`qt']& poor==0
}
sort poor
by poor: sum rif_10ses rif_50ses rif_90ses

sum poor 
forvalues qt = 10(40)90 {	
   sum rif_`qt' if poor==0
   gen qlwnp`qt'=r(mean)
   sum rif_`qt' if poor==1
   gen qlwp`qt'=r(mean)
   gen difq`qt'= qlwnp`qt'-qlwp`qt'
   display difq`qt'

di "doing quantile " `qt'  
oaxaca rif_`qt'ses  ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3, by(poor) w(1) 
est stor rif`qt'sef
}


bs, reps(100): oaxaca csesscore ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 , by(poor) w(1)
est store ob1
bs, reps(100): oaxaca rosses ylparscore ylfriendscore psesscore caredu sib $sibcon r2 r3 if round==4,  by(poor) w(1)
est store ob2

/*Table 7*/
esttab ob1 rif10 rif50 rif90  using rifd.rtf ///
, ci(a2) label b(a2) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Table 10: Quantile Decomposition, Age 12-13") coeflabel(group_1 "Non-poor" group_2 "poor" grouphh "Household Controls" groupchild "Child Controls") /// 
compress onecell drop(_cons sib $sibcon r2 r3) order(psesscore ylparscore ylfriendscore caredu)  nonote ///
replace mtitles("Oaxaca-Blinder" "10th Percentile" "50th Percentile" "90th Percentile" )

esttab ob2 rif10sef rif50sef rif90sef  using rifd.rtf ///
, ci(a2) label b(a2) star(* 0.1 ** 0.05 *** 0.01) ///
 title("Table 10: Quantile Decomposition, Age 12-13") coeflabel(group_1 "Non-poor" group_2 "poor" grouphh "Household Controls" groupchild "Child Controls") /// 
compress onecell drop(_cons sib $sibcon r2 r3) order(psesscore ylparscore ylfriendscore caredu)  addnote("Households declared poor if in the lower quintile of wealth" "Controls listed in Table 1" ) ///
append replace mtitles("Oaxaca-Blinder" "10th Percentile" "50th Percentile" "90th Percentile" )

esttab ob1 rif10 rif50 rif90  using rifd.tex ///
, ci(a2) label b(a2) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Table 10: Quantile Decomposition, Age 12-13") coeflabel(group_1 "Non-poor" group_2 "poor" grouphh "Household Controls" groupchild "Child Controls") /// 
compress drop(_cons sib $sibcon r2 r3) order(psesscore ylparscore ylfriendscore caredu)  nonote ///
replace mtitles("Oaxaca-Blinder" "10th Percentile" "50th Percentile" "90th Percentile" )

esttab ob2 rif10sef rif50sef rif90sef  using rifd.tex ///
, ci(a2) label b(a2) star(* 0.1 ** 0.05 *** 0.01) ///
 title("Table 10: Quantile Decomposition, Age 12-13") coeflabel(group_1 "Non-poor" group_2 "poor" grouphh "Household Controls" groupchild "Child Controls") /// 
compress drop(_cons sib $sibcon r2 r3) order(psesscore ylparscore ylfriendscore caredu)  addnote("Households declared poor if in the lower quintile of wealth" "Controls listed in Table 1" ) ///
append mtitles("Oaxaca-Blinder" "10th Percentile" "50th Percentile" "90th Percentile" )




********************************************************************************
/*Oaxaca-Blinder, typesite*/
********************************************************************************
estimates clear
cd "/users/jBook/Desktop/School/Paper3/"
use p3sibs, clear
set more off

kdensity csesscore if typesite==0, gen(evalnp1 densnp1) width(0.10) nograph 
kdensity csesscore if typesite==1, gen(evalp1 densp1) width(0.10) nograph 
set scheme s1color
label var evalp1 "Self-Esteem"
label var evalnp1 "Self-Esteem"
graph twoway  (connected densp1 evalp1, msymbol(i) lpattern(dash) clwidth(medium) lc(red) )	///
   (connected densnp1  evalnp1, msymbol(i) lpattern(longdash) clwidth(medium) lc(blue) )  ///
     , ytitle("Density")ylabel(0.0 0.2 0.4 0.6 0.8 1.0) ///
      xlabel(1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)  ///
      legend(pos(7) col(2) lab(1 "Rural")  lab(2 "Urban")   /// 
      symxsize(8) keygap(1) textwidth(34) ) ///
      saving(cses_dens,replace)

forvalues qt = 10(40)90 {	
   gen rif_`qt'ses=.
}
pctile eval1=csesscore if typesite==1 , nq(100) 
kdensity csesscore if typesite==1, at(eval1) gen(evalp densp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalp[`qt']+`qc'/densp[`qt'] if csesscore>=evalp[`qt'] & typesite==1
 replace rif_`qt'=evalp[`qt']-(1-`qc')/densp[`qt'] if csesscore<evalp[`qt'] & typesite==1
}
pctile eval2=csesscore if typesite==0, nq(100) 
kdensity csesscore if typesite==0, at(eval2) gen(evalnp densnp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'ses=evalnp[`qt']+`qc'/densnp[`qt'] if csesscore>=evalnp[`qt'] & typesite==0
 replace rif_`qt'ses=evalnp[`qt']-(1-`qc')/densnp[`qt'] if csesscore<evalnp[`qt']& typesite==0
}
sort typesite
by typesite: sum rif_10ses rif_50ses rif_90ses

forval qt = 10(40)90 {
di "doing quantile " `qt'
oaxaca rif_`qt'ses poor ylparscore ylfriendscore psesscore caredu sib ageyr border hhsize female adverse zppvt chheight r2 r3 if round==4, by(typesite) w(1) 
est store rif`qt'
matrix Rt`qt'=e(b)
}

matrix DR9010=Rt90-Rt10
matrix DR9050=Rt90-Rt50
matrix DR5010=Rt50-Rt10
matrix list DR9010
matrix list DR9050
matrix list DR5010

use p3sibs, clear
keep if round==4

forvalues qt = 10(40)90 {	
   gen rif_`qt'ses=.
}
pctile eval1=rosses if typesite==1 , nq(100) 
kdensity rosses if typesite==1, at(eval1) gen(evalp densp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'=evalp[`qt']+`qc'/densp[`qt'] if rosses>=evalp[`qt'] & typesite==1
 replace rif_`qt'=evalp[`qt']-(1-`qc')/densp[`qt'] if rosses<evalp[`qt'] & typesite==1
}
pctile eval2=rosses if typesite==0, nq(100) 
kdensity rosses if typesite==0, at(eval2) gen(evalnp densnp) width(0.10) nograph 
forvalues qt = 10(40)90 {	
 local qc = `qt'/100.0
 replace rif_`qt'ses=evalnp[`qt']+`qc'/densnp[`qt'] if rosses>=evalnp[`qt'] & typesite==0
 replace rif_`qt'ses=evalnp[`qt']-(1-`qc')/densnp[`qt'] if rosses<evalnp[`qt']& typesite==0
}
sort typesite
by typesite: sum rif_10ses rif_50ses rif_90ses

sum typesite 
forvalues qt = 10(40)90 {	
   sum rif_`qt' if typesite==0
   gen qlwnp`qt'=r(mean)
   sum rif_`qt' if typesite==1
   gen qlwp`qt'=r(mean)
   gen difq`qt'= qlwnp`qt'-qlwp`qt'
   display difq`qt'

di "doing quantile " `qt'  
oaxaca rif_`qt'ses poor ylparscore ylfriendscore psesscore caredu sib ageyr border hhsize female adverse zppvt chheight r2 r3, by(typesite) w(1) 
est stor rif`qt'sef
}


bs, reps(100): oaxaca csesscore poor ylparscore ylfriendscore psesscore caredu sib ageyr border hhsize female adverse zppvt chheight r2 r3 , by(typesite) w(1)
est store ob3
bs, reps(100): oaxaca rosses poor ylparscore ylfriendscore psesscore caredu sib ageyr border hhsize female adverse zppvt chheight r2 r3 if round==4,  by(typesite) w(1)
est store ob4

/*Table 8*/

esttab ob3 rif10 rif50 rif90  using rift.rtf ///
, ci(a1) label b(a2) star(* 0.1 ** 0.05 *** 0.01)  ///
 title("Table 10: Quantile Decomposition, Age 12-13") coeflabel(group_1 "Non-typesite" group_2 "typesite" grouphh "Household Controls" groupchild "Child Controls") /// 
compress onecell drop(_cons sib ageyr border hhsize female adverse zppvt chheight r2 r3) order(psesscore ylparscore ylfriendscore caredu)  nonote ///
replace mtitles("Oaxaca-Blinder" "10th Percentile" "50th Percentile" "90th Percentile" )

esttab ob4 rif10sef rif50sef rif90sef  using rift.rtf ///
, ci(a1) label b(a2) star(* 0.1 ** 0.05 *** 0.01) ///
 title("Table 10: Quantile Decomposition, Age 12-13") coeflabel(group_1 "Non-typesite" group_2 "typesite" grouphh "Household Controls" groupchild "Child Controls") /// 
compress onecell drop(_cons sib ageyr border hhsize female adverse zppvt chheight r2 r3) order(psesscore ylparscore ylfriendscore caredu)  addnote("Households declared poor if in the lower quintile of wealth" "Controls listed in Table 1" ) ///
append replace mtitles("Oaxaca-Blinder" "10th Percentile" "50th Percentile" "90th Percentile" )

********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
**============================================================================**
**============================================================================**
**STATA user created programme citations                                      **
**============================================================================**
**--Baum, C.F., Schaffer, M.E., Stillman, S. 2010.  ivreg2: Stata module for extended instrumental variables/2SLS, GMM and
**   AC/HAC, LIML and k-class regression.  http://ideas.repec.org/c/boc/bocode/s425401.html

**Schaffer, M.E., 2010.  xtivreg2: Stata module to perform extended IV/2SLS, GMM and AC/HAC, LIML and k-class
**	 regression for panel data models.  http://ideas.repec.org/c/boc/bocode/s456501.html

**Jann, Ben. 2004."ESTOUT: Stata module to make regression tables," Statistical Software Components
**	 S439301, Boston College Department of Economics, revised 02 Feb 2017.
**	 <https://ideas.repec.org/c/boc/bocode/s439301.html>

**Jann,Ben. 2008."OAXACA: Stata module to compute the Blinder-Oaxaca decomposition," Statistical Software Components
**	 S456936, Boston College Department of Economics, revised 25 Aug 2011.
**	 <https://ideas.repec.org/c/boc/bocode/s456936.html>
**============================================================================**
**============================================================================**
** I used STATA/IC for Mac version 12.0. Any errors are mine and mine alone.  **
** John F. Creamer, Heriot-Watt University, 2017.                                **
**============================================================================**


