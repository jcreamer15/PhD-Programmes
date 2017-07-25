********************************************************************************
**Data Prep do-file, "The importance of family and friends on human capital development in the mid-childhood and early adolesence"
********************************************************************************
set more off
cd "/users/jBook/Desktop/School/Paper3/"

use "/users/jBook/Desktop/School/Paper3/R4together_edit.dta", clear
drop dadlive
rename WRRYFDR4 WRRYFD

append using "/users/jBook/Desktop/School/Paper3/R3together_ready"
sort childcode round
by childcode: replace childid=childid[_n-1] if round==4


egen cid_num= group(childcode) 
order cid_num round hhsize agemon wi  z* sib* ,  after(childid)
tsset cid_num round

merge 1:1 childcode round using "/users/jBook/Desktop/School/Paper3/UKDA-7931-stata11/stata11/peru_R4/hh_yc/pe_r4_ychh_youngerhousehold.dta", ///
keepusing(RPTGRDR4 MFNRVSR4 MDIGPRR4 MTRBCRR4 MUNHPYR4 MCRYMRR4 MDFENJR4 MDFDECR4 MWRKSFR4 MNOPRTR4 ///
MLSINTR4 MWRTHLR4 MNOGOR4 MTIREDR4 MUNSTMR4 MESYTRR4 MVDLOCR4 BULLDR4 )  
drop _merge
replace BULLD=BULLDR4 if round==4

merge 1:1 childcode round using "/users/jBook/Desktop/School/Paper3/UKDA-7931-stata11/stata11/peru_R4/ch_yc/pe_r4_ycch_youngerchild.dta", ///
keepusing(NOHMWKR4 MISSCHR4 )  
drop _merge


********************************************************************************
**Merging regions and clusters from constructed files**
********************************************************************************

merge m:1 childid round using "/users/jBook/Desktop/School/Paper3/peru_constructedR4e.dta", ///
keepusing(dint panel1234 chheight chgrade momage chgrade chprob chlang careage momedu dadedu momlive dadlive shfam* clustid placeid typesite region schtype juntos caredu carerel caresex) update replace
keep if _merge>=3
drop _merge

**To make data unique, dropped 10 duplicates**

merge 1:1 childcode round using "/users/jBook/Desktop/School/Paper3/UKDA-7931-stata11/stata11/peru_R4/hh_yc/householdrosterr4e.dta", ///
keepusing(NUMBOKR4 TXTFUNR4  HLPWRKR4 AGESTR4) 
drop _merge 
replace numbooks=NUMBOKR4 if round==4
recode numbooks HLPWRKR4 (77=.) 

**Merging Siblings Info**
merge m:1 childcode round using "/users/jBook/Desktop/School/Paper3//UKDA-7931-stata11/stata11/peru_R4/hh_yc/pe_r4_ychh_childmobilitye.dta", ///
keepusing(YRMOVER4* ) 
drop _merge
replace S1GND=S1SEXR3 if round==3
bysort childcode: replace S1GND=S1SEXR3[_n-1] if round==4 & S1GND==.
replace sibage=sibage*12
replace sibage=sibage[_n-1] +48 if round==4
replace sib1agemon=sibage if round==4 & sib1agemon==.


gen sdage1=.
replace sdage1=agemon if round==3
replace sdage1=sib1agemon if round==4
replace sdage1=sib2agemon if sdage1==.
label var sdage1 "Age Difference"


replace totalexp=foodexp+nfoodexp if round==4
rename CLDSTD asp
recode asp EXPJOB cladder LADDER (77=.) (79=.) (88=.)
recode FARLAD (10=9) (14=9) (15=9) (77=.) (79=.) (99=.)
recode schtype hplay (99=.)
recode GRDLKE  YRMOVER4* carerel momlive dadlive (77=.) (79=.) (88=.) (99=.)
recode PEDRUNK CNSDRKR3 AGGDRKR3 (2=1) (1=2) (88=.)
label define drunk 0 "No" 1 "Sometimes" 2 "Yes"
label values PEDRUNK CNSDRKR3 AGGDRKR3 drunk  
recode typesite WANTCLD (1=0) (2=1) (88=.) (99=.)
label define yesno 0 "No" 1 "Yes"
label values WANTCLD yesno
label define urban 0 "Urban" 1 "Rural"
label values typesite urban
recode chlang (31=0) (32=1) (35=0) (37=1)
replace chlang=0 if chlang==. & round==4
label var chlang "Native language Quechua"
replace CAMBTN=CAMBTNR3 if round==3

replace agemon=round(agemon,1)

drop LLORA LCARGA LTRANQ LMECE DAGUA DPALM LSAC LPELL LAMEN LMED LDMED LDPECH LPBOC LESAB LNADA LOTRO 

**Creating Wealth and Expenditure terciles/quartiles**
xtile quart4=wi if round==4, n(4) 
xtile quart3=wi if round==3, n(4)
xtile quart2=wi if round==2, n(4) 
xtile quart1=wi if round==1, n(4) 
gen quart=.
	foreach t of numlist 1/4{
		replace quart=quart`t' if round==`t'
		}
tab quart, gen(q)
label var q1 "Lower 25% of wealth "
label var q2 "25-50% of wealth"
label var q3 "50-75% of wealth"
label var q4 "Upper 25% of wealth "

xtile wquint4=wi if round==4, n(5) 
xtile wquint3=wi if round==3, n(5) 
xtile wquint2=wi if round==2, n(5) 
xtile wquint1=wi if round==1, n(5) 
gen wquint=.	
	foreach t of numlist 1/4{
		replace wquint=wquint`t' if round==`t'
		}
tab wquint, gen(wq) 

label var wq1 "Bottom Quintile of wealth"
label var wq2 "Quintile 2 of wealth"
label var wq3 "Quintile 3 of wealth"
label var wq4 "Quintile 4 of wealth"
label var wq5 "Top Quintile of wealth"

gen poor=0
replace poor=1 if wquint==1

xtile quint4=totalexp if round==4, n(5) 
xtile quint3=totalexp if round==3, n(5) 
xtile quint2=totalexp if round==2, n(5) 
*xtile quint1=totalexp if round==1, n(5) 
gen quint=.
	foreach t of numlist 2/4{
		replace quint=quint`t' if round==`t'
		}
tab quint, gen(qu) 	
label var qu1 "Bottom Quintile of total expenditure"
label var qu2 "Quintile 2 of total expenditure"
label var qu3 "Quintile 3 of total expenditure"
label var qu4 "Quintile 4 of total expenditure"
label var qu5 "Top Quintile of total expenditure"

xtile fquint4=foodexp if round==4, n(5) 
xtile fquint3=foodexp if round==3, n(5) 
xtile fquint2=foodexp if round==2, n(5) 
*xtile quint1=totalexp if round==1, n(5) 
gen fquint=.
	foreach t of numlist 2/4{
		replace fquint=fquint`t' if round==`t'
		}
tab quint, gen(fqu) 	
label var fqu1 "Quintile 1 of total food expenditure"
label var fqu2 "Quintile 2 of total food expenditure"
label var fqu3 "Quintile 3 of total food expenditure"
label var fqu4 "Quintile 4 of total food expenditure"
label var fqu5 "Quintile 5 of total food expenditure"

xtile nfquint4=nfoodexp if round==4, n(5) 
xtile nfquint3=nfoodexp if round==3, n(5) 
xtile nfquint2=nfoodexp if round==2, n(5) 
*xtile quint1=totalexp if round==1, n(5) 
gen nfquint=.
	foreach t of numlist 2/4{
		replace nfquint=nfquint`t' if round==`t'
		}
tab quint, gen(nfqu) 	
label var nfqu1 "Quintile 1 of total non food expenditure"
label var nfqu2 "Quintile 2 of total non food expenditure"
label var nfqu3 "Quintile 3 of total non food expenditure"
label var nfqu4 "Quintile 4 of total non food expenditure"
label var nfqu5 "Quintile 5 of total non food expenditure"

xtile agemon4=agemon if round==4, n(4)
tab agemon4, gen(age)
	label var age1 "136-140 months"
	label var age2 "141-143 months"
	label var age3 "144-146 months"
	label var age4 "147-150 months"

xtile agemon3=agemon if round==3, n(4)
tab agemon3, gen(age3)
	label var age31 "88-92 months"
	label var age32 "93-95 months"
	label var age33 "96-99 months"
	label var age34 "100-103 months"	

drop lnexp
gen lnexp=ln(totalexp)
drop quint2 quint3 quint4 fquint2 fquint3 fquint4 nfquint2 nfquint3 nfquint4  quart1 quart2 quart3 quart4


**Cronbach's Alpha (full sample, no missings)**
alpha CAG1 CAG2 CAG5 if round>=3, item std
alpha CPS2 CPS3 CPS4 CPS5 if round>=3, item std
alpha CTRYHD CFTRWR CBRJOB CPLDEC CNOCHC, item std 
alpha CASHSH CASHCL CASHWK CCLTRG CEMBBK CWRUNI, item std

********************************************************************************
***Keeping Individuals who have answered all of the psycosocial questions***
********************************************************************************

**Marking missing responses**

recode CAG1 CAG2 CAG5  (77=.) (79=.) (88=.) (99=.)
recode CPS2 CPS3 CPS4 CPS5  (77=.) (79=.) (88=.) (99=.)
recode CTRYHD CFTRWR CBRJOB CPLDEC CNOCHC (77=.) (79=.) (88=.) (99=.) 
recode CASHCL CASHWK CASHSH CCLTRG CEMBBK CWRUNI (77=.) (79=.) (88=.) (99=.)

**Dropping children who are too old**
keep if agemon<=150 


**Creating binary variables for full resposnes**
gen fullpar=0 if round>=3
replace fullpar= (CPS2!=. & CPS3!=. & CPS4!=. & CPS5!=.) if round>=3

gen fullchild=0 if round>=3
	replace fullchild=( CASHCL!=. & CASHWK!=. & CASHSH!=. & CCLTRG!=. & CEMBBK!=. & CWRUNI!=.) if round>=3

gen fullsib=0 if round==4
	replace fullsib=( SASHCL!=. & SASHWK!=. & SASHSH!=. & SCLTRG!=. & SEMBBK!=. & SWRUNI!=.) if round==4

foreach x in FEAY03R4 FEAY07R4 FEAY10R4 FEAY13R4 FEAY19R4 FEAY25R4 FEAY29R4 FEAY02R4 FEAY09R4 FEAY12R4 ///
 FEAY16R4 FEAY20R4 FEAY24R4 FEAY34R4 FEAS02R4 FEAS06R4 FEAS08R4 FEAS11R4 FEAS12R4 FEAS15R4 FEAS20R4 FEAS23R4 ///
 FEAS01R4 FEAS04R4 FEAS07R4 FEAS10R4 FEAS13R4 FEAS16R4 FEAS17R4 FEAS19R4 FEAS22R4 {
	recode `x' (77=.) (79=.) (88=.) (99=.)
	*qui sum `x'
	*replace `x'=r(mean) if `x'==. & round==4
	}

gen fullpar1=0
	replace fullpar1=(FEAY03R4!=. & FEAY07R4 !=. & FEAY10R4 !=. & FEAY13R4!=. & FEAY19R4!=.& FEAY21R4!=. ///
	 & FEAY25R4!=. & FEAY29R4!=. & FEAS02R4!=. & FEAS06R4!=. & FEAS08R4!=. & FEAS11R4!=. & FEAS15R4!=. & FEAS17R4!=. & FEAS20R4!=. & FEAS23R4!=.) if round==4
gen fullfriend=0
	replace fullfriend=(FEAY02R4!=. & FEAY09R4!=. & FEAY12R4!=. & FEAY16R4!=. & FEAY20R4!=. & FEAY21R4!=. & FEAY24R4!=. & FEAY34R4!=. & FEAY31R4!=. & ///
	FEAS01R4!=. & FEAS04R4!=. & FEAS07R4!=. & FEAS13R4!=. & FEAS10R4!=. & FEAS13R4!=. & FEAS17R4!=. & FEAS19R4!=.) if round==4
	

**Generating macros**
global sef FEAY01R4 FEAY05R4 FEAY08R4 FEAY11R4 FEAY15R4 FEAY18R4 FEAY22R4 FEAY26R4 FEAY28R4 FEAY32R4 
global ses FEAY04R4 FEAY06R4 FEAY14R4 FEAY17R4 FEAY23R4 FEAY27R4 FEAY30R4 FEAY33R4 
global sibses FEAS03R4 FEAS05R4 FEAS09R4 FEAS12R4 FEAS18R4 FEAS21R4 FEAS14R4 FEAS24R4
global psycho FEAY01R4 FEAY05R4 FEAY08R4 FEAY11R4 FEAY15R4 FEAY18R4 FEAY22R4 FEAY26R4 FEAY28R4 FEAY32R4 ///
FEAY04R4 FEAY06R4 FEAY14R4 FEAY17R4 FEAY23R4 FEAY27R4 FEAY30R4 FEAY33R4 ///
FEAS03R4 FEAS05R4 FEAS09R4 FEAS12R4 FEAS18R4 FEAS21R4 FEAS14R4 FEAS24R4 FEAY31R4 FEAY21R4 FEAS16R4 FEAS22R4

foreach x in $psycho {
	recode `x' (77=.) (79=.) (88=.) (99=.)
	}


********************************************************************************
***Equalising self-esteem and self-efficacy observations***
********************************************************************************
gen rosfull=0 if round==4
	replace rosfull= (FEAY04R4!=. & FEAY06R4!=. & FEAY14R4!=. & FEAY17R4!=. & FEAY23R4!=. &  FEAY27R4!=. & FEAY30R4!=. &  FEAY33R4!=.) if round==4

gen srosfull=0 if round==4
	replace srosfull= (FEAS03R4!=. & FEAS05R4!=. & FEAS09R4!=. &  FEAS12R4!=. & FEAS18R4!=. & FEAS21R4!=.  & FEAS14R4!=. & FEAS24R4!=.) if round==4


sort cid_num round
replace clustid=. if round==4
bysort cid_num: replace clustid=clustid[_n-1] if round==4

by cid_num: replace agemon=agemon[_n-1]+48 if agemon==.


********************************************************************************
**Creating categorical variables and controls**
********************************************************************************

sort cid_num round
**Generating dummies of notable variables (wealth terciles, levels of education, etc.)**
*education dummies*
gen carepri=0 if caredu!=.
	replace carepri=1 if caredu>=0 & caredu<=8
gen caresec=0
	replace caresec=1 if caredu>8 & caredu<=10
gen careter=0
	replace careter=1 if caredu>10 & caredu<=22

label var carepri "Caregiver has completed up to primary education"
label var caresec "Caregiver has completed up to the secondary education"
label var careter "Caregiver has completed up to tertiary education"

gen mompri=0 if momedu!=.
	replace mompri=1 if momedu>=0 & momedu<=8
gen momsec=0
	replace momsec=1 if momedu>8 & momedu<=10
gen momter=0
	replace momter=1 if momedu>10 & momedu<=22

label var mompri "Mother has completed up to primary education"
label var momsec "Mother has completed up to the secondary education"
label var momter "Mother has completed up to tertiary education"

gen dadpri=0 if dadedu!=.
	replace dadpri=1 if dadedu>=0 & dadedu<=8
gen dadsec=0
	replace dadsec=1 if dadedu>8 & dadedu<=10
gen dadter=0
	replace dadter=1 if dadedu>10 & dadedu<=22

label var dadpri "Father has completed up to primary education"
label var dadsec "Father has completed up to the secondary education"
label var dadter "Father has completed up to tertiary education"
	
*caregivers relation to child*
gen carebi=0 if carerel!=.
	replace carebi=1 if carerel==1
gen caremom=0 if caresex!=.
	replace caremom=1 if carerel==1 & caresex==2
gen carenomom=0 
	replace carenomom=1 if caremom==0
				

label var caremom "Caregiver is mother"
label var carebi "Caregiver is a biological relative to the child"

**where does mom live**
gen maway=0 if momlive!=.
	replace maway=1 if momlive!=1
gen daway=0 if dadlive!=.
	replace daway=1 if dadlive!=1

label var maway "Mother lives away from the home"
label var daway "Father lives away from the home"	

**Aspirations Dummy**
gen aspdum=0 if asp!=.
replace aspdum=1 if asp==16 | asp==19
gen pargrade=0 if GRDLKE!=.
replace pargrade=1 if GRDLKE==16 | GRDLKE==19			

label var aspdum "Child aspires to university completion"
label var pargrade "Caregiver aspires for their children to complete university"


sort cid_num round
recode CRECH (2=0)

replace adverse=adverser4 if round==4
gen stunt=0
replace stunt=1 if zhfa<=-2

sum caredu if round==3
replace caredu=r(mean) if round==3 & caredu==.
sum caredu if round==2
replace caredu=r(mean) if round==2 & caredu==.

replace caredu=momedu if caremom==1 & caredu==. 
*recode caredu (18=.) (19=.) (20=.) (21=.) (22=.)


sort childcode round
replace border=border[_n+1] if round==3
foreach x of varlist BHOSPIT-ADVANTAG {
	tab `x'
	bysort childcode: replace `x'=`x'[_n+1] if `x'==.
	}


recode PEMOVED (88=0)
*replace migrate=PEMOVED if round==2

gen divorce=(shfam7==1)
tab clustid, gen(cluster)

drop MNPDAC-LSTLK momrel-NUMJNTR4 ///
ENDDINT  SNGSEXR4  TMCMWKR4-GENDYLR4 PPLRELR4-WRSHPR4 ///   
DINT  FTDIEDR4 LNGDADR4-WHRMUMR4 LVDMUMR4-COMMIDR3 SEE* ppvt S1WBRT S2WBRT NUMTHG NUM4YR HHNOW HH4YR

drop PELOAN-GETINF07 HMCARE* score_cog-TREATEXP3 FAMSON CFUTURJB  ///
DOCBRTH RQJNR305-TMINJR3 *_co TMONINR3-RISKAR32  ///
childid ladder CAMBTNR3 BHOSPIT LONGTERM-JOIN TRUST-ADVANTAG
**Regional Fixed Effects**
tab region, gen(r)
replace r2=r4 if r4==1
replace r1=1 if region==.
replace r2=0 if region==.
replace r3=0 if region==.
drop r4



********************************************************************************
***Parents non-cog***
********************************************************************************
**Self-efficacy**
keep if round==4
*keep if SIBEXT==1
drop if fullpar==0
drop if fullchild==0
drop if fullpar1==0
drop if fullfriend==0
drop if fullsib==0

drop if srosfull==0
drop if rosfull==0
drop if ppvtraw==.
drop if sppvt==.

gen sdgend=.
recode S1GNDR (2=1)(1=0) , gen(sibsex)
replace sdgend=sibsex if round==4
label var sdgend "Gender"

gen ageyr=agemon/12
	replace ageyr=round(ageyr,1)

**Cognitive measures*
bysort agemon: egen sdam=sd(ppvtraw) 
bysort agemon: egen mppvt=mean(ppvtraw)
bysort agemon: egen sdm=sd(math) 
bysort agemon: egen mmath=mean(math)
bysort agemon: egen sdl=sd(lang_raw) 
bysort agemon: egen mlang=mean(lang_raw)

gen double zppvt=(ppvtraw-mppvt)/sdam
label var zppvt "Age standardised PPVT score"

gen double zmath=(math-mmath)/sdm
label var zmath "Age standardised Math score"

gen double zlang=(lang_raw-mlang)/sdl
label var zlang "Age standardised Language score"

drop if zppvt==. 

drop sdam mppvt sdm mmath sdl mlang

sum sppvt
qui sum sib1agemon
gen agemon2=round(sib1agemon,1)
gen ageyr2=sib1agemon/12
	replace ageyr2=round(ageyr2,1)
bysort agemon2: egen sdams=sd(sppvt) 
bysort agemon2: egen msppvt=mean(sppvt)
gen double zsppvt=(sppvt-msppvt)/sdams
label var zsppvt "Age standardised PPVT score, siblings"
gen sdborder=border+1 if round==4
label var sdborder "Birth Order"

recode S1HGHT (-999=.)
sum S1HGHT

**Prelim Maternal Depression measure**
alpha MFNRVSR4 MDIGPRR4 MTRBCRR4 MUNHPYR4 MCRYMRR4 MDFENJR4 MDFDECR4 MWRKSFR4 MNOPRTR4 MLSINTR4 MWRTHLR4 MNOGOR4 MTIREDR4 MUNSTMR4 MESYTRR4
foreach x of varlist MFNRVSR4 MDIGPRR4 MTRBCRR4 MUNHPYR4 MCRYMRR4 MDFENJR4 MDFDECR4 MWRKSFR4 MNOPRTR4 MLSINTR4 MWRTHLR4 MNOGOR4 MTIREDR4 MUNSTMR4 MESYTRR4 {
	egen z`x'=std(`x')
	}
gen mdsum=(MFNRVSR4 + MDIGPRR4 + MTRBCRR4 + MUNHPYR4 + MCRYMRR4 + MDFENJR4 + MDFDECR4 + MWRKSFR4 + MNOPRTR4 + MLSINTR4 + MWRTHLR4 + MNOGOR4 + MTIREDR4 + MUNSTMR4 + MESYTRR4)/15 if round==4	
gen mdindexa=(MFNRVSR4==1 | MDIGPRR4==1 | MTRBCRR4==1 | MUNHPYR4==1 | MCRYMRR4==1 | MDFENJR4==1 | MDFDECR4==1 | MWRKSFR4==1 | MNOPRTR4==1 |MLSINTR4==1 | MWRTHLR4==1 | MNOGOR4==1 | MTIREDR4==1 | MUNSTMR4==1 | MESYTRR4==1) if round==4 

recode CAG4 CAG5  (5=1) (4=2) (2=4) (1=5) , 
label define reverse 1 "Strongly Agree" 2 "Agree" 3 "More or Less" 4 "Disagree" 5 "Strongly Disagree"
label values CAG4 CAG5 reverse
	
foreach x in CAG1 CAG2 CAG5 {
	egen double z`x'=std(`x') if round==4
	}
gen double psefscore = (zCAG1+zCAG2+zCAG5)/3
label var psefscore "Caregiver Self-Efficacy"
gen double psefscoret = (CAG1+CAG2+CAG5)/3
label var psefscoret "Caregiver Self-Efficacy, Raw Score"

*replace psefscoret= (sCAG1+sCAG2+sCAG5)/3 if round==2	
alpha CAG1 CAG2 CAG3 CAG4 CAG5, item
alpha CAG1 CAG2 CAG5, item

********************************************************************************
**Maternal Pride**
********************************************************************************

foreach x in CPS2 CPS3 CPS4 CPS5 {
	egen double z`x'=std(`x') if round==4
	}			

gen double psesscore= (zCPS2+zCPS3+zCPS4+zCPS5)/4
label var psesscore "Caregiver Self-Esteem"
gen double psesscoret= (CPS2+CPS3+CPS4+CPS5)/4
label var psesscoret "Caregiver Self-Esteem, Raw Score"

alpha CPS1 CPS2 CPS3 CPS4 CPS5, item
alpha CPS2 CPS3 CPS4 CPS5, item 


********************************************************************************
***YL child and Sibling Pride***
********************************************************************************

********************************************************************************
**Index Child Pride**
********************************************************************************

local se CASHSH CASHCL CASHWK CCLTRG CEMBBK CWRUNI

foreach x of local se {
	egen double z`x'=std(`x') if round==4
	}
gen double csesscore=(zCASHCL + zCASHWK + zCASHSH + zCCLTRG  + zCEMBBK + zCWRUNI )/6
la var csesscore "YL Child Self-Esteem"
gen double csesscoret=(CASHCL + CASHWK + CASHSH + CCLTRG  + CEMBBK + CWRUNI )/6
la var csesscoret "YL Child Self-Esteem, Raw Score"

alpha CASHSH CASHCL CASHWK CCLTRG CEMBBK CWRUNI, item


order CASHSH CASHCL CASHWK CCLTRG CEMBBK CWRUNI CTRYHD CFTRWR CBRJOB CPLDEC CNOCHC, after(wi)

  

********************************************************************************
**Sibling Pride**
********************************************************************************

recode SASHCL SASHWK SASHSH SCLTRG SEMBBK SWRUNI (77=.) (79=.) (88=.) (99=.)
gen fullscses=(SASHCL!=. | SASHWK!=. | SASHSH!=. | SCLTRG!=. | SEMBBK!=. | SWRUNI!=.)

local sibse SASHCL SASHWK SASHSH SCLTRG SEMBBK SWRUNI
foreach x of local sibse {
	egen double z`x'=std(`x')
	}	
		
gen double ssesscore=(zSASHCL  + zSASHWK + zSASHSH + zSCLTRG  + zSEMBBK + zSWRUNI )/6
la var ssesscore "Sibling Self-Esteem"
gen double ssesscoret=(SASHCL  + SASHWK + SASHSH + SCLTRG  + SEMBBK + SWRUNI )/6
alpha SASHCL SASHWK SASHSH SCLTRG SEMBBK SWRUNI, item


order SIBEXT SIBENR SASHCL SASHWK SPLDEC SNOCHC SCLTRG STRYHD SFTRWR SEMBBK ///
SASHSH SBRJOB SWRUNI SIBID SBAGYRMN STESTLNG S1ID S1SCLE S1DAY S1MTH ///
 S1YEAR S1GNDR S1WGHT S1N0WG S1HGHT S1N0HG S2ID S2SCLE S2DAY S2MTH ///
 S2YEAR S2GNDR S2WGHT S2N0WG S2HGHT S2N0HG, after(zSWRUNI)
 

**Child-Parent opinion(?) score**



qui sum FEAY21R4
replace FEAY21R4=r(mean) if FEAY21R4==.	
qui sum FEAY31R4
replace FEAY31R4=r(mean) if FEAY31R4==.
		
foreach x of varlist FEAY03R4 FEAY07R4 FEAY10R4 FEAY13R4 FEAY19R4 FEAY21R4 FEAY25R4 FEAY29R4 FEAY02R4 FEAY09R4 FEAY12R4 FEAY16R4 FEAY20R4 FEAY24R4 FEAY31R4 FEAY34R4 ///
FEAS01R4 FEAS03R4 FEAS04R4 FEAS07R4 FEAS10R4 FEAS13R4 FEAS16R4 FEAS17R4 FEAS19R4 ///
FEAS02R4 FEAS05R4 FEAS06R4 FEAS08R4 FEAS09R4 FEAS11R4 FEAS12R4 FEAS14R4 FEAS18R4 FEAS15R4 FEAS20R4 FEAS21R4 FEAS22R4 FEAS23R4 FEAS24R4 /// 
{
	replace `x'=((4/3)*`x')-(1/3)
	qui sum `x'
	gen z`x'=(`x'-r(mean))/r(sd)
	}

********************************************************************************
**Sib psychosocial**
********************************************************************************
**Sibling Self-Esteem**

alpha FEAS03R4 FEAS05R4 FEAS09R4 FEAS12R4 FEAS18R4 FEAS21R4 FEAS14R4 FEAS24R4, item /*0.69*/
 
gen avgsros= (FEAS03R4 + FEAS05R4 + FEAS09R4 + FEAS12R4 + FEAS18R4 + FEAS21R4 + FEAS14R4 + FEAS24R4) /8 
gen sros=(zFEAS03R4 + zFEAS05R4 + zFEAS09R4 + zFEAS12R4 + zFEAS18R4 + zFEAS21R4 + zFEAS14R4 + zFEAS24R4)/8
	label var sros "Sibling Rosenberg Scale"


********************************************************************************
**Parental and Peer Relationship Variables**
********************************************************************************

alpha FEAY03R4 FEAY07R4 FEAY10R4 FEAY13R4 FEAY19R4 FEAY21R4 FEAY25R4 FEAY29R4 /*0.77*/

gen avgylpar=(FEAY03R4 + FEAY07R4 + FEAY10R4 + FEAY13R4 + FEAY19R4 + FEAY21R4 + FEAY25R4 + FEAY29R4) / 8	
gen ylparscore=(zFEAY03R4 + zFEAY07R4 + zFEAY10R4 + zFEAY13R4 + zFEAY19R4 + zFEAY21R4 + zFEAY25R4 + zFEAY29R4)/8
qui sum ylparscore
replace ylparscore=r(mean) if ylparscore==. & round==4
label var ylparscore "Parent/Child relationship score"

alpha FEAS02R4 FEAS06R4 FEAS08R4 FEAS11R4 FEAS15R4 FEAS17R4 FEAS20R4 FEAS23R4 /*0.82*/
gen avgsibpar=(FEAS02R4 + FEAS06R4 + FEAS08R4 + FEAS11R4 + FEAS15R4 + FEAS17R4 + FEAS20R4 + FEAS23R4)/8
gen sibparscore=(zFEAS02R4+zFEAS06R4+zFEAS08R4+zFEAS11R4+zFEAS15R4+ zFEAS17R4 + zFEAS20R4+zFEAS23R4)/8
gen parsd = ylparscore-sibparscore

alpha FEAY02R4 FEAY09R4 FEAY12R4 FEAY16R4 FEAY20R4 FEAY24R4 FEAY34R4 FEAY31R4 /*0.73*/
gen avgfriend=(FEAY02R4 + FEAY09R4 + FEAY12R4 + FEAY16R4 + FEAY20R4 + FEAY24R4 + FEAY34R4 + FEAY31R4)/8
gen ylfriendscore=(zFEAY02R4+zFEAY09R4+zFEAY12R4+zFEAY16R4+zFEAY20R4+zFEAY24R4+zFEAY34R4+zFEAY31R4)/8
qui sum ylfriendscore
replace ylfriendscore=r(mean) if ylfriendscore==. & round==4

alpha FEAS01R4 FEAS04R4 FEAS07R4 FEAS13R4 FEAS10R4 FEAS16R4 FEAS19R4 FEAS22R4 /*0.64*/
gen avgsibfriend=(FEAS01R4 + FEAS04R4 + FEAS07R4  + FEAS10R4 + FEAS13R4 + FEAS16R4 + FEAS19R4 + FEAS22R4)/8
gen sibfriendscore=(zFEAS01R4 + zFEAS04R4+ zFEAS07R4 + zFEAS10R4+ zFEAS13R4+ zFEAS16R4 + zFEAS19R4 + zFEAS22R4)/8
qui sum sibfriendscore
replace sibfriendscore=r(mean) if sibfriendscore==. & round==4
gen friendsd = ylfriendscore-sibfriendscore
**to make use of above, will have to long form siblings and difference on contemporaneous characteristics**

********************************************************************************
****Rosenberg Self-Esteem, Generalised Self-Efficacy******
******************************************************************************** 

foreach x in $sef {
	egen z`x'=std(`x')
	}

gen gensef=(zFEAY01R4 + zFEAY05R4 + zFEAY08R4 + zFEAY11R4 + zFEAY15R4 + zFEAY18R4 + zFEAY22R4 + zFEAY26R4 + zFEAY28R4 + zFEAY32R4)/10
	label var gensef "Generalised Self-Efficacy"
	
alpha $sef, item /*0.71*/

foreach x in $ses {
	egen z`x'=std(`x')
	}	

gen avgros=(FEAY04R4 + FEAY06R4 + FEAY14R4 + FEAY17R4 + FEAY23R4 + FEAY33R4 + FEAY27R4 + FEAY30R4)/8 
gen rosses=(zFEAY04R4 + zFEAY06R4 + zFEAY14R4 + zFEAY17R4 + zFEAY23R4 + zFEAY33R4 + zFEAY27R4 + zFEAY30R4)/8 
	label var rosses "Rosenberg Self-Esteem"
	
alpha $ses, item /*0.71*/


********************************************************************************
**Assorted Last Prep**
********************************************************************************

**Possible Instruments**
bysort clustid: egen meanses=mean(psesscore)
bysort clustid: egen meansef=mean(psefscore)

sort cid_num round

order CAG1 CAG2 CAG3 CAG4 CAG5 CPS1 CPS2 CPS3 CPS4 CPS5, after(CNOCHC)

label var hq "Housing Quality"
label var sv "Housing Services"
label var cd "Consumer Durables" 
label var divorce "Are parents divorced?"
label var hhsize "Household size"
label var RPTGRDR4 "Repeated a grade"
label var TRAVALON "Child went to school with parents"
label var female "Female"
label var typesite "Rural"
label var adverse "Shock"
label var stunt "Stunted"
label var chprob "Health Problem"
label var quint "Quintile of Expenditure"
bysort round: sum zhfa



**Cronbach's Alpha (working sample, missings added and removed)** 
alpha CAG1 CAG2 CAG5 , item std
alpha CPS2 CPS3 CPS4 CPS5 , item std
alpha CTRYHD CFTRWR CBRJOB CPLDEC CNOCHC, item std 
alpha CASHSH CASHCL CASHWK CCLTRG CEMBBK CWRUNI, item std

sort cid_num round
gen firstborn=0
replace firstborn=1 if border==1
gen yc=1


/*agency drops significantly, 709 of 955*/


save p3datasd, replace

**Sample as of 3/7/17, 510 sibling pairs**






