**============================================================================**
***Do File, "Sensitive period effects on Cognitive Skills in Peruvian School Aged Children"
**============================================================================**
cd "/users/jBook/Desktop/School/PhD Year 1/Data/Constructed_Files/Draft_File/Peru/"
capture log close
log using peru_sum, text replace

**IF necessary, please install the following user programmes for use**
*ssc install estout, replace
*ssc install ivreg2, replace
*ssc install ivreg2h, replace 
*ssc install xtivreg2h, replace 
*ssc install carryforward, replace 

**============================================================================**
***Preparing datasets for merge***
**============================================================================**
use pe_oc_childlevel, clear
rename CHILDID childid
gen yc=0 
gen round=3
save pe_oc_childleveledit, replace

use pe_oc_householdlevel, clear
rename CHILDID childid
gen yc=0 
gen round=3
save pe_oc_householdleveledit, replace

use pe_yc_householdlevel, clear
rename CHILDID childid
gen yc=1
gen round=3
save pe_yc_householdleveledit, replace

use pe_yc_childlevel, clear 
rename CHILDID childid
gen yc=1
gen round=3
save pe_yc_childleveledit, replace

use pechildlevel12yrold, clear
rename CHILDID childid
gen round=2
gen yc=0
save pechildlevel12yroldedit, replace

use pechildquest12yrold, clear
rename CHILDID childid 
gen round=2
gen yc=0
save pechildquest12yroldedit, replace

use pechildlevel5yrold, clear
rename CHILDID childid
gen round=2
gen yc=1
save pechildlevel5yroldedit, replace

use pesubillnesses5, clear
rename CHILDID childid 
gen round=2
reshape wide ILLDIE ILLTRT TRTILL1 TRTILL2 TRTILL3 PAYILL DISILL, i(childid) j(ILLID)
save pesubillnesses5edit, replace

use pesublongtermhealth5, clear
rename CHILDID childid 
gen round=2
reshape wide HEALTH PEHTHTRT HTHTRT TREATEXP, i(childid) j(HEALTHID)
save pesublongtermhealth5edit, replace 

use pesubchildwork12, clear
rename CHILDID childid 
gen round=2
gen yc=0
reshape wide WORKACT WORKPAY PAYNAME RANKPAY, i(childid) j(WORKID)
save pesubchildwork12edit, replace

use pesubchildwork5, clear
rename CHILDID childid
gen round=2
gen yc=1
save pesubchildwork5edit, replace

use pechildlevel8yrold, clear
rename CHILDID childid
gen round=1
gen yc=0
save pechildlevel8yroldedit, replace

use pechildlevel1yrold, clear
rename CHILDID childid
gen round=1
gen yc=1
save pechildlevel1yroldedit, replace

**============================================================================**
***Merging data for school and work for each round and cohort***
**============================================================================**
use peru_constructed , clear
drop if yc==0

**Round 3**
merge 1:1 childid round using pe_yc_childleveledit, keepus(MNPDACR3 MSTLKR3 LSTLKR3 /// 
STNPRSR3 CTRUSTR3 CGOVRGR3 ///
CSFEOWR3 CSRVCMR3 CFRNSTR3 CLEADR3 CTRYHDR3 CASHSHR3 CPLDECR3 CFTRWRR3 CCLTRGR3 ///
CASHCLR3 CEMBBKR3 CWRUNIR3 CBRJOBR3 ///
CASHWKR3 CNOCHCR3 FTRWRKR3   ///
HLPCHLR3 HRDTLKR3 TMONINR3 RISKAVR3 RISKAR32 PPLTRTR3 INCGMER3 egra math ppvt sppvt egra_co rsppvt_co  ///
math_co rppvt_co rmath_co) update
drop _merge

***Adding sibling and JUNTOS data***
merge 1:1 childid round using pe_yc_householdleveledit, keepus(SIBLING S1IDR3 /// 
S1MTHR3 S1YEARR3 S1SEXR3 S1WBRTR3 S1WGDCR3 S1WT1R3 S1WT2R3 S1WGHTR3 S1WGNOR3 ///
S1HT1R3 S1HT2R3 S1HGHTR3 S1HGNOR3 S2IDR3 S2MTHR3 S2YEARR3 S2SEXR3 S2WBRTR3 ///
S2WGDCR3 S2WT1R3 S2WT2R3 S2WGNOR3 S2WGHTR3 S2HT1R3 S2HT2R3 S2HGHTR3 S2HGNOR3 ///
OBTNLNR3 MTWGHTR3 CAG1R3 CPS1R3 CAG2R3 CPS2R3 CPS3R3 CAG3R3 ///
CSD1R3 CPS4R3 CAG4R3 CSD2R3 CPS5R3 CSD3R3 CAG5R3 CAMBTNR3 GRDLKER3 JUNTOSR3 AMTJUNR3 RQJNR305 ///
RQJNR306 RQJNR307 RQJNR309 RQJNR312 NMEHLTR3 TMINJR3 WRRYFDR3 HWLLBGR3 ) update
drop _merge

**Round 2**
merge 1:1 childid round using pechildlevel5yroldedit, keepus(ACCESS02 BORNBEF CURPRESC SEEDAD SEEMUM SEECARE PEAGREE1 PEAGREE2 ///
PEAGREE3 PELOAN CSV1 CSV2 CSV3 CSV4 CSV5 CTR1 CTR2 CTR4 CTR5 CTR6 CAG1 CPS1 CAG2 CPS2  ///
CPS3 CAG3 CSD1 CPS4 CAG4 CSD2 CPS5 CSD3 CAG5 GETINF01 GETINF02 GETINF03 GETINF04 GETINF05 GETINF06 GETINF07 ///
LADDER FARLAD FAMSON CFUTURJB VACCARD BCG MEASLES DPT OPV HIB FOODSHRT PE4C27 PE4C34 PE4C37 ///
GRADLIKE EXPGRADE MTWEIGHT MTHEIGHT TRAVALON PERPRESC CRECH PEINSURE score_cog ) update
drop _merge

merge 1:1 childid round using pesubillnesses5edit, keepus(ILLDIE1 ILLTRT1 ILLDIE2 ILLTRT2)
drop _merge
 
merge 1:1 childid round using pesublongtermhealth5edit, keepus(HEALTH1 HEALTH2 HEALTH3 HTHTRT1 HTHTRT2 HTHTRT3 TREATEXP1 TREATEXP2 TREATEXP3)
drop _merge


**Round 1**
merge 1:1 childid round using pechildlevel1yroldedit, keepus(CRECH CARE CHLDCARE ///
CAREYUNG LLORA LCARGA LTRANQ LMECE DAGUA DPALM LSAC LPELL LAMEN ///
LMED LDMED LDPECH LPBOC LESAB LNADA LOTRO MOTHREL PREMATUR NWEEKS WHICHSEX BHOSPIT ///
LONGTERM JOIN TRUST ALONG PART ADVANTAG SEEDAD SEEMOM WANTCLD DOCBRTH pesomad tallmad) update
drop _merge 

drop if zhfa==.
drop if ppvtraw==. & round!=1


replace PREMATUR = PREMATUR[_n-1] if missing(PREMATUR) & yc==1
replace BHOSPIT = BHOSPIT[_n-1] if missing(BHOSPIT) & yc==1
replace LONGTERM = LONGTERM[_n-1] if missing(LONGTERM) & yc==1
replace SEEMOM=SEEMUM[_n] if missing(SEEMOM)
drop SEEMUM
recode LCARGA-LOTRO (8=0) (77=.) (79=.) (88=.) (99=.)
recode CRECH (2=0) (77=.) (79=.) (88=.) (99=.)
  
local variable_list LCARGA LTRANQ LMECE DAGUA DPALM LSAC LPELL LAMEN LMED LDMED LDPECH LPBOC LESAB LNADA LOTRO
foreach var of local variable_list{
la drop `var'
la def `var' 0 "Not Mentioned" 1 "YES" 99 "NK" 
}

***Creating general shock variables and table***

gen crimeshock = (shcrime2==1| shcrime5==1 | shcrime7==1)
**gen regulshock = (shregul1==1 | shregul2==1 | shregul3==1 | shregul4==1 | shregul5==1 | shregul6==1)
gen econshock = (shecon1==1 | shecon2==1 | shecon4==1 | shecon5==1 | ///
shecon14==1)
gen envshock = (shenv1==1 | shenv2==1 | shenv4==1 | shenv5==1 | shenv8==1 )
gen hhshock = (shhouse1==1 | shhouse2==1 | shhouse3==1)
gen famshock = (shfam5==1 | shfam6==1 | shfam7==1 | shfam8==1 |shfam13==1 | shfam18==1)
gen adverse= (crimeshock==1 | econshock==1 | envshock==1 | ///
hhshock==1 | famshock==1)

gen econshock2=econshock if round==2
gen crimeshock2=crimeshock if round==2
**gen regulshock2=regulshock if round==2
gen envshock2=envshock if round==2
gen hhshock2=hhshock if round==2
gen famshock2=famshock if round==2

gen doeswork= (hwork>0 | htask>0)
gen doeschores= (hchore>0)
gen doesplay= (hplay>0)


 
**============================================================================**
***Variable creation and clean up***
**============================================================================**

recode sex (1=0) (2=1), gen(female)
recode typesite (2=0), gen(urban)
recode PREMATUR (2=0), gen(bornearly)
recode BHOSPIT (2=0), gen(hospstay)
recode CSD3 (2=1), gen (meanteach)
recode NWEEKS (88=0) (99=0)
recode schtype (1=0) (2=1) (3=1) (99=.), gen(privsch)
recode dadlive chhrel (1=0) (2=0) (3=1) (99=.)
recode CTRYHDR3 CBRJOBR3 CPLDECR3 CFTRWRR3 CPLDECR3 FOODSHRT BCG MEASLES DPT OPV HIB (77=.) (79=.) (88=.) (99=.)
recode PE4C34 PE4C37 (2=1) (3=1) (4=0) (77=.) (79=.) (88=.) (99=.)
replace JUNTOSR3=. if JUNTOSR3==79
replace OBTNLNR3=. if OBTNLNR3==79
recode WANTCLD DOCBRTH WHICHSEX (88=.) (99=.)
recode WANTCLD DOCBRTH (2=0)
recode MTWEIGHT MTHEIGHT (-88=.) (-99=.)
drop if MTHEIGHT==58.35
recode SEEDAD dadlive CURPRESC  TRAVALON (88=.) (99=.)
recode SEEDAD dadlive CURPRESC  TRAVALON (77=.) (79=.)
gen momage1= momage if round==1
gen R1age=agemon if round==1
egen vactotal= rowtotal(BCG MEASLES DPT OPV HIB)


***R1 Wealth Index***
bysort childid: gen wip1=wi[_n-2]
by childid: replace wip1=wi[_n-1] if wip1==.
by childid: replace wip1=wi[_n] if wip1==.

bysort childid: gen zhfap1=zhfa[_n-2]
by childid: replace zhfap1=zhfa[_n-1] if zhfap1==.
by childid: replace zhfap1=zhfa[_n] if zhfap1==.

gen term=(bornearly!=1)
gen coast=(region==31)
gen mountain=(region==32)
gen jungle=(region==33) 
gen noesp = (chlang!=31)
egen siblings= rowtotal(male05-male1317 female05-female1317)
tab siblings, gen(sibsnum)
gen work=(hwork>0) if hwork !=.
gen momage1sq= momage1^2
gen logexp= log(totalexp_r)
gen highwi3=1 if wi>.5758499 & round==3
gen highwi2=1 if wi>.5048839 & round==2
gen highwi1=1 if wi>.4632247 & round==1
recode high* (.=0)
tab clustid, gen(cluster)
gen expR2=0
replace expR2=foodexp_pc if round==2
recode expR2(0=.)
by childid: replace expR2= expR2[_n-1] if expR2==.

xtile pct=ppvtraw, n(4)
tab pct, gen(pq)
drop pct



***Quantifying animal ownership***
egen numanimals= rowtotal(animilk-aniothe)

******ANOTHER WAY TO DO IT********
egen cid_num= group(childid)
tsset cid_num round
gen lagwi=l.wi
gen lag2wi=l2.wi
gen diffsibs= siblings-l.siblings
gen diffsibs2= siblings-l2.siblings
gen widiff= wi-lagwi
gen widiff2=lagwi-lag2wi

xtile pct=wi, n(4)
tab pct, gen(wiq)
drop pct

sort childid round
by childid: egen bwp1=total(bwght)
replace bwp1=. if bwp1==0
gen ppvtrawr2=0
bysort childid: replace ppvtrawr2=ppvtraw[_n-1]
replace ppvtrawr2=. if ppvtrawr2==-88
sort childid round

xtile pct=ppvtrawr2, n(4)
tab pct, gen(pqr2)
drop pct

sort childid round
by childid: gen lagzhfa=zhfa[_n-1]
by childid: gen lagzhfa2=zhfa[_n-2]

gen lagstunt=.
by childid: replace lagstunt= stunt[_n-1]
by childid: gen lagstunt2= stunt[_n-2]
**drop if zhfa>4 & round==1 | zhfa<-7 & round==1
recode bornearly (88=0) (99=0)
gen hazpre = zhfap1*bornearly
gen hazterm= zhfap1*term
gen hazdelta= zhfa-lagzhfa if round==3
gen hazdeltar2= zhfa-lagzhfa if round==2
gen hazavg=(lagzhfa+zhfa)/2 if round==2
gen zhfa1diff= zhfa-lagzhfa if round==3
gen zhfa2diff= lagzhfa-lagzhfa2 if round==3


**birthorder**
gen birthorder=0 if siblings<4 & round==1
replace birthorder=1 if siblings>4 & round==1
gen birthorder1=1 if siblings==0 & round==1
gen birthorder2=1 if siblings==1 & round==1
gen birthorder3=1 if siblings==2 & round==1
gen birthorder4=1 if siblings==3 & round==1
gen birthorder5=1 if siblings==4 & round==1
gen birthorder6=1 if siblings==5 & round==1
gen birthorder7=1 if siblings==6 & round==1
gen birthorder8=1 if siblings==7 & round==1
gen birthorder9=1 if siblings==8 & round==1
recode birthorder1-birthorder9 (.=0)
recode BORNBEF (77=.)
gen border= BORNBEF+1
gen borderbig=0 if border<4 & round==2
replace borderbig=1 if border>4 & round==2

tab border, gen(bordernum)

**birthmonth**



***Occupational aspirations, based on UK Std. Occupational Classification 2010***
***Baseline includes non-skilled workers, traditional jobs, military and "other"***
gen mangasp= (FTRWRKR3==45) if round==3
gen sciengasp= (FTRWRKR3==5|FTRWRKR3==14|FTRWRKR3==31) if round==3
gen medasp= (FTRWRKR3==9|FTRWRKR3==11|FTRWRKR3==25|FTRWRKR3==41) if round==3
gen teachasp= (FTRWRKR3==21|FTRWRKR3==37) if round==3
gen bmlasp= (FTRWRKR3==1|FTRWRKR3==20|FTRWRKR3==38) if round==3
gen civserv= (FTRWRKR3==4|FTRWRKR3==29|FTRWRKR3==30) if round==3
gen pubserv= (FTRWRKR3==10|FTRWRKR3==12|FTRWRKR3==16|FTRWRKR3==28) if round==3
gen cultasp= (FTRWRKR3==2|FTRWRKR3==3|FTRWRKR3==32|FTRWRKR3==34) if round==3
gen skillasp= (FTRWRKR3==7|FTRWRKR3==8|FTRWRKR3==13|FTRWRKR3==15|FTRWRKR3==23|FTRWRKR3==24|FTRWRKR3==26|FTRWRKR3==27|FTRWRKR3==37) if round==3
gen studasp= (FTRWRKR3==40) if round==3

gen adverseR1= (adverse==1) if round==1
gen adverseR2= (adverse==1) if round==2
gen adverseR3= (adverse==1) if round==3
gen chweightp2=chweight if round==2



**============================================================================**
***Factors***
**============================================================================**
**Cognitive factors**

egen zegra=std(egra)
egen zmath= std(math)
egen zscore_cog= std(score_cog)
replace zscore_cog= zscore_cog[_n-1] if missing(zscore_cog)

egen z3ppvt= std(ppvtraw) if round==3
egen z2ppvt= std(ppvtraw) if round==2
gen ppvtdiff= ppvtraw-ppvtrawr2
egen zppvt= std(ppvtraw)
by childid: egen ppvtavg= mean(ppvtraw)

factor ppvtraw score_cog if round==2, fa(1) 
rotate
predict cogR2
egen zcogR2= std(cogR2)

factor ppvtraw egra math if round==3, fa(1) 
rotate 
predict cogR3
egen zcogR3= std(cogR3)

factor lagzhfa lagzhfa2
rotate
predict health

**============================================================================**
**Dropping unneeded variables**
**============================================================================**
drop dint hq cd sv male05-female61 careage careid sunderweight sthinness  ///
inr1 inr2 inr3 panel123 headedu headage headsex headid headsch aniany-aniothe ///
prvision prhear prhead prresp  ///
ravens chgrade  MNPDACR3-LSTLKR3 ///
SIBLING OBTNLNR3 S1IDR3-S2HGNOR3 
***drop CAG* CPS* CSD*
**drop egra_co-rmath_co
drop elecq-cookingq
drop *land
drop PEA*
drop if noesp==1


save peycall, replace

***Interactions**
gen haz2mom= lagzhfa2*momedu
gen haz1mom= lagzhfa*momedu
gen hazmom= zhfa*momedu
gen haz2dad= lagzhfa2*dadedu
gen haz1dad= lagzhfa*dadedu
gen hazdad= zhfa*dadedu
gen haz2fem= lagzhfa2*female
gen haz1fem= lagzhfa*female
gen haz2border= lagzhfa2*borderbig
gen haz1border= lagzhfa*borderbig
gen hazppvt= lagzhfa*ppvtrawr2
gen hazhigh= lagzhfa*pqr24
gen hazmedhigh= lagzhfa*pqr23
gen hazlowmed= lagzhfa*pqr22
gen hazwi4= lagzhfa*wiq4
gen hazwi3= lagzhfa*wiq3
gen hazwi2= lagzhfa*wiq2
gen hazwi1= lagzhfa*wiq1
gen haz2wi= lagzhfa2*lag2wi
gen haz1wi= lagzhfa*lagwi
gen hazage= zhfa*agemon
gen haz1age= zhfa*agemon if round==2
gen haz2age= zhfa*agemon if round==1
gen femfd=round*female
gen momfd= round*momedu
gen bofd= border*round
gen bwfd= round*bwp1



**============================================================================**
********Expanding 1 period variables********
**This is done because we look at the effect of round 1 on round 3,**
**so round variables need to be carried to round three for computational ease at the moment**
**============================================================================*


foreach x of varlist * {
bysort childid: carryforward `x', replace
}

save peycall, replace
save peycallLTD, replace

xtile pct=hazdeltar2, n(5)
tab pct, gen(deltq)
drop pct
gen lagdel80=lagzhfa*deltq5
gen lagdelt60=lagzhfa*deltq4
gen lagdelt40=lagzhfa*deltq3
gen lagdelt20=lagzhfa*deltq2
gen lagdel=lagzhfa*deltq1

drop if lagzhfa>5 & round!=1 | lagzhfa<-5 & round!=1
drop if lagzhfa2<-5

**============================================================================**
**Regressions**
**============================================================================**

***Last variable creation***
gen homer1= (inject+WANTCLD+DOCBRTH+WHICHSEX+SEEDAD)/5 if round==1
factor inject WANTCLD DOCBRTH WHICHSEX SEEDAD if round==1 , fa(1)
rotate
predict homeinput1 
gen homer2= (SEEDAD+dadlive+CURPRESC+privsch+TRAVALON)/5 if round==2
factor SEEDAD dadlive CURPRESC  TRAVALON if round==2
rotate
predict homeinput2
gen homer3= (SEEDAD+dadlive+privsch+doeswork+doeschores+doesplay)/6 if round==3
factor SEEDAD dadlive privsch doeswork doeschores doesplay if round==3
rotate
predict homeinput3

gen laghi=.
replace laghi=homeinput2
replace laghi=. if round ==2
replace laghi=homeinput1 if round==2

egen zwi=std(wi) if round==3
egen zlagwi=std(lagwi) if round==3
egen zlag2wi=std(lag2wi) if round==3

drop if hazdeltar2>5 & round==3
drop if hazdeltar2<-5 & round==3
recode tallmad (-9999=.)

gen n=_n

label var lagzhfa2 "Height for Age R1"
label var lagzhfa "Height for Age R2"
label var zhfa "Height for Age R3"
label var ppvtraw "PPVT R3"
label var ppvtrawr2 "PPVT R2"
label var z3ppvt "Standardised PPVT R3"
label var z2ppvt "Standardised PPVT R2"
label var wi "Wealth Index R3"
label var lagwi "Wealth Index R2"
label var lag2wi "Wealth Index R1" 
label var momage1 "Mother's age at birth"
label var momage1sq "Age squared"
label var agemon "Age of child (mths)"
label var momedu "Years of school mom"
label var dadedu "Years of school dad"
label var adverseR1 "R1 Shock"
label var borderbig "4th or more child born"
label var bwp1 "Birthweight"
label var pesomad "Mother's Weight R1"
label var tallmad "Mother's Height R1"
label var inject "Number of antenatal injections"
label var bornearly "Born Premature"
label var urban "Urban"
label var female "Female"
label var math "Math test"
label var hazdeltar2 "Difference in R2 and R1 health"
la var SEEDAD "Freq. of seeing father"
la var WANTCLD "Was child planned"
la var DOCBRTH "Doctor Present"
la var WHICHSEX "Which gender desired?"
la var dadlive "Is father alive?"
la var CURPRESC "Currently in Pre School"
la var TRAVALON "How does child get to school"
la var privsch "Private School"

save peycall, replace 

ivreg2 ppvtraw lagzhfa lagzhfa2 lag2wi lagwi ppvtrawr2 agemon tallmad hhsize female coast mountain dadedu momedu momage1 momage1sq border adverseR1 if round==3, r
est store ppvtcomp

test lagzhfa=lagzhfa2
local sign_haz=sign(_b[lagzhfa]-_b[lagzhfa2])
display "H_0: lagzhfa >= lagzhfa2 p-value = " normal(`sign_haz'*sqrt(r(chi2)))

ivreg2 z3ppvt lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 if round==3, r
est store zppvtcomp
gen ppvtsamp=e(sample)

test lagzhfa=lagzhfa2
local sign_haz=sign(_b[lagzhfa]-_b[lagzhfa2])
display "H_0: lagzhfa >= lagzhfa2 p-value = " normal(`sign_haz'*sqrt(r(chi2)))


ivreg2 math lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 if round==3, r
est store mathcomp

test lagzhfa=lagzhfa2
local sign_haz=sign(_b[lagzhfa]-_b[lagzhfa2])
display "H_0: lagzhfa >= lagzhfa2 p-value = " normal(`sign_haz'*sqrt(r(chi2)))

ivreg2 egra lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 if round==3, r
est store egracomp

test lagzhfa=lagzhfa2
local sign_haz=sign(_b[lagzhfa]-_b[lagzhfa2])
display "H_0: lagzhfa >= lagzhfa2 p-value = " normal(`sign_haz'*sqrt(r(chi2)))

ivreg2 ppvtraw (lagzhfa lagzhfa2= bwp1 inject) lagwi lag2wi tallmad ppvtrawr2 female agemon hhsize borderbig momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
ivreg2 z3ppvt (lagzhfa lagzhfa2= bwp1 inject) lagwi lag2wi tallmad z2ppvt female agemon hhsize borderbig momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
ivreg2 math (lagzhfa lagzhfa2= bwp1 inject) lagwi lag2wi tallmad ppvtrawr2 female agemon hhsize borderbig momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
ivreg2 egra (lagzhfa lagzhfa2= bwp1 inject) lagwi lag2wi tallmad ppvtrawr2 female agemon hhsize borderbig momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 

**First Differences**
xtivreg2 ppvtraw lagzhfa lagwi agemon, r fd
est store fdreg
xtivreg2 ppvtraw lagstunt lagwi agemon, r fd
est store fdcheck
xtivreg2 zppvt lagzhfa lagwi agemon, r fd
est store fdzreg
xtivreg2 zppvt lagstunt lagwi agemon, r fd
est store fdzcheck
xtivreg2 ppvtraw lagzhfa lagdel80 lagdelt60 lagdelt40 lagdelt20 lagwi agemon, r fd
est store fdquint
xtivreg2 zppvt lagzhfa lagdel80 lagdelt60 lagdelt40 lagdelt20 lagwi agemon, r fd
est store fdzquint
xtivreg2 zppvt lagzhfa femfd momfd lagwi agemon , r  fd
est store fdinv
gen fdsamp=e(sample)
xtivreg2 zppvt (lagzhfa=bwfd) lagwi agemon femfd momfd , r fd endog(lagzhfa)
est store fdiv

**Lewbel(2012) IV inject not great, fails orthoganality test. BW fine, but weakly id'd. NUMANTE fails too**
**How to fix?**
ivreg2h z3ppvt (lagzhfa lagzhfa2= bwp1) lagwi lag2wi tallmad z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain if round==3, robust gen
ranktest (lagzhfa lagzhfa2) (wi lag2wi tallmad ppvtrawr2 female agemon hhsize border momedu dadedu lagwi adverseR1)

**Standard IV with stunting instead of health to provide more separability**
ivreg2 ppvtraw lagstunt lagstunt2 lag2wi lagwi ppvtrawr2 agemon female coast mountain dadedu momedu momage1 momage1sq borderbig adverseR1 if round==3, r 
est store stuntcheck
	
ivreg2 math (lagzhfa lagzhfa2= bwp1 inject) lagwi lag2wi tallmad ppvtrawr2 female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
ivreg2 egra (lagzhfa lagzhfa2= bwp1 inject) lagwi lag2wi tallmad ppvtrawr2 female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
ivreg2 ppvtraw (lagzhfa lagzhfa2= bwp1 lagzhfa_tallmad_g lagzhfa2_tallmad_g) lagwi lag2wi ppvtrawr2 tallmad female agemon hhsize border momedu dadedu momage1 momage1sq
ivreg2 z3ppvt (lagzhfa lagzhfa2= bwp1 lagzhfa_tallmad_g) lagwi lag2wi z2ppvt tallmad female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain
est store Preferred

**Individual specs**
ivreg2 z3ppvt lagzhfa lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 if round==3, r
est store lag
ivreg2 z3ppvt lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 if round==3, r
est store lag2
ivreg2 z3ppvt (lagzhfa= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lagmiv
ivreg2 z3ppvt (lagzhfa2= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lag2miv
ivreg2 z3ppvt (lagzhfa= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lagiv
ivreg2 z3ppvt (lagzhfa2= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lag2iv
ivreg2 z3ppvt (lagzhfa lagzhfa2= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store miv
gen ivsamp=e(sample)

**Clusters**
ivreg2 z3ppvt lagzhfa lagzhfa2 tallmad lagwi lag2wi z2ppvt female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 cluster2-cluster20 if round==3, r 
est store fullc
ivreg2 z3ppvt lagzhfa lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 cluster2-cluster20 if round==3, r
est store lagc
ivreg2 z3ppvt lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 cluster2-cluster20 if round==3, r
est store lag2c
ivreg2 z3ppvt (lagzhfa= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain cluster2-cluster20 , r 
est store lagivmc
ivreg2 z3ppvt (lagzhfa2= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain cluster2-cluster20, r 
est store lag2ivmc
ivreg2 z3ppvt (lagzhfa= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain cluster2-cluster20 , r 
est store lagivc
ivreg2 z3ppvt (lagzhfa2= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain cluster2-cluster20, r 
est store lag2ivc
ivreg2 z3ppvt (lagzhfa lagzhfa2= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain cluster2-cluster20 , r 
est store ivmc
ivreg2 z3ppvt lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 cluster2-cluster20 if round==3, r
est store zppvtcompc

**Home input addition**
ivreg2 ppvtraw lagzhfa lagzhfa2 lagwi lag2wi ppvtrawr2 tallmad female agemon hhsize borderbig momedu dadedu momage1 momage1sq homeinput1 homeinput2, robust
ivreg2 z3ppvt lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize borderbig momedu dadedu momage1 momage1sq homeinput1 homeinput2, robust


**Robustness**
ivreg2 z3ppvt lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize border coast mountain momedu dadedu momage1 momage1sq adverseR1 if ivsamp==1, r
est store smallsamp
ivreg2 z3ppvt lagzhfa lagzhfa2 lagwi lag2wi z2ppvt tallmad female agemon hhsize bordernum2-bordernum10 coast mountain momedu dadedu momage1 momage1sq adverseR1 if round==3, r
est store border
xtivreg2 zppvt (lagzhfa=bwfd) lagwi agemon femfd momfd if ivsamp==1 , r fd endog(lagzhfa)
est store fdivcheck
xtivreg2 zppvt lagzhfa lagwi agemon femfd momfd if ivsamp==1 , r fd 
est store fdcheck
xtivreg2 zppvt lagzhfa lagdel80 lagdelt60 lagdelt40 lagdelt20 lagwi agemon if ivsamp==1, r fd
est store fdquintcheck
ivreg2 math (lagzhfa= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lagivm
ivreg2 egra (lagzhfa= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lagive
ivreg2 math (lagzhfa2= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lag2ivm
ivreg2 egra (lagzhfa2= bwp1) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store lag2ive
ivreg2 math (lagzhfa lagzhfa2= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store ivmath
ivreg2 egra (lagzhfa lagzhfa2= bwp1 tallmad) lagwi lag2wi z2ppvt female agemon hhsize border momedu dadedu momage1 momage1sq adverseR1 coast mountain , r 
est store ivegra



est table zppvtcomp ppvtcomp mathcomp egracomp,					///
	b(%6.3f) star(0.10 0.05 0.01) 		///
	stats(N r2 F j jdf jp cstat cstatdf cstatp estat estatdf estatp)

est table fdzreg fdzquint fdzcheck fdinv, 		///
	star(0.1 0.05 0.01) 		///
	stats(N F r2)

est table ppvtcomp stuntcheck GenInst GenExtInst fdreg fdcheck, 		///
	b(%6.3f) star(0.1 0.05 0.01) 		///
	stats(N r2 F idstat widstat j jdf jp arubin arf ccev)	

est table lag lag2 lagiv lag2iv miv zppvtcomp,					///
	b(%6.3f) star(0.10 0.05 0.01) 		///
	stats(N r2 F j jdf jp widstat)
	
est table lagc lag2c lagivc lag2ivc ivmc zppvtcompc,					///
	b(%6.3f) star(0.10 0.05 0.01) 		///
	stats(N r2 F j jdf jp widstat)
	
est table lag lag2 lagmiv lag2miv miv zppvtcomp,					///
	b(%6.3f) star(0.10 0.05 0.01) 		///
	stats(N r2 F j jdf jp widstat)
	
est table lagc lag2c lagivmc lag2ivmc ivmc zppvtcompc,					///
	b(%6.3f) star(0.10 0.05 0.01) 		///
	stats(N r2 F j jdf jp widstat)
	
save peycall, replace

**============================================================================**
**============================================================================**
**STATA user created programme citations                                      **
**============================================================================**
**--Baum, C.F., Schaffer, M.E., Stillman, S. 2010.  ivreg2: Stata module for extended instrumental variables/2SLS, GMM and
**   AC/HAC, LIML and k-class regression.  http://ideas.repec.org/c/boc/bocode/s425401.html
**--Baum, CF, Schaffer, ME, 2012.  ivreg2h: Stata module to perform instrumental variables estimation using
**   heteroskedasticity-based instruments.  http://ideas.repec.org/c/boc/bocode/s457555.html
**--estout citation: Ben Jann, ETH Zurich, jannb@ethz.ch
**============================================================================**
**============================================================================**
**--John Creamer, Heriot-Watt University, 2015.                               **
**============================================================================**


log close


