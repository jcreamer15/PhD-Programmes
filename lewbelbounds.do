cap cd "C:\LocalStore\ecomes\Dropbox\Lewbel Monte Carlo"
capture log close
log using bounds, replace
set more off

/*
**Using Lewbel's Engle data**
bcuse engeldat, clear 
global y foodshare
global x lrtotexp
global z lrinc
global controls age-twocars
center age-twocars, prefix(z_)
*/


**Using John's Data**
use peycall, clear
global y z3ppvt
global x lagzhfa2
global z tallmad
*global z agemon
*global controls tallmad
*global controls z2ppvt female hhsize border momedu dadedu momage1 momage1sq privsch urban
global controls

qui ivreg2h $y ($x=) $z $controls
drop if e(sample)==0


**Partialling**
** MISTAKE WAS HERE - MUST PARTIAL OUT Z AS WELL **
reg $y $z $controls
predict double ytilde, resid

** MISTAKE WAS HERE - MUST PARTIAL OUT Z AS WELL **
reg $x $z $controls
predict double xtilde, resid

* Partial out controls only
reg $z $controls
predict double ztilde, resid

**Reduced form**
*RF1*
reg ytilde ztilde, nocons
predict double what, resid

*RF2* 
reg xtilde ztilde, nocons
predict double uhat, resid

**Generating the instrument**
/*
center $z, double
gen double zu=c_$z*uhat
reg zu $controls
predict double zddot, resid
*/

gen double zddot = ztilde*uhat
ivreg2 ytilde (uhat=zddot), nocons r 

gen double wiui= what*uhat
gen double uisq= uhat^2

scalar tau0=0.00001
scalar tau1=0.1
scalar tau5=0.5
scalar tau9=0.9

**Vars and covars**

corr wiui ztilde, c
scalar cov_wu_z= r(cov_12)
scalar var_wu = r(Var_1)

corr uisq ztilde, c
scalar cov_uu_z=r(cov_12)
scalar var_uu=r(Var_1)

corr wiui uisq, c 
scalar cov_wu_uu=r(cov_12)

**************************************************
scalar D1= cov_wu_z^2 / cov_uu_z^2

scalar D2 = var_wu / var_uu

scalar D3 = cov_wu_uu / var_uu

scalar D4 = cov_wu_z / cov_uu_z

*A*
scalar a0 = 1-tau0^2 
scalar a1 = 1-tau1^2 
scalar a5 = 1-tau5^2 
scalar a9 = 1-tau9^2


*B*
scalar b0 = 2 * ((D3 * tau0^2) - D4)
scalar b1 = 2 * ((D3 * tau1^2) - D4)
scalar b5 = 2 * ((D3 * tau5^2) - D4)
scalar b9 = 2 * ((D3 * tau9^2) - D4)


*C*
scalar c0 = D1 - D2 * tau0^2 
scalar c1 = D1 - D2 * tau1^2
scalar c5 = D1 - D2 * tau5^2
scalar c9 = D1 - D2 * tau9^2


*********************************************************************************
**tau==0**
scalar pos0 = (-b0 + sqrt(b0^2 - 4 * a0 * c0)) / (2 * a0)
di pos0

scalar neg0 = (-b0 - sqrt(b0^2 - 4 * a0 * c0)) / (2 * a0)
di neg0

**tau==0.1**
scalar pos1 = (-b1 + sqrt(b1^2 - 4 * a1 * c1)) / (2 * a1)
di pos1

scalar neg1 = (-b1 - sqrt(b1^2 - 4 * a1 * c1)) / (2 * a1)
di neg1

**tau==0.5**
scalar pos5 = (-b5 + sqrt(b5^2 - 4 * a5 * c5)) / (2 * a5)
di pos5

scalar neg5 = (-b5 - sqrt(b5^2 - 4 * a5 * c5)) / (2 * a5)
di neg5

**tau==0.9**
scalar pos9 = (-b9 + sqrt(b9^2 - 4 * a9 * c9)) / (2 * a9)
di pos9

scalar neg9 = (-b9 - sqrt(b9^2 - 4 * a9 * c9)) / (2 * a9)
di neg9
*********************************************************************************

di "point est:  [ "  %6.3f neg0 ", "  %6.3f pos0 "]"
di "tau=0.1:    [ "  %6.3f neg1 ", "  %6.3f pos1 "]"
di "tau=0.5:    [ "  %6.3f neg5 ", "  %6.3f pos5 "]"
di "tau=0.9:    [ "  %6.3f neg9 ", "  %6.3f pos9 "]"

*including controls as a check on the partialling*
*ivreg2 ytilde (uhat= zddot) $controls, nocons r 

* IV estimator
ivreg2 ytilde (uhat= zddot), nocons r 
ivreg2 $y ($x=zddot bwp1) $controls, r 
* IV estimator by hand
di "IV beta = cov(yhat, z*uhat) / cov(xhat, z*uhat) = " cov_wu_z/cov_uu_z


cap log close



