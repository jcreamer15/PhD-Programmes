**Lewbel Monte Carlo**
cd "/Users/jBook/Desktop/School/PhD Year 1/Lewbel"
clear
capture log close
log using lew, replace
capture program drop lewsim
program define lewsim, rclass
	drop _all
	set obs 100




*******************Lewbel Setup********************************** 
// beta11 = a constant in main eq
// beta12 = b coeff on exog regressors in main eq
// beta21 = c constant in 2nd eqn
// beta22 = d coeff on exog regressors in 2nd eq
// gamma1 = ?? coeff on endog regressor in main eq
// gamma2 = 0 in triangular model, coeff on endog regressor in 2nd eqn
// epsilon1 = e1
// epsilon2 = e2
// S1 = v1 N(0,1)
// S2 = v2 N(0,1)
// Y2 = x2 endog regressor
// Y1 = y dep var
// X = x1 exog regressors
// U = u shared component of error N(0,1)


gen u=rnormal()
gen x1=rnormal() 
gen z=rnormal()
gen v1=rnormal()
gen eta=rnormal()
gen v2=rnormal()
global a=1
global c=1
gen    gamma2=0
gen e1=u+(exp(x1))*v1
gen e2=u+(exp(-x1))*v2

//Flavours
//gamma1 is homogeneous or heteorgenous
//beta12 hom or het 
//beta22 hom or het
//epsilon1 homoskedastic or heteroskedastic
//eps2 hom or het 
// how to code:
// stop using globals (scalars) for b, d and gammas
// can keep using globals for constant terms a and c
// indiv global determines each of the 5 choices above
// 5 if then else blocks
// for gamma and beta, hom (example) => gen b = 1
//                     het (example) => gen b = 1 + rnormal()
// for eps if hom use hom code else use het code
// and then assemble x2 and y after all 5 choices
// then...
// you can write a loop to check all combinations
// forvalues i=1/2 {
//   forvalues j=1/2 {
//     forvalues k=1/2 {
//       forvalues m=1/2 {
//         forvalues n=1/2 {
//           global gamma1 = `i'
//           global beta12 = `j'
//           global beta21 = `k'
//           global eps1 = `m'
//           global eps2 = `n'
//           di "Combinatation: `i' `j' `k' `m' `n'"
//           <qui simulate>
//           sum b_gamma1 widstat
//         } etc


if $gamma==1 {
*Constant Treatment effect*
	gen gamma1 = 1
	gen b = 1
	gen d = 1	
	
	gen x2 = $c + d*x1 + e2
	gen y  = $a + b*x1 + gamma1*x2 + e1
}

else if $gamma==2 {
***Heterogeneous treatment effect***
**x2i=bix1i+e2i
**bi=bbar+etai
**x2i=bbarx1i+(bi-bbar)x1i+e2i
	gen b = 1
	gen d = 1
	gen x2 = $c + d*x1 + e2
	
// replace gamma1 (scalar) with gamma1_i (vector, heterogeneous)
	
	gen gamma1_i = rnormal() + 1			// needs to be mean 1
	gen y  = $a + b*x1 + gamma1_i*x2 + e1

}



if $beta12==1   {
	**Same as Constant Treatment Effect**
	}

else if $beta12==2 {
		
	**Het main eq coeff on exog regs**
	gen b_i= rnormal() + 1
	replace y = $a + b_i*x1 + gamma1*x2 + e1

	}



if $beta22==1 {	
	
	
	}

else if $beta22==2 {
	**Het second equation coeff exog regs**
	gen d_i = rnormal() + 1 
	replace x2 = $c + d_i*x1 + e2
	replace y = $a + b*x1 + gamma1*x2 + e1
	}
	

	
if $eps1==1 {
	**Hom main eq error**
	replace e1 = u + v1
	replace x2 = $c + d*x1 + e2
	replace y  = $a + b*x1 + gamma1*x2 + e1
}

else if $eps1==2 {
	
	replace x2 = $c + d*x1 + e2
	replace y  = $a + b*x1 + gamma1*x2 + e1
	
}

if $eps2==1 {
	**Hom second eq. error**
	replace e2 = u + v2
	replace x2 = $c + d*x1 + e2
	replace y  = $a + b*x1 + gamma1*x2 + e1
	
	}

else if $eps2==2 {
	
	replace x2 = $c + d*x1 + e2
	replace y  = $a + b*x1 + gamma1*x2 + e1
	
	}

else if $eps2==3 {

	replace e2 = u + v2*eta
	
	}
	
else if $eps2==4 {
	
	replace e2 = u + eta*x1
	
	}

**Manual Lewbel to easily return coefficients (have to report constant) (Why do we not use included instruments in first stage regression) **
ivreg2 x2 x1
predict double ehat, resid 
sum x1, mean
gen double zlew=(x1-r(mean))*ehat
return scalar b_beta22=_coef[x1]
return scalar b_beta21=_coef[_cons]

**OLS**
reg y x1 x2
return scalar b_ols1=_coef[x1]
return scalar b_ols2=_coef[x2]

**Generated Instrument only**
ivreg2 y x1 (x2=zlew), r 
return scalar b_beta12=_coef[x1]
return scalar b_gamma1=_coef[x2]
return scalar b_beta11=_coef[_cons]
return scalar se_gamma1=_se[x2]
return scalar widstat = e(widstat)

/***Standard IV** 
ivreg y x1 (x2=z), r
return scalar b_iv1=_coef[x1] 
return scalar b_iv2=_coef[x2]*/
/*
**Both Generated and standard instruments for improved efficiency**
ivreg y b x1 (x2=zlew z), r 
return scalar b_consfinal=_coef[b]
return scalar b_bothiv1=_coef[x1]
return scalar b_bothiv2=_coef[x2]
*/

end


set seed 1

forvalues i=1/2 {
	forvalues j=1/2 {
		forvalues k=1/2 {
			forvalues m=1/2 {
				forvalues n=1/4 {
				global gamma=`i'
				global beta12=`j'
				global beta22=`k'
				global eps1=`m'
				global eps2=`n'
				di "Combination: `i' `j' `k' `m' `n'"
				qui simulate 						///
					b_beta21=r(b_beta21)			///
					b_beta22=r(b_beta22)			///
					b_beta11=r(b_beta11)			///
					b_beta12=r(b_beta12)			///
					b_gamma1=r(b_gamma1) 			///
					se_gamma1= r(se_gamma1)			///
					widstat=r(widstat)				///
					, reps(100): lewsim
				sum b_gamma1 widstat
				}
			}
		}
	}
}

//Combination Key:
//Constant Treatment effect endogenous regressor
//	Combination 1 1 1 1 1 = Hom errors
//	Combination 1 1 1 1 2 = Het secondary eq error, main eq error hom
//	Combination 1 1 1 2 1 = Het errors main eq, hom secondary eq errors
//	Combination 1 1 1 2 2 = Het errors in both eqs
//	Combination 1 1 2 1 1 = Hom errors, het in coeff on exog regs in secondary eq
//	Combination 1 1 2 1 2 = Het secondary eq error, het in coeff on exog regs in secondary reg
//	Combination 1 1 2 2 1 = Het error main eq, hom error secondary, het exog coeff in secondary eq
//	Combination 1 1 2 2 2 = Het errors, het exog coeff secondary eq
//	Combination 1 2 1 1 1 = Hom errors, het exog coeff main eq
//	Combination 1 2 1 1 2 = Het secondary eq error, het exog coeff main eq
//	Combination 1 2 1 2 1 = Het main eq error, hom secondary eq error, het exog coeff main eq
//	Combination 1 2 1 2 2 = Het errors, het exog coeff main eq
//	Combination 1 2 2 1 1 = Hom errors, het in exog regs in both eqs
//	Combination 1 2 2 1 2 = Het secondary eq error, het in exog regs in both eqs
//	Combination 1 2 2 2 1 = Het main eq error, het in exog regs in both eqs
//	Combination 1 2 2 2 2 = Het errors, het exog coeffs in both eq's
//Heterogeneous endogenous regressor
//	Combination 2 1 1 1 1 = Hom errors
//	Combination 2 1 1 1 2 = Het secondary eq error, main eq error hom
//	Combination 2 1 1 2 1 = Het errors main eq, hom secondary eq errors
//	Combination 2 1 1 2 2 = Het errors in both eqs
//	Combination 2 1 2 1 1 = Hom errors, het in coeff on exog regs in secondary eq
//	Combination 2 1 2 1 2 = Het secondary eq error, het in coeff on exog regs in secondary reg
//	Combination 2 1 2 2 1 = Het error main eq, hom error secondary, het exog coeff in secondary eq
//	Combination 2 1 2 2 2 = Het errors, het exog coeff secondary eq
//	Combination 2 2 1 1 1 = Hom errors, het exog coeff main eq
//	Combination 2 2 1 1 2 = Het secondary eq error, het exog coeff main eq
//	Combination 2 2 1 2 1 = Het main eq error, hom secondary eq error, het exog coeff main eq
//	Combination 2 2 1 2 2 = Het errors, het exog coeff main eq
//	Combination 2 2 2 1 1 = Hom errors, het in exog regs in both eqs
//	Combination 2 2 2 1 2 = Het secondary eq error, het in exog regs in both eqs
//	Combination 2 2 2 2 1 = Het main eq error, het in exog regs in both eqs
//	Combination 2 2 2 2 2 = Het errors, het exog coeffs in both eq's
//Combinations with 3 or 4 include different forms of heteroskedasticity in the
//secondary error equation.

log close

/*
**Test Size**
gen z_gamma1=(b_gamma1-1)/se_gamma1
count if b_gamma1 < -1.96 | b_gamma1 > 1.96 
gen p_gamma1 = 2*(1-normal(abs(z_gamma1)))
cumul p_gamma1, gen(cdf_p_gamma1)
hist p_gamma1, bin(20) percent name(hist_gamma1, replace)

line cdf_p_gamma1 p_gamma1 if p_gamma1<0.20					///
	,												///
	sort											///
	xline(0.05) yline(0.05)							///
	name(cdf_gamma1, replace)
* Combine PDF and CDF graph
graph combine hist_gamma1 cdf_gamma1						///
	,												///
	title("Lewbel Size")							///
	name(gamma1, replace)
graph export gamma1.png, replace

**Test Power**
capture drop hypoth
capture drop power*
gen hypoth = .
gen power_gamma1 = .

local row 1
forvalues beta = 0(.01)2 {
	qui replace hypoth = `beta' if _n == `row'
	capture drop z
	capture drop reject
	qui gen z = (b_gamma1 - `beta')/se_gamma1
	qui gen reject = (z < -1.96 | z > 1.96)
	qui sum reject
	qui replace power_gamma1 = r(mean) if _n == `row'
	local row = `row' + 1
	}
/*	
line power_gamma1 hypoth	///
	,																///
	yline(0.05) xline(1)											///
	ytitle("Rejection frequency") title("Power")					///
	name(power, replace)

line power_gamma1 hypoth if power_gamma1<0.2                 		///
	,																///
	yline(0.05) xline(1)											///
	ytitle("Rejection frequency") title("Power closeup")					///
	name(power_closeup, replace)
*/
