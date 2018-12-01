
cap program drop mpovline
program define mpovline, rclass sortpreserve byable(recall)
	version 10.0, missing
	if c(more)=="on" set more off
	local version : di "version " string(_caller()) ", missing:"
	syntax varlist(numeric min=1 max=1) [if] [in] [aweight fweight], [varpl(varname numeric) LINEs(numlist sort) mpl(numlist sort) INdicator(string) max]	       
			            		                                 
	tempvar  w wwvar point0 point1 aux
	marksample touse
	
	if ("`mpl'"~="")&("`varpl'"=="") {
		di in red "You must specify a poverty line variable" _new
		exit 198
	}
	if ("`lines'"=="")&("`varpl'"=="") {
		di in red "You must specify a poverty line variable" _new
		exit 198
	}
	
	qui {	
		* * Indicator
		if ("`indicator'"=="") {
			local fgt0 "fgt0"
		}
		else {
			local t 0
			forvalues i = 0(1)2 {
				local t = regexm("`indicator'","fgt`i'") + `t'
			}
			if `t'==0 {
				di in red "Indicator not valid"
				exit 198
			}	
			if regexm("`indicator'","fgt0")!=0 local fgt0 "fgt0"
			if regexm("`indicator'","fgt1")!=0 local fgt1 "fgt1"
			if regexm("`indicator'","fgt2")!=0 local fgt2 "fgt2"
		}	
		
		* * Weight variable
		if ("`weight'"=="") {
			gen `w' = 1
			local wvar "`w'"
		}	
		else {
			local weight "[`weight'`exp']"				
			local wvar : word 2 of `exp'
		}
		sum `wvar'  if `touse' , meanonly
		local pop_sum = r(sum)
		local t_obs = r(N)
		gen double `wwvar' = `wvar'/`pop_sum'
		
		
		* * Points
		sum `varlist' if `touse' , meanonly
		local min `r(min)'
		local max1 `r(max)'
		
		if ("`max'"=="") local max1 ""	
		
		* * varpl
		if ("`varpl'"=="") {
			tempvar varpl
			gen byte `varpl' = 1
			local mpl "`lines'"
		}	
		if ("`mpl'"=="") local mpl "1"
	
		gen double `point0' = .
		gen double `point1' = .
		gen double `aux' = .
		local list "`min' `mpl'"
		
		local signal "<"
		
		cap mat drop fgt
		cap mat drop fgt0
		cap mat drop fgt1
		cap mat drop fgt2
		
		local cont 0
		foreach pt1 in `mpl' `max1' {
			
			local ++cont
			local pt0 : word `cont' of `list'
				
			replace `point0' = `pt0'*`varpl'
			replace `point1' = `pt1'*`varpl'
			
			if 	"`pt1'"=="`max1'" {
				replace `point1' = `max1'
				local signal "<="
			}			
				
		
			/* Indicators Calculation */
					
					
			* ** fgt0					
				
			tempvar in
			gen double `in' = (`varlist'>=(`point0') & `varlist'`signal'(`point1')) if `touse'
			if("`fgt0'"~="") {	
				sum `wvar' if `touse' & `in'==1, meanonly
				local pop_sum_in `r(sum)'
				local fgt0 = 100*`pop_sum_in'/`pop_sum'
													
				mat fgt = nullmat(fgt) \ (0, `cont', `fgt0')
				mat fgt0 = nullmat(fgt0) \ (`cont', `fgt0')	
				mat colnames fgt0 = pline rate	
			}			
			* ** fgt1
			if("`fgt1'"~="") {		
				replace `aux' = 100*`in'*`wwvar'*(((`point1')-`varlist')/(`point1')) if `touse' 
				sum `aux'  if `touse' , meanonly
				local fgt1  `r(sum)'
							
				mat fgt = nullmat(fgt) \ (1, `cont', `fgt1')
				mat fgt1 = nullmat(fgt1) \ (`cont', `fgt1')	
				mat colnames fgt1 = pline rate	
			}
						
			
			* ** fgt2
			if("`fgt2'"~="") {					
				replace `aux' = 100*`in'*`wwvar'*((((`point1')-`varlist')/(`point1'))^2) if `touse' 
				sum `aux'  if `touse' , meanonly
				local fgt2 `r(sum)'
						
				mat fgt = nullmat(fgt) \ (2, `cont', `fgt2')	
				mat fgt2 = nullmat(fgt2) \ (`cont', `fgt2')		
				mat colnames fgt2 = pline rate		
			}	
		}	
		
	}

tempvar index povertyline rate
mat colnames fgt = `index' `povertyline' `rate'
svmat fgt, n(col)
* ** Show Results
	
label define `index' 	      ///
    0 "Headcount ratio - FGT(0)" ///
	1 "Poverty Gap - FGT(1)"   ///
 	2 "Poverty Severity - FGT(2)"   	     
label values `index' `index'
		
label var `index' "Indicator"
label var `rate' "`varlist'"

local line ""
local aux = wordcount("`mpl'")
if (`aux'!=1) | ("`max'"!="") {
	local line "`povertyline'"			
	label var `povertyline' "Poverty line range"
	cap tab `povertyline', matrow(a)
	local m = `r(r)'
	if ("`max'"!="") local m = `r(r)'-1
	if ("`max'"!="") label define `povertyline' `r(r)' "max"
	forvalues i =1(1)`m' {
		local j : word `i' of `mpl' 
		label define `povertyline' `i' "`j'", add	
	}
	label values `povertyline' `povertyline'
}	
	
tabdisp `index' `line'  if `index'!=. , cell(`rate') format(%12.2fc)
	
		 
*** Save results		
  		
if ("`fgt2'"~="") return matrix fgt2 = fgt2
if ("`fgt1'"~="") return matrix fgt1 = fgt1
if ("`fgt0'"~="") return matrix fgt0 = fgt0
		
mat colnames fgt = index pline rate
return matrix b = fgt
	
end			
			