* version 1.0.0         <march2012>         Viviane Sanfelice


cap program drop mpoverty
program define mpoverty, rclass sortpreserve
	version 9.0
		syntax varlist(numeric min=1 max=1) ///
			    [if] [in]                           ///
			    [aweight fweight] [,        ///
				pl(numlist sort)			        ///
				varpl(varname numeric)				///
				mpl(numlist sort)					///
				all									///
				fgt0								///
				fgt1								///
				fgt2 								///
				stat								///
				max									///
				NOOUTlier                              ///
				]
				marksample touse
				tempvar w wwvar aux varindex varpoint0 varpoint1 index rate points
			
	
			
			
qui {
	
	*** Error messages
	if ("`mpl'"~="")&("`varpl'"=="") {
		di in red "You must specify a poverty line variable" _new
				exit 198
	}
	if ("`nooutlier'"~="")&("`max'"=="") {
		di in red "This option must be combined with max" _new
				exit 198
	}
	
	
	*** Indicator to be calculated
	if "`all'"~="" {
		loc fgt0 = "t"
		loc fgt1 = "t"
		loc fgt2 = "t"
	}
	
	
	if("`max'"~="")&("`nooutlier'"~=""){
			
		tempvar outlier
		
		sum `varlist' if `touse' 
		local max_v = r(max)
		local mean_v = r(mean)
		local sd_v = r(sd)
		local N = r(N)
		
		*** Grubbs Test
		local t2=(invttail(`N'-2,0.01/(2*`N')))^2                       
		local g =((`N'-1)/sqrt(`N'))*sqrt(`t2'/(`N'-2+`t2'))
		
		gen `outlier' = ((abs(`varlist' - `mean_v')/`sd_v')>`g')
		local out =" & `outlier'!=1"
	}

	
	
		** Weight variable
		if ("`weight'"=="") {
			gen `w' = 1
			local wvar "`w'"
		}	
		else {
			local weight "[`weight'`exp']"				
			local wvar : word 2 of `exp'
		}
		
		
		sum `wvar'  if `touse' `out'
		local pop_sum = r(sum)
		local t_obs = r(N)
		
		gen `wwvar' = `wvar'/`pop_sum'
		
		
		** Values
		
		gen `aux'=. if `touse' `out'
		gen `varpoint0'=.
		gen `varpoint1'=.
		gen `index'=.
		gen `rate'=.
		
		
		
		sum `varlist' if `touse' `out'
		local min_v = r(min) 
		local max_v = r(max)		
	
				
		
		if ("`pl'"~=""){	
		
		local values = "`pl'"
		local aux_v = ""
				
		}
		
		if ("`mpl'"~=""){

		local values = "`mpl'"
		local aux_v = "`varpl'*"
		
		}
		
		if ("`varpl'"~="")&("`mpl'"==""){

		local values = "1"
		local aux_v = "`varpl'*"
		
		}
		
		if("`max'"~=""){
		
		local max = `max_v'
		
		}
		
	
		local value0 = `min_v'
		local n_point1 = 0
		local i  = 0
		
		
		foreach value in `values' `max'{		
										
				local point0 = "`value0'"
				local point1 = "`aux_v'`value'"
				local value0 = "`aux_v'`value'"
				local n_point0 = `n_point1'
				local n_point1 = `value'
				
				if(`value'==`max_v'){
				local point1 = "`value'"
				local n_point1 = int(`max')
				}
				
				
				tempvar in j ratio	
								
		
				* Observation on range
				sum `varlist' if (`varlist'>=`point0' & `varlist'<=`point1') & `touse' `out'
				local obs = r(N) 
				
				if (`obs'<=30) {
				di in red "The range used must have more than 30 observations" _new
				exit 198
				}
				
				else{	
				
					* Take out range not useful
					if (`point0' >= `point1') | (r(min) == r(max)) | (r(N) == 0){
					
					}
					
					
					else{
					
					/* Descriptive Estatistics */
					
						sum `varlist' `weight' if (`varlist'>=(`point0') & `varlist'<=(`point1')) & `touse' `out', detail
										
						mat E = nullmat(E)\ (`n_point0', `n_point1', r(N), 100*r(N)/`t_obs', r(p50), r(mean), r(sd), r(min), r(max))
					
					
					
					/* Indicators Calculation */
					
						*** fgt0
						local i = `i'+1
						
						gen `in' = (`varlist'>=(`point0') & `varlist'<(`point1'))
						sum `wvar' if `touse' `out' & `in'==1
						local pop_sum_in = r(sum)
						local fgt0 = 100*`pop_sum_in'/`pop_sum'
						
						replace `varpoint0'= `n_point0' in `i'
						replace `varpoint1'= `n_point1' in `i'
						replace `index'= 0 in `i'
						replace `rate' = `fgt0' in `i' 
												
						mat fgt0 = nullmat(fgt0) \ (`fgt0')
						
						
						*** fgt1
						if("`fgt1'"~="") {
						
						local i = `i'+1
						
						replace `aux' = 100*`in'*`wwvar'*(((`point1')-`varlist')/(`point1')) if `touse' `out'
						sum `aux'  if `touse' `out'
						local fgt1 = r(sum)
						
						replace `varpoint0'= `n_point0' in `i'
						replace `varpoint1'= `n_point1' in `i'
						replace `index'= 1 in `i'
						replace `rate' = `fgt1' in `i'
												
						mat fgt1 = nullmat(fgt1) \ (`fgt1')
			
						}
						
						
						*** fgt2
						if("`fgt2'"~="") {
						
						local i = `i'+1
						
						replace `aux' = 100*`in'*`wwvar'*((((`point1')-`varlist')/(`point1'))^2) if `touse' `out'
						sum `aux'  if `touse' `out'
						local fgt2 = r(sum)
						
						replace `varpoint0'= `n_point0' in `i'
						replace `varpoint1'= `n_point1' in `i'
						replace `index'= 2 in `i'
						replace `rate' = `fgt2' in `i'
												
						mat fgt2 = nullmat(fgt2) \ (`fgt2')
						
						}					
										
					
					
					}
					
				}	
		 
		}
		

}		
		
		*** Show Descriptive Statistics
		if ("`stat'"~=""){
		tempvar mt1 mt2 mt3 mt4 mt5 mt6 mt7 mt8 mt9 
		mat colnames E = `mt1' `mt2' `mt3' `mt4' `mt5' `mt6' `mt7' `mt8' `mt9' 
		svmat E, n(col)
		label var `mt2' "Poverty Line"
		label var `mt4' "% Obs"
		label var `mt5' "Median"
		label var `mt6' "Mean"
		label var `mt7' "Std. Dev."
		label var `mt8' "Min"
		label var `mt9' "Max"
		
		tabdisp `mt2'  if `mt2'!=., cell(`mt4' `mt6' `mt7' `mt8' `mt9' ) format(%12.2fc) 
		}
		
		
		*** Show Results
		
		label define `index' 	      ///
		 0 "Headcount ratio - FGT(0)" ///
		 1 "Poverty Gap % - FGT(1)"   ///
 		 2 "Index FGT(2)"   	     
		label values `index' `index'
		
		
		label var `index' "Indicator"
		label var `rate' "`varlist'"
		
		gen `points' = `varpoint1'
		replace `points' = int(`varpoint1') if `varpoint1'== `max_v'
		local aux = int(`max_v') 
		label define `points' `aux' "Max"
		label values `points' `points'
		
		if ("`pl'"~=""){	
		label var `points' "Poverty Line"
		format %8.2f `points'
		tabdisp `index' `points' if `index'!=. , cell( `rate') format(%12.3fc)
		}
		
		if ("`mpl'"~="") {
		label var `points' "Poverty Line - Multiple"
		format %8.1f `points'
		tabdisp `index' `points' if `index'!=. , cell( `rate') format(%12.3fc)
		}
		
		 
		*** Save results		
		sort `index' `varpoint0' `varpoint1'
		mkmat `index' `varpoint0' `varpoint1' `rate' , matrix(b) nomissing
         
			
		if ("`fgt2'"~="") return matrix fgt2 = fgt2
		if ("`fgt1'"~="") return matrix fgt1 = fgt1
		
		return matrix fgt0 = fgt0
		mat colnames b = index point0 point1 rate
		return matrix b = b
		mat colnames E = point0 point1 obs %obs median mean sd min max 
		return matrix stat = E
	
				
	

end
