
/*******************************************************************************
******************* FEED THE FUTURE P2-ZOI SURVEY ******************************
**************************   SUMMARY  STATISTICS  ******************************
******************************* [Kenya, 2023] ********************************
********************************************************************************
Syntax file name: TABTYPE_RCI_RCSI_INDICES
Author(s): Jamie Parr @ICF
Date: Aug/2022
Updated by: Jamie Parr @ICF
Update Date: Nov/2024

Description: The objectives of this program are to
   1. Estimate indicator, 95% CI, n, diff, and P-value for BL and ML survey by SUBPOPULATION
      using weighted data
   2. Export results to Excel sheet.

This program includes syntax for two Stata executable commands:
   N.B. This Stata program is developed using the resilience capacities section of the Kenya Feed the Future P2-ZOI Survey 
   Phase 2 MIDLINE core questionnaire. It must be 
   adapted for the final country-specific questionnaire.    
   
*******************************************************************************/   
**X1. Set the system 
set   more off
clear all
set maxvar 30000

**X2. Set global var for working dataset
* global tabname "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Uganda\Stata\Tables\FTF-P2-ZOI-Survey-2022-Uganda-Batch-1+2+3-Table_ICF-response-to-USAID-comments_working-revised_tables-upd.xlsx"

global tabname "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\Backup\Tables\Kenya_2023_P2ZOI_RFZ_Midline-Batch3_RCI_RCSI-v2.xlsx"


cap prog def TABTYPE_RCSI_MEAN

**2. Load data
   use "`1'" if `2',clear

**3. Declare survey design for data set   
   svyset hhea [pw=`3'], strata(stratum) singleunit(scaled)
   
**4. Define Excel file
   putexcel set "$tabname", sheet(`4') modify
  
**5. Set the row into which the outcome will be pasted  
   local row = `5' 

**6. Define disaggregate
   levelsof `6', local(subcat)

**7. Estimate prop, confidence interval and obs

foreach x of varlist `7' {
	foreach i in `subcat' {
		cap svy, subpop(if `6' == `i'): mean `x'
		cap mat est= r(table)

        *Proportion
		cap gen est1=string(est[1,1], "%3.1f")
		cap destring est?,replace
        cap drop es?

        *Lower boundaries of CI
		gen lb1 =string(est[5,1], "%3.1f")
	  
        *Upper boundaries of CI
		gen ub1 =string(est[6,1], "%3.1f")
 	  	  
	    **Confidence interval in [lb - ub] format 
		egen CI1 = concat(lb1 ub1), punct(", ") 
	  
	    **Number of observations
		mat obs = e(_N)
        gen obs1=obs[1,1]
        mat drop obs
		
	    ** List all estimated values      
        cap list est1 CI1 obs1

        ** Replace CI with "—" if obs=0 
        replace CI1="—" if (obs1==0)

        ** Replace CI with "^" if obs>0 & obs<30	
	    replace CI1="^" if (obs1>0 & obs1<30)
	  
        ** Replace CI with "na" if est is 0 or 100 & obs>=30
        replace CI1="na" if (est1==0|est1==100) & obs1>=30
	
        * Populate table
	    *BL
	    putexcel B`row'=est1
	    putexcel C`row'=CI1
	    putexcel D`row'=obs1

        **Update est, diff, pvalue

        **. IF obs=0: Set Est to "—"
	    if (obs1==0)  {
            putexcel B`row'=("—"), hcenter font("Gill Sans MT", 10) nobold
            }	 	  	  
        **. IF (obs>0 & obs<30): Set Est to "^" 
	    if (obs1>0 & obs1<30)  {
	        putexcel B`row'=("^"), hcenter font("Gill Sans MT", 10) nobold
	        }
	 
        ** Drop vars  
      	drop lb? ub? est? CI? obs?
		
        * Move to next row
        local row=`row'+1
		}

}
end


********************************************************************************
//B1. TABTYPE_RCSI_PROP: calculate BINARY indicators **************************
********************************************************************************
cap prog def TABTYPE_RCSI_PROP


**2. Load data
   use "`1'" if `2',clear

**3. Declare survey design for data set   
   svyset hhea [pw=`3'], strata(stratum) singleunit(scaled)
   
**4. Define Excel file
   putexcel set "$tabname", sheet(`4') modify
  
**5. Set the row into which the outcome will be pasted  
   local row = `5' 

**6. Define disaggregate
   levelsof `6', local(subcat)

**7. Estimate prop, confidence interval and obs
   foreach var of varlist `7' {
     foreach i in `subcat' {
      cap estpost svy,subpop(if `6'==`i'): tab `var', col ci perc
      cap mat est=e(b)'
      cap mat lb= e(lb)'
      cap mat ub= e(ub)'
	 
      **Proportions	 
      cap gen est1=string(est[2,1], "%3.1f") //BL
      cap destring est?,replace
      cap mat drop est
	  
	  **Lower limits of CI
      cap gen lb1 =string(lb[2,1], "%3.1f")   //BL 
	  cap mat drop lb?

      **Upper limits of CI
	  cap gen ub1 =string(ub[2,1], "%3.1f") 
	  cap mat drop ub?
	  
      **Set confidence interval in [lb - ub] format
      egen CI1 = concat(lb1 ub1), punct(", ") 
	  
	  **Number of observation
	  mat obs=e(obs)'
	  gen obs1=obs[3,1]
	  mat drop obs
	  	  
**12. List all estimated values      
      cap list est1 CI1 obs1 if _n==1

**13a. Replace CI with "—" if obs=0 
      replace CI1="—" if (obs1==0)

**13b. Replace CI with "^" if obs>0 & obs<30	
	  replace CI1="^" if (obs1>0 & obs1<30)
	  
**13c. Replace CI with "na" if est is 0 or 100 & obs>=30
      replace CI1="na" if (est1==0|est1==100) & obs1>=30
	  
	
	* Populate table
	*BL
	putexcel B`row'=est1
	putexcel C`row'=CI1
	putexcel D`row'=obs1
	putexcel E`row'=("`var'"), left 
	
	drop lb? ub? est? CI? obs?
	local row=`row'+1
	}
   }

end 

********************************************************************************
// TABTYPE_RCSI_MEAN2: calculate MEAN indicators no disaggregates **************************
********************************************************************************


cap prog def TABTYPE_RCSI_MEAN2


**2. Load data
   use "`1'" if `2',clear

**3. Declare survey design for data set   
   svyset hhea [pw=`3'], strata(stratum) singleunit(scaled)
   
**4. Define Excel file
   putexcel set "$tabname", sheet(`4') modify
  
**5. Set the row into which the outcome will be pasted  
   local row = `5' 

**6. Define disaggregate
   levelsof `6', local(subcat)

**7. Estimate prop, confidence interval and obs


foreach x of varlist `7' {
	cap svy: mean `x'
    cap mat est= r(table)

      *Proportion
	cap gen est1=string(est[1,1], "%3.1f")

      *Lower boundaries of CI
	gen lb1 =string(est[5,1], "%3.1f")
	  
      *Upper boundaries of CI
	gen ub1 =string(est[6,1], "%3.1f")
  	cap mat drop es?
 	  	  
	  **Confidence interval in [lb - ub] format 
    egen CI1 = concat(lb1 ub1), punct(", ") 
	  
	**Number of observations
	mat obs = e(_N)
	gen obs1=obs[1,1]
	mat drop obs
	  
	* Populate table
	*BL
	putexcel B`row'=est1
	putexcel C`row'=CI1
	putexcel D`row'=obs1
	putexcel E`row'=("`var'"), left 
	
	drop lb? ub? est? CI? obs?
	local row=`row'+1

}
end