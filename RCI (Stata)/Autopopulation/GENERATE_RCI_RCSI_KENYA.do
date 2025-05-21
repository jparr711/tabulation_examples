
/*******************************************************************************
******************* FEED THE FUTURE P2-ZOI SURVEY ******************************
**************************   SUMMARY  STATISTICS  ******************************
******************************* [Kenya, 2023] ********************************
********************************************************************************
Syntax file name: GENERATE_RCI_RCSI_INDICES
Author(s): Jamie Parr @ICF
Date: Aug/2022
Updated by: Jamie Parr @ICF
Update Date: Nov/2024

Description: The objectives of this program are to
   1. Estimate indicator, 95% CI, n, diff, and P-value for BL and ML survey by SUBPOPULATION
      using weighted data
   2. Export results to Excel sheet.

   N.B. This Stata program is developed using the resilience capacities section of the Kenya Feed the Future P2-ZOI Survey 
   Phase 2 MIDLINE core questionnaire. It must be 
   adapted for the final country-specific questionnaire.    
   
*******************************************************************************/   

* Set Working Environment
*************************************************************
set more off
clear 	all
set 	maxvar 30000
macro drop _all

global syntax "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Syntax"
global analytic  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\03. Processed_Data\Analytic Data"
global analytic2  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Analytic" 
global log "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Log"
global rcidata "$analytic2\FTF_P2-ZOI_2023_Kenya_ML_RCI_vars.dta"


				
				

* svyset 	hhea [pw=wgt_hh], strata(stratum) singleunit(scaled)

* REVISE -----------
* 

**** RESILIENCE CAPACITY INDEX ******************
********************************************************************************
// 5.4.6 ***********************
********************************************************************************
**1. Define global var for table name
global sheet T5.4.6

**2. Define log file
cap log using "$log/$sheet.log",replace

**3. Set global variables (if needed)
global subzone subzone==2|subzone==3
global all 		all==1

**4. Load TABTYPE_RCSI into Stata memory (we will use the RCSI calculation ssince it's the same)
quiet do "$syntax\TABTYPE_RCSI_KENYA"

**5. Run TABTYPE_RCSI_MEAN 
TABTYPE_RCSI_MEAN "$rcidata" "$subzone" wgt_hh "$sheet" 4   all           	 resil_index_rs
TABTYPE_RCSI_MEAN "$rcidata" "$subzone" wgt_hh "$sheet" 6   genhhtype_dj  	 resil_index_rs
TABTYPE_RCSI_MEAN "$rcidata" "$subzone" wgt_hh "$sheet" 11  awiquint_rev  	  	 resil_index_rs
TABTYPE_RCSI_MEAN "$rcidata" "$subzone" wgt_hh "$sheet" 17  shock_sev resil_index_rs
TABTYPE_RCSI_MEAN "$rcidata" "$all" wgt_hh "$sheet" 22  all resil_index_rs

macro drop sheet subzone all
drop est1 lb1 ub1 CI1 obs1
log close

**************************************************************
***** AUTOPOPULATE RESILIENCE CAPACITIES
**************************************************************
* 1. ABSORPTIVE
global i_absorptive_rs absorb_rs i_snet_informal_rs i_scap_bond_rs i_access_savings_rs i_access_remittance_rs  i_assets_owned_rs i_shock_prep_rs i_access_ins_rs i_access_assist_rs
sum 	$i_absorptive_rs

svyset 	hhea [pw=wgt_hh], strata(stratum) singleunit(scaled)

svy:  mean $i_absorptive_rs

putexcel set "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\Backup\Tables\Kenya_2023_P2ZOI_RFZ_Midline-Batch3_RCI_RCSI-v1.xlsx", sheet("T5.4.7") modify

local 	row=5
foreach x of varlist $i_absorptive_rs {
	cap svy: mean `x' if all_exc_samburu == 1
    cap mat est= r(table)

      *Proportion
	cap gen est1=string(est[1,1], "%3.2f")
	cap destring est?,replace

      *Lower boundaries of CI
	gen lb1 =string(est[5,1], "%3.1f")
	  
      *Upper boundaries of CI
	gen ub1 =string(est[6,1], "%3.1f")
 	  	  
	  **Confidence interval in [lb - ub] format 
    egen CI1 = concat(lb1 ub1), punct(", ") 
	  
	**Number of observations
	mat obs = e(_N)
	gen obs1 = obs[1,1]
	mat drop obs
	mat drop est
	* Populate table
	*BL
	putexcel D`row'=est1
	putexcel E`row'=CI1
	putexcel F`row'=obs1
	
	drop lb? ub? est? CI? obs?
	
	local row=`row'+1
}		



global 	absorptive i_snet_informal i_scap_bond i_access_savings i_access_remittance i_assets_owned i_shock_prep i_access_ins i_access_assist
sum 	$absorptive

local 	row=6
foreach x of varlist $absorptive {
	svy: mean `x'  if all_exc_samburu == 1
	mat a=r(table)
	putexcel C`row'=a[1,1]
	local row=`row'+1
}


* 2. ADAPTIVE

global i_adapt_rs adapt_rs i_aspiration_rs i_scap_bridge_rs i_scap_link_rs i_snet_index_rs i_edu_rs i_lhood_rs i_exposure_info_rs /*i_ag_improved_rs*/ i_assets_owned_rs i_service_fin_rs
sum $i_adapt_rs



local 	row=14
foreach x of varlist $i_adapt_rs {
	cap svy: mean `x'  if all_exc_samburu == 1
    cap mat est= r(table)

      *Proportion
	cap gen est1=string(est[1,1], "%3.2f")
	cap destring est?,replace
  	cap drop es?

      *Lower boundaries of CI
	gen lb1 =string(est[5,1], "%3.1f")
	  
      *Upper boundaries of CI
	gen ub1 =string(est[6,1], "%3.1f")
 	  	  
	  **Confidence interval in [lb - ub] format 
    egen CI1 = concat(lb1 ub1), punct(", ") 
	mat drop est
  
	**Number of observations
	mat obs = e(_N)
	gen obs1 = obs[1,1]
	mat drop obs
	
	* Populate table
	*BL
	putexcel D`row'=est1
	putexcel E`row'=CI1
	putexcel F`row'=obs1
	
	drop lb? ub? est? CI? obs?
	
	local row=`row'+1

}

global adaptive i_aspiration i_scap_bridge i_scap_link i_snet_index i_edu i_lhood i_exposure_info /*i_ag_improved*/ i_assets_owned i_service_fin


sum $adaptive


local row=15
foreach x of varlist $adaptive {
	svy: mean `x'  if all_exc_samburu == 1
	mat a=r(table)
	putexcel C`row'=a[1,1]
	local row=`row'+1
}

********TRANSFORMATIVE CAPACITY**********
global i_transform_rs transform_rs i_snet_formal_rs i_access_market_rs i_access_commune_rs i_access_basic_rs i_access_infra_rs i_service_ag_rs i_service_vet_rs i_scap_bridge_rs i_scap_link_rs i_collective_act_rs i_social_coh_rs  i_local_dm_rs i_gender_hhindex_rs /* i_equit_dm_rs */

sum $i_transform_rs



local 	row=24
foreach x of varlist $i_transform_rs {
	cap svy: mean `x'  if all_exc_samburu == 1
    cap mat est= r(table)

      *Proportion
	cap gen est1=string(est[1,1], "%3.2f")
	cap destring est?,replace

      *Lower boundaries of CI
	gen lb1 =string(est[5,1], "%3.1f")
	  
      *Upper boundaries of CI
	gen ub1 =string(est[6,1], "%3.1f")
 	  	  
	  **Confidence interval in [lb - ub] format 
    egen CI1 = concat(lb1 ub1), punct(", ") 
	  
	**Number of observations
	mat obs = e(_N)
	gen obs1 = obs[1,1]
	mat drop obs
	mat drop est
	
	* Populate table
	*BL
	putexcel D`row'=est1
	putexcel E`row'=CI1
	putexcel F`row'=obs1
	
	drop lb? ub? est? CI? obs? 
	
	local row=`row'+1

}

global transformative i_snet_formal i_access_market i_access_commune i_access_basic i_access_infra i_service_ag i_service_vet i_scap_bridge i_scap_link i_collective_act i_social_coh i_local_dm i_gender_hhindex /*i_equit_dm */ 

sum $transformative
local row=25
foreach x of varlist $transformative {
	svy: mean `x'  if all_exc_samburu == 1
	mat a=r(table)
	putexcel C`row'=a[1,1]
	local row=`row'+1
}

svy: mean absorb_rs if all ==1
svy: mean adapt_rs if all ==1
svy: mean transform_rs if all ==1


//6. INDEX OF COMMUNITY RESILIENCE 
putexcel set "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\Backup\Tables\Kenya_2023_P2ZOI_RFZ_Midline-Batch3_RCI_RCSI-v1.xlsx", sheet("T5.4.8") modify

global comm_resil_rs comm_resil_index_rs nrm_grp_rs comm_risk_rs soc_protect_rs pub_goods_rs conflict_mit_rs 


local 	row=5
foreach x of varlist $comm_resil_rs {
		cap svy: mean `x' if all_exc_samburu==1
    cap mat est= r(table)

      *Proportion
	cap gen est1=string(est[1,1], "%3.2f")
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
	gen obs1 = obs[1,1]
	mat drop obs
	mat drop est
	
	* Populate table
	*BL
	putexcel D`row'=est1
	putexcel E`row'=CI1
	putexcel F`row'=obs1
	
	drop lb? ub? est? CI? obs? 
	
	local row=`row'+1

}

global comm_resil nrm_grp comm_risk soc_protect pub_goods conflict_mit


local row=6
foreach x of varlist $comm_resil {
	svy: mean `x' if all_exc_samburu==1
	mat a=r(table)
	putexcel C`row'=a[1,1]
	local row=`row'+1
}

macro drop sheet rfz 
log close

svy: mean comm_resil_index_rs if all==1
	

************** AUTOPOPULATE ************************
** T5.7.6-5.7.8
macro drop _all


global syntax "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Syntax"
global analytic  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\03. Processed_Data\Analytic Data"
global analytic2  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Analytic" 
global log "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Log"
global rcsidata "$analytic2\FTF_P2-ZOI_Survey_Kenya_2023_rcsi.dta"
********************************************************************************
// 5.4.9 ***********************
********************************************************************************

**1. Define global var for table name
global sheet T5.4.9

**2. Define log file
cap log using "$log/$sheet.log",replace

**3. Set global variables (if needed)
global all_exc_samburu all_exc_samburu==1

**4. Load TABTYPE_NUTRI into Stata memory
quiet do "$syntax\TABTYPE_RCSI_KENYA"

**5. Run TABTYPE_RCSI_MEAN 
TABTYPE_RCSI_MEAN "$rcsidata" "$all_exc_samburu" wgt_hh "$sheet" 4   all           	 rcsi 
TABTYPE_RCSI_MEAN "$rcsidata" "$all_exc_samburu" wgt_hh "$sheet" 6   genhhtype_dj  	 rcsi
TABTYPE_RCSI_MEAN "$rcsidata" "$all_exc_samburu" wgt_hh "$sheet" 17  awiquint_rev  	  	 rcsi
TABTYPE_RCSI_MEAN "$rcsidata" "$all_exc_samburu" wgt_hh "$sheet" 23  shock_sev rcsi
TABTYPE_RCSI_MEAN "$rcsidata" "all == 1" wgt_hh "$sheet" 28  all rcsi


macro drop sheet all_exc_samburu
log close

********************************************************************************

********************************************************************************
// 5.4.10 ***********************
********************************************************************************

macro drop _all

global syntax "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Syntax"
global analytic  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\03. Processed_Data\Analytic Data"
global analytic2  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Analytic" 
global log "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Log"
global rcsidata "$analytic2\FTF_P2-ZOI_Survey_Kenya_2023_rcsi.dta"

**1. Define global var for table name
global sheet T5.4.10

**2. Define log file
cap log using "$log/$sheet.log",replace

**3. Set global variables (if needed)
global all_exc_samburu all_exc_samburu==1

**4. Load TABTYPE_NUTRI into Stata memory
quiet do "$syntax\TABTYPE_RCSI_KENYA"

**5. Run TABTYPE_RCSI_PROP 
TABTYPE_RCSI_PROP "$rcsidata" "$all_exc_samburu" wgt_hh "$sheet" 3   all           "v327_bin  v335_bin v339_bin  v328_bin v336_bin"

********************************************************************************
* Calculate number of households for T5.4.10

estpost tab v327_bin
mat obs=e(b)

gen xtot_bl=obs[1,3]
gen xtot_ml=obs[1,6]

macro drop obs

**7b. Export to Excel
putexcel B8=(xtot_bl)
		 
macro drop sheet all_exc_samburu 
*log close

********************************************************************************
// T5.4.11 ***********************
********************************************************************************

macro drop _all

global syntax "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Syntax"
global analytic  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\03. Processed_Data\Analytic Data"
global analytic2  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Analytic" 
global log "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Autopopulation\Batch 3\Log"
global rcsidata "$analytic2\FTF_P2-ZOI_Survey_Kenya_2023_rcsi.dta"


**1. Define global var for table name
global sheet T5.4.11

**2. Define log file
cap log using "$log/$sheet.log",replace

**3. Set global variables (if needed)
global all_exc_samburu all_exc_samburu==1

**4. Load TABTYPE_NUTRI into Stata memory
quiet do "$syntax\TABTYPE_RCSI_KENYA"

**5. Run TABTYPE_RCSI_PROP 
TABTYPE_RCSI_MEAN2 "$rcsidata" "$all_exc_samburu" wgt_hh "$sheet" 3  all           "v327_time v335_time  v339_time  v328_time  v336_time "

macro drop sheet all_exc_samburu
log close




