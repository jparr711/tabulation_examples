/*******************************************************************************
************************* FEED THE FUTURE ZOI SURVEY ***************************
***************************     RESILIENCE         ***************************
************************** [KENYA Midline, 2023]    **************************
********************************************************************************
**Description: This code is intended to calculate the absorptive, adaptive, and
** transformative indexes, as well as the index of community resilience per the
**Center for Resilience's methodolgies, with some adaptions to align with the 
**Kenya P2-ZOI-RFZ Survey 2023 questionnaire.

**Prepared by Kirsten Zalisk @ ICF, 2020
********************************************************************************/
**
** Updated for P2-ZOI_RFZ Kenya 2023: Jamie Parr & Athena Pantazis Mar 2024
**----------------------------------------------------------------------------**

set   more off
clear all
set maxvar 30000
macro drop _all

*******************Update the file paths, etc

//DIRECTORY PATH
global syntax	 "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Syntax\Resilience" 
global analytic2  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Analytic" 
global source    "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\"
global source2 "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\02. Raw_Data"
global analytic  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\03. Processed_Data\Analytic Data"
global log	     "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\log" 


///Input(s):     	  $analytic\Uganda_ P2-ZOI_Survey_2022 household data analytic.dta
///                   $analytic\Uganda_ P2-ZOI_Survey_2022 persons data analytic.dta
///Log Outputs(s):	  $analytic\Log\Uganda_ P2-ZOI_Survey_2022 wealthindex AWI.log
///Data Outputs(s):	  $analytic\Results\Uganda_ P2-ZOI_Survey_2022 wealthindex AWI.dta
///Syntax file Name:  $syntax\Uganda_ P2-ZOI_Survey_2022 wealthindex AWI.do 

capture log close
log using "$log\FTF_P2-ZOI_Survey_Kenya_2023_RCI.log", replace	

********************************************************************************
***** CALCULATE RESILIENCE CAPACITIES INDICES
********************************************************************************

use "$analytic2\Temp\Kenya_2023-RCI_Component_Indicators.dta", clear

drop if c06a==2
drop if vrresult!=1
*1. ABSORPTIVE CAPACITY

*global absorptive i_snet_informal i_scap_bond i_access_savings i_access_remittance ///
*				  i_assets_owned i_shock_prep i_access_ins i_access_assist

//* scap_bond, scap_bridge instead of i_scap_*				  
global absorptive i_snet_informal i_scap_bond i_access_savings i_access_remittance ///
				  i_assets_owned i_shock_prep i_access_ins i_access_assist

sum $absorptive

foreach x of varlist $absorptive {
*  replace `x'=. if r001r!=1 | (m6100d!=1 & v6result!=1)
  replace `x'=. if vrresult!=1 
  egen z`x'=std(`x')
}

global z_absorptive zi_snet_informal zi_scap_bond zi_access_savings zi_access_remittance ///
					zi_assets_owned zi_shock_prep zi_access_ins zi_access_assist
sum $z_absorptive

/*
*svy, subpop(hh_has_dj):  mean $absorptive
*svy, subpop(hh_has_dj): mean $z_absorptive

local row=4
foreach x of varlist $absorptive {
  svy, subpop(hh_has_dj): mean `x'
  mat a=r(table)
  putexcel G`row'=a[1,1]
  local row=`row'+1
}
*/
*corr $absorptive if hh_has_dj
corr $z_absorptive 

*pca $absorptive if hh_has_dj
*pca $z_absorptive if hh_has_dj

factor $z_absorptive, pcf
estat kmo // 0.6309
predict absorb 
sum absorb 

*Rescale index and components on a scale of 0-100
foreach x of varlist absorb $absorptive {         
  su `x'  
  gen `x'_rs = ((`x' - r(min))/(r(max) - r(min)))*100  
  local lbl : variable label `x'
  local new_lbl="Rescaled (0-100): "+"`lbl'"
  la var `x'_rs "`new_lbl'"
  sum `x'_rs
}

global i_absorptive_rs absorb_rs i_snet_informal_rs i_scap_bond_rs i_access_savings_rs ///
					   i_access_remittance_rs  i_assets_owned_rs i_shock_prep_rs ///
					   i_access_ins_rs i_access_assist_rs
sum $i_absorptive_rs
/*
local row=3
foreach x of varlist $i_absorptive_rs {
  svy, subpop(hh_has_dj): mean `x'
  mat a=r(table)
  putexcel H`row'=a[1,1]
  local row=`row'+1
}

local row=3
foreach x of varlist $i_absorptive_rs {
  svy, subpop(hh_has_dj): mean `x', over(c06)
  mat b=r(table)
  putexcel N`row'=b[1,1] O`row'=b[1,2] P`row'=b[1,3] Q`row'=b[1,4] 
  local row=`row'+1
}

foreach x of varlist $i_absorptive_rs {
  svy, subpop(hh_has_dj): regress `x' i.c06
}
*/
******ADAPTIVE CAPACITY*****************

global adaptive i_aspiration i_scap_bridge i_scap_link i_snet_index i_edu i_lhood ///
				i_exposure_info /*i_ag_improved*/ /*i_assets_owned*/ i_service_fin

foreach x of varlist $adaptive {
*  replace `x'=. if r001r!=1  | (m6100d!=1 & v6result!=1)
  replace `x'=. if vrresult!=1  
  egen z`x'=std(`x')
}

sum $adaptive

global z_adaptive zi_aspiration zi_scap_bridge zi_scap_link zi_snet_index zi_edu zi_lhood ///
				zi_exposure_info /*zi_ag_improved zi_assets_owned*/ zi_service_fin
sum $z_adaptive

factor $z_adaptive, pcf
estat kmo  // 0.6906
predict adapt
sum adapt, detail

*Rescale index and components on a scale of 0-100

foreach x of varlist adapt $adaptive {         
  su `x'  
  gen `x'_rs = ((`x' - r(min))/(r(max) - r(min)))*100  
  local lbl : variable label `x'
  local new_lbl="Rescaled (0-100): "+"`lbl'"
  la var `x'_rs "`new_lbl'"
  sum `x'_rs
}

la var adapt "Score for factor 2"
la var adapt_rs "Rescale (0-100): Scores for factor "

global i_adaptive_rs adapt_rs i_aspiration_rs i_scap_bridge_rs i_scap_link_rs ///
					 i_snet_index_rs i_edu_rs i_lhood_rs i_exposure_info_rs ///
				     /*i_ag_improved_rs i_assets_owned_rs*/ i_service_fin_rs
sum $i_adaptive_rs
/*
foreach x of varlist $i_adaptive_rs {
  svy, subpop(hh_has_dj): mean `x'
}

local row=12
foreach x of varlist $i_adaptive_rs {
  svy, subpop(hh_has_dj): mean `x'
  mat a=r(table)
  putexcel H`row'=a[1,1]
  local row=`row'+1
}

local row=12
foreach x of varlist $i_adaptive_rs {
  svy, subpop(hh_has_dj): mean `x', over(c06)
  mat b=r(table)
  putexcel N`row'=b[1,1] O`row'=b[1,2] P`row'=b[1,3] Q`row'=b[1,4] 
  local row=`row'+1
}

foreach x of varlist $i_adaptive_rs {
  svy, subpop(hh_has_dj): regress `x' i.c06
}

global adaptive2 i_aspiration i_scap_bridge i_scap_link i_snet_index i_edu i_lhood ///
				i_exposure_info /*i_ag_improved i_assets_owned*/ i_service_fin
				
local row=13
foreach x of varlist $adaptive2 {
  svy, subpop(hh_has_dj): mean `x'
  mat a=r(table)
  putexcel G`row'=a[1,1]
  local row=`row'+1
}
*/
*
********************************************************************************
********              TRANSFORMATIVE CAPACITY                **********
sum i_local_dm i_gender_hhindex 
global transformative i_snet_formal i_access_market i_access_commune i_access_basic ///
					  i_access_infra i_service_ag i_service_vet /*i_scap_bridge i_scap_link*/ ///
					  i_collective_act i_social_coh /* i_equit_dm */ i_local_dm i_gender_hhindex 

sum $transformative

foreach x of varlist $transformative {
*  replace `x'=. if r001r!=1 | (m6100d!=1 & v6result!=1)
  replace `x'=. if vrresult!=1  
  egen z`x'=std(`x')
}

global z_transformative zi_snet_formal zi_access_market zi_access_commune zi_access_basic ///
					  zi_access_infra zi_service_ag zi_service_vet zi_scap_bridge zi_scap_link ///
					  zi_collective_act zi_social_coh /*zi_equit_dm*/ zi_local_dm zi_gender_hhindex 
sum $z_transformative

sum $z_transformative

factor $z_transformative, pcf
estat kmo  // 0.6964
predict transform
sum transform 

*Rescale index and components on a scale of 0-100
foreach x of varlist transform $transformative  {         
  su `x'  
  gen `x'_rs = ((`x' - r(min))/(r(max) - r(min)))*100  
  local lbl : variable label `x'
  local new_lbl="Rescaled (0-100): "+"`lbl'"
  la var `x'_rs "`new_lbl'"
  sum `x'_rs
}
la var transform "Score for factor 3"
la var transform_rs "Rescale (0-100): Scores for factor 3"
/*
global i_transform_rs transform_rs i_snet_formal_rs i_access_market_rs i_access_commune_rs ///
					  i_access_basic_rs i_access_infra_rs i_service_ag_rs i_service_vet_rs ///
					  i_scap_bridge_rs i_scap_link_rs i_collective_act_rs i_social_coh_rs ///
					 /* i_equit_dm_rs*/ i_local_dm_rs i_gender_hhindex_rs 
sum $i_transform_rs

foreach x of varlist $i_transform_rs {
  svy, subpop(hh_has_dj): mean `x'
}

local row=22
foreach x of varlist $i_transform_rs {
  svy, subpop(hh_has_dj): mean `x'
  mat a=r(table)
  putexcel H`row'=a[1,1]
  local row=`row'+1
}

svy, subpop(if hh_has_dj==1): mean i_equit_dm_rs
  mat a=r(table)
  putexcel H34=a[1,1]
svy, subpop(if hh_has_dj==1 & r1501==1 & r1404a==1 & r1508!="X"): mean i_gender_hhindex_rs
  mat a=r(table)
  putexcel H35=a[1,1]

  
local row=22
foreach x of varlist $i_transform_rs {
  svy, subpop(hh_has_dj): mean `x', over(c06)
  mat b=r(table)
  putexcel N`row'=b[1,1] O`row'=b[1,2] P`row'=b[1,3] Q`row'=b[1,4] 
  local row=`row'+1
}

svy, subpop(if hh_has_dj==1): mean i_equit_dm_rs, over(c06)
  mat b=r(table)
  putexcel N34=b[1,1] O34=b[1,2] P34=b[1,3] Q34=b[1,4] 
svy, subpop(if hh_has_dj==1 & r1501==1 & r1404a==1 & r1508!="X"): mean i_gender_hhindex_rs, over(c06)
  mat b=r(table)
  putexcel N35=b[1,1] O35=b[1,2] P35=b[1,3] Q35=b[1,4] 

  
foreach x of varlist $i_transform_rs {
  svy, subpop(hh_has_dj): regress `x' i.c06
}

global transformative2 i_snet_formal i_access_market i_access_commune i_access_basic ///
					   i_access_infra i_service_ag i_service_vet i_scap_bridge i_scap_link ///
					   i_collective_act i_social_coh /*i_equit_dm i_local_dm i_gender_hhindex*/ 

local row=23
foreach x of varlist $transformative2 {
  svy, subpop(hh_has_dj): mean `x'
  mat a=r(table)
  putexcel G`row'=a[1,1]
  local row=`row'+1
}
estpost svy, subpop(hh_has_dj): tab i_snet_formal
mat n=e(obs)
putexcel H37=n[1,6]

svy, subpop(if hh_has_dj==1): mean i_equit_dm
  mat a=r(table)
  putexcel G34=a[1,1]
svy, subpop(if hh_has_dj==1 & r1501==1 & r1404a==1 & r1508!="X"): mean i_gender_hhindex
  mat a=r(table)
  putexcel G35=a[1,1]
  
svy, subpop(if hh_has_dj==1): regress i_equit_dm i.c06
svy, subpop(if hh_has_dj==1 & r1501==1 & r1404a==1 & r1508!="X"): regress i_gender_hhindex i.c06
*/
***********************************************************
//Check internal consistency and dimensionality of indices
***********************************************************
global allindicators i_snet_informal i_scap_bond i_access_savings i_access_remittance ///
				     i_assets_owned i_shock_prep i_access_ins i_access_assist ///
				     i_aspiration i_scap_bridge i_scap_link i_snet_index i_edu i_lhood ///
				     i_exposure_info /*i_ag_improved*/ /*i_assets_owned*/ i_service_fin ///
				     i_snet_formal i_access_market i_access_commune i_access_basic ///
					 i_access_infra i_service_ag i_service_vet /*i_scap_bridge i_scap_link*/ ///
					 i_collective_act i_social_coh /*i_equit_dm*/ i_local_dm i_gender_hhindex
sum   $allindicators

//Compute Cronbach's alpha to measure internal consistency/to measure scale reliability
alpha $allindicators //The alpha coefficient all items = 0.6872, suggesting that the items have
                                     //relatively high internal consistency.
alpha absorb_rs adapt_rs transform_rs  //The alpha coefficient for composite indices = 0.8015

//Check multicollinearity among the composite
collin absorb_rs adapt_rs transform_rs  //Mean VIF=1.83, Cond #=7.9072

//OVERALL INDEX
factor absorb_rs adapt_rs transform_rs, comp(3) pcf
estat kmo  // 0.6875
rotate
predict resil_index  // overall resilence index
lab var resil_index "Overall Resilience Capacity Index"
sum     resil_index 
gen resil_index_rs = ((resil_index-r(min))/(r(max)-r(min)))*100  
sum resil_index_rs
lab var resil_index_rs "Rescaled (0-100): Overall Resilience Capacity Index"

******6.6 Overall community resilience index
sum nrm_grp comm_risk soc_protect pub_goods conflict_mit
global comm_resil nrm_grp comm_risk soc_protect pub_goods conflict_mit
foreach x of varlist $comm_resil {
  replace `x'=. if vrresult!=1
  mean `x'
  egen std_`x'=std(`x')
 }

*KMO test for all 5 components is "miserable", so exploring excluding different components
global std_comm_resil std_nrm_grp std_comm_risk std_soc_protect std_pub_goods std_conflict_mit
sum $std_comm_resil
factor $std_comm_resil, pcf
estat kmo // 0.4805
predict comm_resil_index
la var comm_resil_index "Community resilience index"
sum comm_resil_index

foreach x of varlist comm_resil_index $comm_resil {         
  su `x'  
  gen `x'_rs = ((`x' - r(min))/(r(max) - r(min)))*100  
  local lbl : variable label `x'
  local new_lbl="Rescaled (0-100): "+"`lbl'"
  la var `x'_rs "`new_lbl'"  
  sum `x'_rs
}
sum comm_resil_index_rs

global std_comm_resil2 /*std_nrm_grp*/ std_comm_risk std_soc_protect std_pub_goods std_conflict_mit
factor $std_comm_resil2, pcf
estat kmo // 0.5004


*Absorptive, original and rescaled
*Adaptive, original and rescaled
*Transformative, original and rescaled 
*Composite, original and rescaled
*Community resilience, original and rescaled
keep hhea hhnum c06a ///
	 i_snet_informal i_scap_bond i_scap_bridge i_access_savings i_access_remittance ///
	 i_assets_owned i_shock_prep i_access_ins i_access_assist i_aspiration i_scap_link ///
	 i_snet_index i_edu i_lhood i_exposure_info i_service_fin i_snet_formal ///
	 i_access_market i_access_commune i_access_basic i_access_infra i_service_ag ///
	 i_service_vet i_collective_act i_social_coh i_local_dm i_gender_hhindex ///
	 i_snet_informal_rs i_scap_bond_rs i_access_savings_rs i_access_remittance_rs ///
	 i_assets_owned_rs i_shock_prep_rs i_access_ins_rs i_access_assist_rs ///
	 i_aspiration_rs i_scap_bridge_rs i_scap_link_rs i_snet_index_rs i_edu_rs i_lhood_rs ///
	 i_exposure_info_rs i_service_fin_rs i_snet_formal_rs i_access_market_rs ///
	 i_access_commune_rs i_access_basic_rs i_access_infra_rs i_service_ag_rs ///
	 i_service_vet_rs i_collective_act_rs i_social_coh_rs i_local_dm_rs ///
	 i_gender_hhindex_rs absorb absorb_rs adapt adapt_rs transform transform_rs ///
	 resil_index resil_index_rs comm_resil_index comm_resil_index_rs ///
	 nrm_grp comm_risk soc_protect pub_goods conflict_mit nrm_grp_rs ///
	 comm_risk_rs soc_protect_rs pub_goods_rs conflict_mit_rs
	 
	 
mmerge hhea hhnum using "$analytic\FTF_P2-ZOI_2023_Kenya-Survey_household_data_analytic.dta", ukeep(genhhtype_dj c06a stratum subzone wgt_hh)

tab _merge
drop if _merge == 2
drop _merge

mmerge hhea hhnum using "$analytic\FTF_P2-ZOI_Survey_Kenya_2023_household_data_analytic_resilience.dta", ukeep(shock_sev)

tab _merge
drop if _merge == 2
drop _merge

mmerge hhea hhnum using "$analytic\FTF_P2-ZOI_2023_Kenya_Survey_wealthindex_AWI.dta", ukeep(awiquint)

tab _merge
drop if _merge == 2
drop _merge


recode awiquint (5=1 "Wealthiest") ///
                (4=2 "Fourth")     ///
                (3=3 "Middle")     ///
                (2=4 "Second")     ///
                (1=5 "Poorest"), gen (awiquint_rev)	 
				
gen all_exc_samburu=1 if subzone==2|subzone==3

gen all = 1



tab subzone 
 
save "$analytic2\FTF_P2-ZOI_2023_Kenya_ML_RCI_vars.dta", replace
log close

use "$analytic2\FTF_P2-ZOI_2023_Kenya_ML_RCI_vars.dta", clear

svyset hhea [pw=wgt_hh], strata(stratum) singleunit(scaled)


svy: mean resil_index_rs  if subzone==2|subzone==3, over(genhhtype_dj) 

mat list e(b)

test c.resil_index_rs@1.genhhtype_dj=c.resil_index_rs@2.genhhtype_dj=c.resil_index_rs@3.genhhtype_dj



svy: mean resil_index_rs  if subzone==2|subzone==3, over(awiquint_rev)

mat list e(b)

test c.resil_index_rs@1.awiquint_rev=c.resil_index_rs@2.awiquint_rev=c.resil_index_rs@3.awiquint_rev==c.resil_index_rs@4.awiquint_rev==c.resil_index_rs@5.awiquint_rev


svy: mean resil_index_rs  if subzone==2|subzone==3, over(shock_sev)


mat list e(b)

test c.resil_index_rs@1.shock_sev=c.resil_index_rs@2.shock_sev=c.resil_index_rs@3.shock_sev==c.resil_index_rs@4.shock_sev


svy: mean i_scap_bond_rs  if subzone==2|subzone==3
