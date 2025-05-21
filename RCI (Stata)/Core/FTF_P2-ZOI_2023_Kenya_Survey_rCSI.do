/*******************************************************************************
************************************* MIDLINE **********************************
*************************** FEED THE FUTURE ZOI SURVEY *************************
*****************************  COPING STRATEGIES INDEX***************************
********************************* [Kenya 2023] *******************************
********************************************************************************
*This do file calculates the reduced coping strategies index (rCSI) for the 
  2023 Kenya midline ZOI/RFZ. The five components of the rCSI are as follows,
  with the corresponding weight in parentheses:
  • eating less-preferred foods (1.0)
  • borrowing food/money from friends and relatives (2.0)
  • limiting portions at mealtime (1.0)
  • limiting adult intake (3.0) 
  • reducing the number of meals per day (1.0) 

******************************************************************************
Customized for Kenya 2023 by Athena Pantazis and Jamie Parr @ICF, April 2024

This syntax file must be adapted for the final  
country-specific questionnaire. The syntax could only be partially tested using 
ZOI Survey data; therefore, double-check all results carefully and troubleshoot 
to resolve any issues identified. 
********************************************************************************/
*
set   more off
clear all
macro drop _all
set maxvar 30000
*
*/DIRECTORY PATH
global syntax	 "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Syntax\Resilience" 
global analytic2  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\Analytic" 
global source    "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\"
global source2 "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\02. Raw_Data"
global analytic  "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\03. Processed_Data\Analytic Data"
global log	     "C:\Users\jparr\OneDrive\Documents\ICF\Analysis\Kenya\KenyaDA\2. Midline\X5. Jamie\log" 

*
*

//Input data:  $source\FTF_P2-ZOI_Survey_Kenya_2023_household_data_raw.dta 
//			   $analytic\FTF_P2-ZOI_Survey_Kenya_2023_household_data_analytic.dta
//Log Output:  $analytic\Log\FTF_P2-ZOI_Survey_Kenya_2023_household.log	
//Output data: $analytic\FTF_P2-ZOI_Survey_Kenya_2023_household_data_analytic.dta
//Syntax: 	   $syntax\FTF_P2-ZOI_Survey_Kenya_2023_household_analytic.do 	

capture log close
log using "$log\FTF_P2-ZOI_Survey_Kenya_2023_rcsi.log", replace



use "$analytic\FTF_P2-ZOI_2023_Kenya-Survey_persons_data_analytic.dta", clear

collapse (sum) ncu5y=c0_59m, by(hhea hhnum) 

tab ncu5y

merge 1:1 hhea hhnum using "$analytic\FTF_P2-ZOI_2023_Kenya-Survey_household_data_analytic.dta"

keep if _merge==3
drop _merge
********************************************************************************

************************reduced COPING STRATEGIES INDEX  ***********************

tab1 v327 v328 v335 v336 v339
sum v327 v328 v335 v336 v339

* Rely on less expensive or less preferred foods? v327
tab v327, m
gen csi_1=v327*1
tab csi_1
la var csi_1 "Relied on less expensive or less preferred foods (weight=1)"

gen v327_bin = .
replace v327_bin = 1 if v327!=0 & v327!=.
replace v327_bin = 0 if v327==0

tab v327_bin 
tab v327

gen v327_time = .
replace v327_time = v327 if v327!=0 & v327!=.
replace v327_time = . if v327==0

tab v327_bin 
tab v327_time
tab v327

* Borrow food or rely on help from friends or relatives? v328
tab v328, m
gen csi_2=v328*2
tab csi_2
la var csi_2 "Borrowed food or relied on help from friends or relatives (weight=2)"


gen v328_bin = .
replace v328_bin = 1 if v328!=0 & v328!=.
replace v328_bin = 0 if v328==0

gen v328_time = .
replace v328_time = v328 if v328!=0 & v328!=.
replace v328_time = . if v328==0

tab v328_bin 
tab v328_time
tab v328


* Limit portion size at mealtimes? v335 
tab v335, m
gen csi_3=v335*1
tab csi_3
la var csi_3 "Limited portion size at mealtimes (weight=1)"

gen v335_bin = .
replace v335_bin = 1 if v335 !=0 & v335 !=.
replace v335_bin = 0 if v335 ==0

gen v335_time = .
replace v335_time = v335 if v335!=0 & v335!=.
replace v335_time = . if v335==0

tab v335_bin 
tab v335_time
tab v335


* Reduce adult consumption so children can eat? v336
tab v336 if ncu5y!=0, m
tab v336, m
gen csi_4=v336*3
tab csi_4
replace csi_4 = 0 if ncu5y==0 & subzone==2|subzone==3|subzone==4	//per RFS comment
la var csi_4 "Reduced adult consumption so children could eat (weight=3)"


gen v336_bin = .
replace v336_bin = 1 if v336 !=0 & v336 !=.
tab v336_bin
replace v336_bin = 0 if v336 ==0
tab v336_bin
replace v336_bin = 0 if ncu5y==0 & subzone==2|subzone==3|subzone==4	//per RFS comment
tab v336_bin

gen v336_time = .
replace v336_time = v336 if v336!=0 & v336!=. & subzone==2|subzone==3|subzone==4
replace v336_time = . if v336==0 & subzone==2|subzone==3|subzone==4

tab v336_bin 
tab v336_time
tab v336


* Reduce number of meals eaten per day? v339 
tab v339, m
gen csi_5=v339*1
tab csi_5
la var csi_5 "Reduced number of meals eaten per day (weight=1)"

gen v339_bin = .
replace v339_bin = 1 if v339 !=0 & v339 !=.
replace v339_bin = 0 if v339 ==0


gen v339_time = .
replace v339_time = v339 if v339!=0 & v339!=.
replace v339_time = . if v339==0

tab v336_bin 
tab v336_time
tab v336


mean csi_*

******************************************************************************
*Calculated total rcsi score for each household
egen rcsi=rowtotal(csi_*)
tab rcsi
replace rcsi=. if v327==.
sum rcsi
la var rcsi "Reduced coping strategy index"

****
// Generate rCSI caregories: (1) rCSI <4,  (2) rCSI 4-18, (3) rCSI >18
gen     rcsi_cat=.
replace rcsi_cat=1  if rcsi < 4 & rcsi!=.
replace rcsi_cat=2  if inrange(rcsi, 4,18) & rcsi!=.
replace rcsi_cat=3  if rcsi > 18  & rcsi!=.

tab  rcsi_cat, m
tab  rcsi_cat

keep hhea hhnum wgt_hh stratum genhhtype_dj subzone v327 v328 v335 v336 v339 rcsi_cat csi_* rcsi *_bin *_time

merge 1:1 hhea hhnum using "$analytic\FTF_P2-ZOI_Survey_Kenya_2023_household_data_analytic_resilience.dta", keepusing(shock_sev)

drop _merge

merge 1:1 hhea hhnum using "$analytic\FTF_P2-ZOI_2023_Kenya_Survey_wealthindex_AWI.dta", keepusing(awiquint)

drop _merge

gen all_exc_samburu=1 if subzone==2|subzone==3

gen all = 1

recode awiquint (5=1 "Wealthiest") ///
                (4=2 "Fourth")     ///
                (3=3 "Middle")     ///
                (2=4 "Second")     ///
                (1=5 "Poorest"), gen (awiquint_rev)	 
				

save "$analytic2\FTF_P2-ZOI_Survey_Kenya_2023_rcsi.dta", replace
log close


use "$analytic2\FTF_P2-ZOI_Survey_Kenya_2023_rcsi.dta", clear



svy: mean rcsi  if subzone==2|subzone==3, over(genhhtype_dj) 

mat list e(b)

* c.resil_index_rs@  c.resil_in~s@  c.resil_in~s@  c.resil_in~s@
*     1.genhhtyp_dj   2.genhhtyp~j   3.genhhtyp~j   4.genhhtyp~j

test c.rcsi@1.genhhtype_dj=c.rcsi@2.genhhtype_dj=c.rcsi@3.genhhtype_dj

svy, subpop(if subzone == 2|subzone == 3): regress rcsi i.genhhtype_dj

svy: mean rcsi  if subzone==2|subzone==3, over(awiquint_rev)

mat list e(b)

test c.rcsi@1.awiquint_rev=c.rcsi@2.awiquint_rev=c.rcsi@3.awiquint_rev==c.rcsi@4.awiquint_rev==c.rcsi@5.awiquint_rev

svy, subpop(if subzone == 2|subzone == 3): regress rcsi i.awiquint_rev


svy: mean rcsi  if subzone==2|subzone==3, over(shock_sev)


mat list e(b)

test c.rcsi@1.shock_sev=c.rcsi@2.shock_sev=c.rcsi@3.shock_sev==c.rcsi@4.shock_sev

tab v328_bin

svy: tab v336_bin if subzone==2|subzone==3


svy: tab v336 if subzone==2|subzone==3

svy, subpop(if subzone == 2|subzone == 3): regress rcsi i.shock_sev
