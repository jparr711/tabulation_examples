/*******************************************************************************
************************* FEED THE FUTURE ZOI SURVEY ***************************
***************************     RESILIENCE         ***************************
************************** [KENYA Midline, 2023]    **************************
********************************************************************************
**Description: This code is intended to calculate the component indicators that make up the
** absorptive, adaptive, and transformative indexes, as well as the index of community 
** resilience per the Center for Resilience's methodolgies, with some adaptions to align 
** with the Kenya P2-ZOI-RFZ Survey 2023 questionnaire.

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

**# MERGE IN COMMUNITY MODULE VARIABLES NEEDED

**  Merge the community variables used for the resilience capacities index  into
**  the hh analytic file - many to one on the hhea/cluster.

use "$source2\KE23MID_COMMUNITY.dta", clear 

* clonevar hhea = c02

keep hhea c_r338 cr219 cr901 cr902 cr903 cr904 cr905 c_r342 c_r343 cr309 cr313 cr314 cr315 cr306 cr307 cr308 c_r506 c_r508 cr502a  cr502b cr354 cr360 cr362 cr502c cr502e cr502g cr368 cr502a cr502b cr502o cr350 cr347 cr351 cr208 cr211 cr214 cr217 cr313 cr314 cr320 cr320a cr320c cr320d cr330 cr330a cr330e cr333 c_r359 cr801 cr801a  cr360 cr362 cr306 cr307 cr308 cr309 cr313 cr314 cr315 cr219 cr209 cr215 cr212  cr218 cr802 cr802a 

* Use raw or HH analytic file?
* merge 1:m hhea using "$source2\FTF_P2-ZOI_2023_Kenya-Survey_household_data_raw.dta"
merge 1:m hhea using "$analytic\FTF_P2-ZOI_2023_Kenya-Survey_household_data_analytic.dta"
drop _merge


//**---------------------*******-------------------*******--------------------**
*****               CREATE VARIABLES FOR INDEXES
**----------------------------------------------------------------------------**
********************************************************************************
*1. ABSORPTIVE CAPACITY INDEX
********************************************************************************
*The absorptive capacity index is constructed from 8 indicators.

/*0. Determine the line number of the HH member who responded to the resilience
     module. It is the primary female adult decisionmaker.*/
*tab r100c r100e, m  /*baseline variable names */
*
tab1 r001c r001e

tab1 r001d 

/*1. Availability of informal safety nets Community (A-WEAI module 6.4B)
     The number of types of community organizations that serve as informal
     safety nets in a community and have been active in the 12 months prior to the survey.
	 The groups include: (1) credit or micro-finance/savings, (2) mutual help (v6404_05), 
     (4) religious (_09), (5) mothers' and (6) women's (_10). (range:0-6)
	 
//* Old:The groups include: (1) credit or micro-finance, (2) savings, (3) mutual help, 
//      (4) religious, (5) mothers', and (6) women's. (range:0-6)
*Uganda  adaptation: Credit & savings combined in one response, no mothers' 	
*                   group response, no question re: group was active in past 12 months
*                   so used question re: presence of group in community, range
*                   is 0-4 rather than 0-6. */
*/

tab c06a
tab1 r801_06 r801_07 r801_12 r801_15 r801_16

tab r801_06 c06a
tab r801_07 c06a
tab r801_12 c06a
tab r801_15 c06a
tab r801_16 c06a

tab r801_06 r001d

//**credit or microfinance group
gen corg_credit=1 if r801_06==1

//**mutual help or insurance group
gen corg_help=1 if r801_07==1

//**religious group
gen   corg_rel=1 if r801_12==1

//**mothers group
gen corg_moth = 1 if r801_15==1

//**Other women's group
gen  corg_women=1 if r801_16==1 


sum corg_*

preserve
gen hholds = 1
collapse(count) hholds corg_credit corg_help corg_rel corg_moth corg_women, by(hhea)
tab1 corg_credit corg_help corg_rel corg_moth corg_women
gen corg_credit_perc = 	corg_credit/hholds
gen corg_help_perc 	 = 	corg_help/hholds
gen corg_rel_perc 	 = 	corg_rel/hholds
gen corg_moth_perc = corg_moth/hholds
gen corg_women_perc = corg_women/hholds

save "$analytic2\Temp\resilience_informal_safety_net_groups.dta", replace
restore

merge m:1 hhea using "$analytic2\Temp\resilience_informal_safety_net_groups.dta", keepusing(corg_* hhea)
sum corg_*_perc if c06a==1 & vrresult==1

gen inf_sn1 = 1 if corg_credit_perc	> 0.5
gen inf_sn2 = 1 if corg_help_perc > 0.5
gen inf_sn3 = 1 if corg_rel_perc  > 0.5
gen inf_sn4 = 1 if corg_moth_perc 	> 0.5
gen inf_sn5 = 1 if corg_women_perc > 0.5

sum inf_sn*


egen   i_snet_informal=rowtotal(inf_sn*)
sum    i_snet_informal if c06a==1
*drop   i_snet_informal
*egen   i_snet_informal=rowtotal(corg_credit corg_help corg_rel corg_moth corg_women)
*sum    i_snet_informal if c06a==1
la var i_snet_informal "Number of informal safety nets available (0-5)"



//*2. Bonding social capital index (r1304a-c r1307a-c)
********************************************************************************
** This indicator is constructed from two sub-indices: 
** (1) bonding sc: the degree of bonding among households  
**      in their own community:v361a, v361b, v361e and v361f 
** (2) bridging sc: the degree of bridging between households in the area 
**       to households outside their own community: v361c, v361d, v361g, v361h
**--------------------------------------------------------------------------**
/*2a. Bonding social capital index (v361a v361e v361b v361fc)
**--------------------------------------------------------------------------**
*  A measure of whether the HH would be able to get help from or give help to people 
*  living INSIDE their community if they needed it. That is  
* (1) Household be able to lean on relatives/non-relatives living in your community: r1304a, r1304b
* (2) Same relatives/non-relatives living in your community are able to lean on you: r1307a, r1307b
*/

/*
r1304a	Can get help w/in community from: relatives
r1304b	Can get help w/in community from: non-relatives
r1307a	Would help relatives w/in community
r1307b	Would help non-relatives w/in community
*/
*
tab1 r1304a r1304b r1307a r1307b
for var r1304a r1304b r1307a r1307b: gen Xx=1       if X==1 
for var r1304a r1304b r1307a r1307b: replace Xx=0   if X!=1 
* If No Consent and module is blank
for var r1304a r1304b r1307a r1307b: replace Xx=.   if vrresult!=1

* To capture skips
replace  r1304bx = 0  if  r1304a==2
replace  r1307bx = 0  if  r1307a==2

sum r1304?x r1307?x

* Generate the Bonding Social Capital index taking into account reciprocal relationship
* By checking whether the HH be able to lean on relatives or non-relatives 
*   and same relatives or non-relatives are able to lean on the HH 
*   INSIDE of their community. 
* 
gen     scap_bond=0
replace scap_bond=1             if (r1304ax==1 & r1304bx==1) 
replace scap_bond=scap_bond+1   if (r1307ax==1 & r1307bx==1)
replace scap_bond=.             if r1304ax==. | r1304bx==. | r1307ax==. | r1307bx==.
*
tab scap_bond if c06a==1
la var scap_bond "Bonding SC (0-2)"
clonevar i_scap_bond=scap_bond
*

//*3. Access to cash savings (r601)
*    A binary variable equal to 1 if the respondent reported that a HH member 
*     regularly saves cash.*/
tab     r601 c06a
gen     i_access_savings=0 
replace i_access_savings=1 if r601==1 
la var  i_access_savings "Has access to cash savings (0-1)"
la val  i_access_savings yes_no
tab     i_access_savings if c06a==1,m

*svy: tab r602

/*4. Access to remittances (r1105 r1106)
     A binary variable equal to 1 if the respondent reported that the HH receives 
     remittances.*/
tab   r1101 c06a
tab   r1105 c06a,m
tab   r1105 r1101,m

gen     i_access_remittance=0
replace i_access_remittance=1 if r1105==1
la var  i_access_remittance "Has access to remittances (0-1)"
la val  i_access_remittance YESNO
tab     i_access_remittance if c06a==1,m

/*5. Asset ownership index (module 2 and module R2 questions)
     Asset ownership is measured using the number of consumer durables, productive 
     assets, and livestock owned. (range: 0-49)*/
	 
*5a. Productive assets (number of items = xx)
*    Check frequencies for implausible values and fix 
*    Create new variables indicating if HH currently owns 1+ of each item
* NOTE: (1) The Midline Resilience QRE did not include R2. Productive assets were 
*    moved to Module 2: Dwelling Characteristics. (2) Hoe, Shovel, Spade, Pick axe,
*    Shear, ... were grouped into "Hand Tools" (3) Tractor, Water pumps were not added
*    (4) NOT asked: number of productive assets (was done at baseline) 
/*
Including:
v223j	HAND TOOLS
v223k	MECHANICAL PLOW
v223l	BROAD BED MAKER
v223m	KNAPSACK CHEM. SPRAYER
v223n	MODERN BEEHIVE
v223o	GRANARY
v223p	MODERN SILO
v223q	GRAIN BAGS
v240a	Own any agri. land (YN)

Should we include? Currently not in 
v223r			r) Tarpaulins?															
v223s			s) A solar pump?															
*/
*
tab1 v223j v223k v223l v223m v223n v223o v223p v223q v240a
foreach i of varlist v223j v223k v223l v223m v223n v223o v223p v223q v240a {
   gen     `i'x=0 if vrresult==1
   replace `i'x=1 if `i'==1 & `i'!=.
   la var `i'x "HH owned asset `i' at time of survey"
   la val `i'x YESNO
}
*
*Generate a count of productive assets owned
egen   prodasset_own=anycount(v223j v223k v223l v223m v223n v223o v223p v223q v240a), values(1)
replace prodasset_own=. if v236!=1
la var prodasset_own "Number of types of productive assets owned (0-9)"
tab prodasset_own if c06a==1

*5b. Livestock (7)
*    Check frequencies for implausible values and fix 
*    Create new variables indicating if HH currently owns 1+ of each animal
*NOTE: Yes/No questions for livestock NOT included in Midline.
tab1 v226a v226b v226c v226d v226e v226f v226g v226h v226i v226j v226k v226l v226m v226n v226o
tab v225
tab v225a

foreach j of varlist v226a v226b v226c v226d v226e v226f v226g v226h v226i v226j v226k v226l v226m v226n v226o {
  gen    `j'x=0 if v225!=1
  replace `j'x=1 if `j' > 0 & `j'!=.
  la var  `j'x "HH owned livestock `i' at time of survey"
  la val  `j'x YESNO
}
*
foreach j of varlist v226a v226b v226c v226d v226e v226f v226g v226h v226i v226j v226k v226l v226m v226n v226o {
   tab `j'x
}
*
*Generate a count of types of livestock owned
egen    lvstock_own=anycount(v226?x), values(1)
replace lvstock_own=. if v225!=1
replace lvstock_own=0 if lvstock_own==. & v225==1 & c06a==1
la var lvstock_own "Number of types of animals owned (0-15)"
**** SHOULD WE CHANGE THIS RANGE BASED ON NUMBER INCLUDED - currently up to 9
*** CHANGED to 15
tab lvstock_own if c06a==1,m

*5c. Durable goods owned (from Module 2, not resilience module)
* v222? 
/*
v222a			a) Electricity?															
v222b			b) Biogas?															
v222c			c) Solar?															
v222d			d) Diesel generator?															
v222e			e) A radio?															
v222f			f) A television?															
v222g			g) A refrigerator?															
v222h			h) A computer/laptop?															
v222i			i) A table?															
v222k			k) A chair?															
v222l			l) A sofa?															
v222m			m) A bed?															
v222n			n) A cupboard?															

*/
sum v222? v223a v223b v223c v223d v223e v223f v223g v223h v223i v223j
egen    durable_own=anycount(v222? v223a v223b v223c v223d v223e v223f v223g v223h v223i v223j), values(1)
replace durable_own=. if v236!=1
la var durable_own "Number of types of durable goods owned (0-23)"
*** SHOULD WE CHANGE THIS RANGE BASED ON NUMBER INCLUDED
tab durable_own if c06a==1

*5d. Add all assets together
gen i_assets_owned=lvstock_own + prodasset_own + durable_own
la var i_assets_owned "Asset ownership index (0-47)"
replace i_assets_owned=0 if i_assets_owned==. & c06a==1 & vrresult==1 
tab i_assets_owned if c06a==1
*
sum lvstock_own prodasset_own durable_own i_assets_owned if c06a==1

/*6. Shock preparedness and mitigation  (r908, r126-r137, v6404a_k, cr354)
      Summary variable of 4 categories (range:0-4)*/

*6a.NGO/government disaster planning/response program
*   Captured the presence of a distaster planning program in sub-module R8 
*   so to apply to community level, said a program was present in the cluster if
*   at least 3 HHs said one was available.

//* Note: The ML Questionnaire add the 'disaster planning/response group' question 
*        in the Resilience module. However, for comparison with BL 'shock_prep1'
*        is removed from analysis.


		 
gen     disaster_group=.
replace disaster_group=1 if r801_10==1 

preserve
collapse (count) disaster_group, by(hhea)
tab disaster_group
save "$analytic2\Temp\resilience_disaster_group.dta", replace
restore

drop disaster_group
drop _merge
merge m:1 hhea using "$analytic2\Temp\resilience_disaster_group.dta", keepusing(disaster_group)
drop _merge
tab disaster_group if c06a==1
mean disaster_group if c06a==1
gen      shock_prep1=1 if disaster_group>=3 & disaster_group!=.
replace  shock_prep1=0 if disaster_group< 3 & disaster_group!=.

*gen shock_prep1=v6404a_11==1 | m6404a_11==1
tab shock_prep1


*6b.Community livestock take-off planning
tab cr354
gen     shock_prep2=1 if cr354==1
replace shock_prep2=0 if shock_prep2==. & cr001k==1
*Note all communities completed for Kenya ML


*6c.HH participation in community activities 
*   (i.e., soil conservation activities, flood diversion structures, planting  
*    trees on communal land, or improving access to health services)
*   For Kenya: repair/build health center; plant trees; conserve soil; divert flood

egen shock_prep3=anymatch(r908b r908d r908f r908g), values(1)         

*6d.HH preparation //* Issues with var list: Some not in MIDLINE *//

*   (i.e., increasing savings, putting aside grains/fodder, switching to different 
*    crops/livestock, added ag activity to non-ag activity, added non-ag activity 
*    to ag activity, acquiring crop insurance)
/* In Midline QRE
  r127   Save cash for difficult times YN, 12 mo
  r128   Increased food stores for difficult times
  r129   Implemented new ag activities YN, 12 mo
  r130   Started non-ag work YN, 12 mo
  r131a  Crop insurance for difficult time, 12 mo
  r133   Relocated temp/permanently YN, 12 mo
  */

  /* In Midline QRE - similar questions but not included
  r131b  Acquired Livestock Insurance to protect from diff times YN, 12 mo
  r131c  Acquired Health Insurance to protect from diff times  YN, 12 mo 
  r131d  Acquired other insurance to protect from diff times  YN, 12 mo
  r131e  Acquired (second) other insurance to protect from diff times  YN, 12 mo
  r131a  Crop insurance for difficult time, 12 mo
  */
 
tab r127 c06a

tab r128 c06a

tab r129 c06a

tab r130 c06a

tab r133 c06a

 

*egen shock_prep4=anymatch(r127 r128 r130 r133 r135 r136 r137 r138 r139 r140 r142), values(1) /* BL */

egen shock_prep4=anymatch(r127 r128 r129 r130 r133), values(1) 

sum shock_prep?

egen i_shock_prep=rowtotal(shock_prep?)
la var i_shock_prep "Shock preparedness and mitigation (0-3)"
tab  i_shock_prep if c06a==1,m

assert i_shock_prep<=4

/*7. Access to insurance  (r505-r511)
     A binary variable equal to 1 if any type of insurance is available in the 
     respondent’s village OR the respondent’s HH reports having any type of insurance.*/
	 
	 
tab1 r505 r507 r509 r511 c_r506 c_r508	 

* crop insurance
tab r505 c06a

tab r505 v233 if c06a==1

* livestock insurance
tab r507 c06a

tab r507 v225 if c06a==1
tab r507 v225a if c06a==1
* FILTER ISSUE on r507 with v225, v225a confirms skip logic worked.

* health insurance
tab r509 c06a

* other insurance
tab r511 c06a


egen i_access_ins= anymatch(r505 r507 r509 r511 c_r506 c_r508), values(1)
la var i_access_ins "Has access to insurance (0-1)"
la val i_access_ins YESNO
tab i_access_ins if c06a==1,m


/*8. Access to humanitarian assistance (r302a r304a r302b r304b)
     A binary variable equal to 1 if govt or NGO emergency food or cash assistance 
     is available in the respondent’s community OR the HH received emergency food 
	 or cash assistance from the govt or NGO during the 12 months prior to the survey.*/

	 
	 


gen i_access_assist=0
replace i_access_assist=1 if r304a==1 | r304b==1 | cr502a==1 | cr502b==1
la var i_access_assist "Has access to humanitarian assistance (0-1)"
la val i_access_assist YESNO
tab i_access_assist if c06a==1,m

********************************************************************************
*2. ADAPTIVE CAPACITY INDEX 
********************************************************************************
*The adaptive capacity index is constructed from ten indicators.

/*4.1 Aspirations/confidence to adapt/locus of control index (r1401-r1405 r1407-r1416) 
      This index is based on indicators of the underlying concepts around people’s 
	  aspirations, confidence to adapt, and a sense of control over one’s life. 
	  (range:0-16)*/

**a. Aspiration: Absence of fatalism (r1401 r1402 r1413 r1413a)
*    The aspirations index is based on two sub-indices: (a) absence of fatalism,
*    and (b) belief in the future. 
*	r1401. Each person is responsible for his/her own success or failure in life. 
*	r1402. To be successful one needs to work very hard rather than rely on luck. 
*   r1413a. My experience in life has been that what is going to happen will happen. (reverse-code)
*   r1413.  It is not always good for me to plan too far ahead because many things 
*       turn out to be a matter of good or bad fortune.
tab1 r1401 r1402 r1413a r1413
gen fatalism1=r1401==1
gen fatalism2=r1402==1
gen fatalism3=inlist(r1413a,1,2,3) //reverse coded
gen fatalism4=inlist(r1413,4,5,6)
tab1 fatalism*

**b. Aspiration: Belief in future (r1404 r1405)
*    The belief in the future sub-index is based on two binary (dummy) variables 
*    equal to 1 regarding the respondent’s view of the future.
*    1. They are hopeful for their children’s future.
*    2. They want their children to graduate from secondary or post-secondary school.
tab1 r1404 r1405
gen future1=r1404==1
gen future2=r1405==5 | r1405==6
tab1 future*

**c. Confidence to adapt (r1403 r1407-r1411)
*    The sub-index consists of six variables regarding the degree to which the 
*     respondent is exposed to alternatives
*     1. Is willing to move somewhere else to improve his/her life.
*     2. Communicates regularly with at least one person outside of the village.
*     3. Engaged in any economic activities with members of other villages or clans 
*        during the week prior to the survey.
*     4. Got together with people to have food or drinks, either in their home or 
*        in a public place 1+ time in past month.
*     5. Attended a church/mosque/other religious service 1+ time in past month.
*     6. Stayed more than two days outside of their community/village 1+ time in past month
tab1 r1403 r1407 r1408
gen confidence1=r1403==1
gen confidence2=r1407==1
gen confidence3=r1408==1

*Check r1409, r1410, and r1411 for implaubible values. Set implausible values to missing
*Kenya ML 4 implausible values for r1409 (>31)
**# Kenya ML r1411 has 23 values over 31, but is asked for full year. All under 100
tab1 r1409 r1410 r1411
tab r1409 
tab r1410
tab r1411 
gen confidence4=1 if r1409>0 & r1409<31
gen confidence5=1 if r1410>0 & r1410<. 
gen confidence6=1 if r1411>0 & r1411<. 
for var confidence4 confidence5 confidence6: recode X .=0
tab1 confidence*

**d. Locus of control (r1414-r1416 r1413b)
*    This indicator is constructed from 4 questions. For each question, a binary 
*    variable is calculated equal to 1 if the respondent reports they “strongly 
*    agree”, “agree”, or “slightly agree” with the question.
*    1. My life is chiefly controlled by other powerful people. (reverse-code)
*    2. I can mostly determine what will happen in my life.
*    3. When I get what I want, it is usually because I worked hard for it.
*    4. My life is determined by my own actions.
tab1 r1413b r1414 r1415 r1416
gen control1=inlist(r1413b,1,2,3) //reverse coded
gen control2=inlist(r1414,4,5,6)
gen control3=inlist(r1415,4,5,6)
gen control4=inlist(r1416,4,5,6)
tab1 control*

*Calculate the aspirations/confidence index by adding all variables created
egen i_aspiration=rowtotal(fatalism? future? confidence? control?)
la var i_aspiration "Aspirations/confidence index (0-16)"
tab i_aspiration if c06a==1,m 
sum i_aspiration if c06a==1
** -------------------------------------------------------------------
/*4.2. Bridging social capital (r1305a r1305b r1308a r1308b)
** -------------------------------------------------------------------
* This index is based on whether: 
* (1) Household be able to lean on relatives/non-relatives living OUTSIDE the community: r1305a, r1308a
* (2) Same relatives/non-relatives living OUTSIDE your community are able to lean on you: r1305b, r1308b
*/
* 
/*
r1305a	Can get help outside community from: relatives
r1305b	Can get help outside community from: non-relatives
r1308a	Would help relatives outside community
r1308b	Would help non-relatives outside community
*/
*
tab1 r1305a r1305b r1308a r1308b
*
for var r1305a r1305b r1308a r1308b: gen Xx=1   if X==1 
for var r1305a r1305b r1308a r1308b: replace Xx=0   if X!=1 
* If No Consent and module is blank
for var r1305a r1305b r1308a r1308b: replace Xx=.   if vrresult!=1

* To capture skips
replace r1305bx = 0  if  r1305a==2
replace r1308bx = 0  if  r1308a==2

sum  r1305?x r1308?x

** Generate the Bridging Social Capital index taking into account reciprocal relationship
* By checking whether the HH be able to lean on relatives or non-relatives 
*   and same relatives or non-relatives are able to lean on the HH 
*   OUTSIDE of their community. 
*
gen     scap_bridge=0
replace scap_bridge=1               if (r1305ax==1 & r1305bx==1) 
replace scap_bridge=scap_bridge+1   if (r1308ax==1 & r1308bx==1)
replace scap_bridge=.               if r1305ax==. | r1305bx==. | r1308ax==. | r1308bx==.
*
tab    scap_bridge if c06a==1
sum	   scap_bridge if c06a==1
la var scap_bridge "Bridging SC (0-2)"
clonevar i_scap_bridge=scap_bridge


/*4.2 Bridging social capital (r1305a-r1305c r1308a-r1308c)
This index is based on whether the HH would be able to get help from or give help
to people living OUTSIDE OF their community if they needed it. (range:0-4)*/
tab1 r1305a r1305b r1308a r1308b

/*sum r1305a r1305b r1305c r1308a r1308b r1308c 
for var r1305a r1305b r1305c r1308a r1308b r1308c: gen Xx=1 if X==1 
sum r1305?x r1308?x

gen i_scap_bridge=0
replace i_scap_bridge=i_scap_bridge+1 if r1305ax==1 
replace i_scap_bridge=i_scap_bridge+1 if r1305ax==1 & r1308ax==1
replace i_scap_bridge=i_scap_bridge+1 if (r1305bx==1 | r1305cx==1) 
replace i_scap_bridge=i_scap_bridge+1 if (r1305bx==1 | r1305cx==1) & (r1308bx==1 | r1308cx==1) 
replace i_scap_bridge=. if r1305a==. & r1305b==. & r1305c==. & r1308a==. & r1308b==. & r1308c==. 
la var i_scap_bridge "Bridging social capital index (0-4)"
tab i_scap_bridge,m
egen i_scap_bridge2=rowtotal(r1305ax r1305bx r1305cx r1308ax r1308bx r1308cx)
replace i_scap_bridge2=. if r1305a==. & r1305b==. & r1305c==. & r1308a==. & r1308b==. & r1308c==. 

gen i_scap_bridge3=0
replace i_scap_bridge3=i_scap_bridge3+1 if r1305ax==1 & r1308ax==1
replace i_scap_bridge3=i_scap_bridge3+1 if (r1305bx==1 | r1305cx==1) & (r1308bx==1 | r1308cx==1) 
replace i_scap_bridge3=. if r1305a==. & r1305b==. & r1305c==. & r1308a==. & r1308b==. & r1308c==. 

sum i_scap_bridge*


rename scap_bridge_orig i_scap_bridge
*/
*
/*4.3 Linking social capital (r1309-r1314)
This index is based on whether HH members know a government official and/or NGO 
leader and whether they believe the official/leader would help their family or 
community if help was needed. (range:0-4)*/
/*
r1321 HH personally knows an elected gov official
r1323 HH could ask gov official for help YN
r1324 HH personally knows a NGO worker YN
r1326 HH could ask NGO worker to help YN
*/
*
sum r1321 r1323 r1324 r1326
tab1 r1321 r1323 r1324 r1326
for var r1321 r1323 r1324 r1326: gen Xx=1 if X==1
sum r1321x r1323x r1324x r1326x

egen   i_scap_link=rowtotal(r1321x r1323x r1324x r1326x)
la var i_scap_link "Linking social capital index (0-4)"
tab    i_scap_link if c06a==1
sum	   i_scap_link if c06a==1

/*4.4 Social network index (v6404_04 v6404_05 v6404_10 r902-r904)
      An index based on 6 variables: (1) there is a savings group in the village;
      (2) there is a mutual help group in the village; (3) there is a women’s 
	  group in the village; and the proportion of HHs in the village that report 
	  any HH member participated in a group that provided (4) food, (5) labor, 
	  (6) other type of support to someone in that village 1+ time in the last 
	  12 months (range:0-6)*/
*4.4a savings group in community (credit or micro-finance)
tab r801_06
gen soc_net1=r801_06==1

*4.4b mutual help group in community
tab r801_07
gen soc_net2=r801_07==1 

*4.4c women's group in community
tab r801_16
tab r801_10
gen soc_net3=r801_16==1

*4.4d HH partook in group that provided labor
tab r901
tab r902
gen soc_net4yes=inlist(r902,2,3,4)

tab r901 r902

*4.4e HH partook in group that provided food
tab r903
gen soc_net5yes=inlist(r903,2,3,4) 

tab r901 r903
*4.4f HH partook in group that provided other help
tab r904 r901
gen soc_net6yes=inlist(r904,2,3,4) 

preserve
gen hholds=1
replace soc_net4yes=. if soc_net4yes!=1
replace soc_net5yes=. if soc_net5yes!=1
replace soc_net6yes=. if soc_net6yes!=1
drop if vrresult!=1
collapse (count) hholds soc_net4yes soc_net5yes soc_net6yes, by(hhea)

gen soc_net4perc=soc_net4yes/hholds
gen soc_net5perc=soc_net5yes/hholds
gen soc_net6perc=soc_net6yes/hholds
save "$analytic2\Temp\temp1.dta", replace
*save "$analytic\Temp\resilience_snet_index.dta", replace
restore
merge m:1 hhea using "$analytic2\Temp\temp1.dta"
*merge m:1 hhea using "$analytic\Temp\resilience_snet_index.dta"
drop _merge
*
rename soc_net4perc soc_net4
rename soc_net5perc soc_net5
rename soc_net6perc soc_net6
drop soc_net4yes soc_net5yes soc_net6yes

sum soc_net*

egen i_snet_index=rowtotal(soc_net*)
tab i_snet_index if c06a==1
sum i_snet_index if c06a==1
la var i_snet_index "Social network index (0-6)"


/*4.5 Education/training (r1327-r1333 nedu_prim_dj)
	  An index calculated from three variables that indicate: (1) 1+ HH member  
	  can read and write, (2) 1+ HH member completed primary school, and (3) 1+  
	  HH received at least 1 of 6 types of training (adult education, job/skills 
	  training, business development, early warning for natural disasters,  
	  natural resource management, mobile phone use to get info about market 
	  prices or weather).(range:0-3)*/
*
tab1 r1327-r1333 /* nedu_prim_dj */
*4.5a Any HH member can read and write
gen  edu1=r1327==1
**# Pull in person's analytic data file and calculate nedu_prim_dj
*4.5b Any de jure HH member completed primary school 
save "$analytic2\Temp\Kenya_2023-RCI_temp.dta", replace
use "$analytic\FTF_P2-ZOI_2023_Kenya-Survey_persons_data_analytic.dta", clear
tab edu_prim
gen edp = 1 if edu_prim==1
tab edp
collapse(count) edp, by(hhea hhnum hhmem_dj)

tab edp
tab hhmem_dj
drop if hhmem_dj==0
drop hhmem_dj
save "$analytic2\Temp\edu_prim_temp.dta", replace

use "$analytic2\Temp\Kenya_2023-RCI_temp.dta", clear

merge 1:1 hhea hhnum using "$analytic2\Temp\edu_prim_temp.dta"


drop if _merge==2

drop _merge
tab edp
gen edu2=1 if edp>0 & edp!=.
tab edu2
*4.5c Any HH member received training
egen edu3=anymatch(r1328 r1329 r1330 r1331 r1332 r1333),v(1)
tab1 edu1-edu3

egen i_edu=rowtotal(edu1 edu2 edu3)
la var i_edu "Education/training (0-3)"
tab  i_edu if c06a==1,m

/*4.6 Livelihood diversification (r1001_01-r1001_18)
	The total number of livelihood activities engaged in over the last year. The activities could have resulted in food or income for the HH.

r1001_01 - The household's own production or sale of crops?																r1001_02 - The household's own livestock production, fattening, or sales?															r1001_03 - Running your own agricultural-based business, such as providing threshing service to local farmers, aggregating and selling crop byproducts, processing and selling groundnuts, or fish mongering?														r1001_04 - Agricultural wage labor (non-salaried)?													r1001_05 - Non-agricultural wage labor (non-salaried)?													r1001_06 - Salaried work?															r1001_07 - Petty trade that involved selling items produced at home (eg. livestock byproducts, woven goods, homebrew, etc.) or sex work?															r1001_08 - Petty trade that involved selling item produced by other people, such as reselling grains, vegetables, oil, sugar, mobile phone minutes, or cigarettes?	
r1001_09 - Running your own non-agricultural-based business, such as stone cutting or hair braiding?														r1001_10 - Hunting or fishing?														
r1001_11 - The sale of other wild/ bush products, such as charcoal, firewood, wild plants, or wild meats?															r1001_12 - Honey production or sales?															r1001_13 - Remittances?													r1001_14 - Gifts or inheritance?													r1001_15 - Government programs, such as safety net food or cash assistance?														r1001_16 - Rental of land, house, or rooms?															r1001_17 - Any other source? (Specify)															r1001_18 - Any other source? (Specify)															r1001_19 - Any other source? (Specify)																	
	
	
	(range:0-19)*/ 
**
sum r1001_01-r1001_19
tab r1001_19, m
tab r1001_18, m
tab r1001_17
tab r1001_01
for var r1001_01-r1001_19: gen Xx=1 if X==1

sum r1001_01x-r1001_19x
egen i_lhood=rowtotal(r1001_01x-r1001_19x)
tab i_lhood
la var i_lhood "Livelihood diversification (# activities) (0-19)"
*Set any HHs with 0 income/food sources to 1...Must have at least one source
replace i_lhood=1 if i_lhood==0 & vrresult==1
replace i_lhood=. if i_lhood==0 & vrresult!=1

tab i_lhood if c06a==1,m

/*4.7 Exposure to information (sub-module R7 questions)
      The number of topics the respondent has received information on in the 
	  last year. (range:0-19)
Receive information on:
r701 - Early warning systems for natural hazards
r709 - Rainfall
r713 - Water prices or availability
r717 - Conflict or security issues
r729 - Market prices of food HH bought
r733 - Child health and nutrition
r737 - Equal rights for women and men or violence against women
r745 - NRM
r749 - Animal/livestock health
r753 - Crop health
r769 - Market prices for live animals or animal products
	  
	  
	  */

global info r701 r709 r713 r717 r729 r733 r737 r745 r749 r753 r769
sum $info
tab1 $info
for var $info: gen Xx=1 if X==1 
sum     r7*x
egen    i_exposure_info=rowtotal(r7*x)
la var  i_exposure_info "Exposure to info, number of topics (0-11)"
tab     i_exposure_info if c06a==1,m
sum     i_exposure_info if c06a==1
drop    r7*x

/*4.8 Adoption of improved practices (ag module questions)
      This binary variable is equal to 1 if respondent adopted 3+ improved practices 
	  for crop production, OR adopted 3+ improved practices for livestock production,
	  OR adopted 1+ natural resource management practice or technique not related 
	  directly to on-farm production, OR used any improved storage method.*/
	  
* Uganda Adoption of improved practices (ag module questions): Not included.

/*4.9 Asset ownership index 
	  (see absorptive index #5 above)*/

//*Note: Did not find questions about financial resources in the Kenya ML questionnaire

/*4.10 Availability of financial resources (cr360 cr362)
	   This variable is equal to 0 if there is no institution in a community that 
	   provides credit or savings support, 1 if there is only one type of support, 
	   and 2 if there are both types of support. (range:0-2)*/
tab1 cr360 cr362
gen comm_credit=1 if cr360==1 
gen comm_savings=1 if cr362==1 

egen i_service_fin=rowtotal(comm_credit comm_savings)

la def FIN 0 "None"    ///
           1 "Credit or savings" ///
		   2 "Both" 
la val i_service_fin FIN 
la var i_service_fin "Availability of financial resources (0-2)"
tab i_service_fin if c06a==1
sum i_service_fin if c06a==1

drop comm_credit comm_savings


********************************************************************************
*3. TRANSFORMATIVE CAPACITY INDEX 
********************************************************************************
*The transformative capacity index is constructed from 15 indicators.

tab1 r1??d r12?

/*1. Availability of/access to formal safety nets (r302d r302f r302g r355 r302a r302b r303)
	 This index indicates whether there are places in a village where people can 
	 get (1) food assistance, (2) housing materials and other non-food items, 
	 (3) assistance due to losses in livestock, and (4) government or NGO help when they are faced with a shock, plus (5) the availability of a government 
	 or NGO disaster response program, and (6) whether the HH received assistance 
	 
cr502c - CCT - should we include this?
cr502e - Non-emergency unconditional cash transfer
cr502g - HH materials
cr368  - Program or Places for Assistance due to Livestock Losses
cr502a - Emergency food assistance 
cr502b - Emergency cash assistance
cr502o - Disaster Planning response support	 

Should we include????
cr502l - Livestock inputs
	 (range:0-6)*/
*Uganda adaptation: Scale:0-5 because no question about government or NGO disaster response program.
* Kenya - asks about disaster response program support.
tab1 cr502c cr502e cr502g cr368 cr502a cr502b cr502o
sum cr502c cr502e cr502g cr368 cr502a cr502b cr502o
*1.1 Non-emergency food assistance
gen fsn1=cr502c==1 | cr502e==1 

*1.2 Housing materials
gen fsn2=cr502g==1 

*1.3 Assistance for livestock loss IN community
gen fsn3=cr368==1

*1.4 Govt or NGO help available when they are faced with a shock 
*Uganda & Kenya ML adaptation: Did not ask this question explicitly so adjusting a bit. (i.e., emergency food/cash)
gen fsn4=cr502a==1 | cr502b==1 

*1.5 Govt or NGO disaster response/planning program (Captured in A-WEAI module)
*  Uganda adaptation: Omitted because not included in QRE.
 gen fsn5=cr502o==1 

*1.6 HH received assistance from the government or NGO
*Kenya ML no r303, just asked about the types of formal support used
tab1 r304a r304b r304c r304e r304f r304h r304i r304j r304k r304m r304n r304o r304x

egen fsn6=anymatch(r304a r304b r304c r304e r304f r304h r304i r304j r304k r304m r304n r304o r304x), v(1)

//* # of formal safety nets availabile in the village

tab1 fsn*
egen    i_snet_formal=rowtotal(fsn1-fsn6)
la var  i_snet_formal "# of formal safety nets availabile in the village (0-6)"
tab     i_snet_formal if c06a==1,m

sum     i_snet_formal if c06a==1

/*2. Availability of markets (r346 r347 r348)
	 This index indicates whether there is (1) 1+ market for selling ag products, 
	 (2) 1+ market for purchasing ag inputs, and (3) 1+ livestock market within 
	 5km of community. (range:0-3)*/
tab1 cr350 cr347 cr351
for var cr350 cr347 cr351: gen Xx=1 if X==1
sum cr350x cr347x cr351x 
egen i_access_market=rowtotal(cr350x cr347x cr351x)
la var i_access_market "# of markets available within 5 km of the village (0-3)"
tab i_access_market,m

/*3. Availability of/access to communal natural resources (r307 r309 r311 r313)
	 This index indicates whether the following communal resources are available 
	 in the community: (1) grazing land, (2) water source for livestock, 
	 (3) firewood source, and (4) irrigation water source. (range 0-4)*/
tab1  cr208 cr211 cr214 cr217
for var cr208 cr211 cr214 cr217: gen Xx=1 if X==1
sum     cr208x cr211x cr214x cr217x
egen    i_access_commune=rowtotal(cr208x cr211x cr214x cr217x)
la var  i_access_commune "# communal natural resources available in community (0-4)"
tab     i_access_commune if c06a==1,m
sum 	i_access_commune if c06a==1

/*4. Availability of/access to basic services (r325 r326 r329-r332 r333-r336 r359-r361 r349 r350 r352 r353)
	 This index indicates the number of basic services available in a community and whether certain services generally provided by the government are of a minimum  quality of service. (range 0-5)



*/

**4a. Roads
*	  This binary variable indicates if community's main route is a paved, dirt, 
*	  or mixed paved/dirt road, AND people are not prevented from traveling at certain times of year due to “poor road/trail conditions”.
tab1 cr313 cr314 
gen acc_road=1 if cr313<=3 & cr314==2
tab acc_road

**4b. Primary schools
*     This binary variable indicates that there is a primary school <5 km of the 
*	  community AND its physical condition is “good” AND there are enough teachers.
tab1 cr320 cr320a cr320c cr320d
gen acc_school=1 if (cr320==1 | cr320a==1) & (cr320c==1 | cr320c==2) & cr320d==1
tab acc_school

**4c. Health services (post, clinic, center)
*	  This binary variable indicates if there are health services <5 km of the 
*	  community AND its physical condition is “good” AND there were no problems 
*     accessing services over the last year".
tab1 cr330 cr330a cr330e cr333
gen acc_health=1 if (cr330==1 | cr330a==1) & (cr330e==1 | cr330e==2) & cr333==2
tab acc_health

**4d. Police/security force
*	  This binary variable indicates if there are government security forces (local or 
*	  national) that can reach the village within one hour.
tab1 c_r359 cr801 cr801a
gen acc_security=1 if c_r359==1 & inlist(cr801a,1,2)
replace acc_security = . if cr801=="E" | cr801=="EX"
tab acc_security

**4e. Financial services
*	  This binary variable indicates if there are formal institutions (i.e., govt 
*	  regulated banks) in the commnuity where people can borrow or save money.
tab1 cr360 cr362 
gen acc_finance=1  if cr360==1 | cr362==1
tab1 acc_finance

sum acc_*
egen i_access_basic=rowtotal(acc_*)
la var i_access_basic "# of good quality basic services available in the village (0-5)"
tab i_access_basic if c06a==1
sum i_access_basic if c06a==1

/*5. Availability of/access to infrastructure (v211 v222b r701 r321 r323 r325 r326 r327)
	 This index indicates the number of types of infrastructure available in a community, 
	 as determined by the following conditions: (1) 50+% HHs in the village have access 
	 to piped water, (2) 50+% HHs in the village have electricity from the main grid,
	 (3) the village has either mobile phone service/network coverage OR a public 
	 telephone/kiosk, and (4) the village can be reached with a paved road all year OR 
	 is served by a public transportation system. (range 0-4)*/
tab1 v211 v222a r701

*Determine the percent of HHs in each cluster who have piped water, electricity (and get shock info)
sort hhea hhnum
gen piped_h2o=1 if inlist(v211,11,12,13,14)
gen elec_grid=1 if v222a==1 
*shock_info added for later index
gen  shock_info=1 if r701==1
*
tab1 piped_h2o elec_grid shock_info
*
preserve
collapse (count) hhnum piped_h2o elec_grid shock_info, by(hhea)
gen half_piped=piped_h2o/hhnum
gen half_elec=elec_grid/hhnum
gen prop_shock=shock_info/hhnum
tab1 half_* prop_*
save "$analytic2\Temp\temp1.dta", replace
*save "$analytic\Temp\Kenya_P2-ZOI_Survey_comm_piped_water_electricity.dta", replace
restore

*merge m:1 hhea using "$analytic\Temp\Kenya_P2-ZOI_Survey_comm_piped_water_electricity.dta"
merge m:1 hhea using "$analytic2\Temp\temp1.dta"

drop _merge

*5.1 50+% HHs in the village have access to piped water
gen infra_pipedh2o=1 if half_piped >=0.5

*5.2 50+% HHs in the village have electricity from the main grid
*    (HHs that have electricity in a cluster with the main source=main grid)
gen infra_elec=1 if half_elec >=0.5 

tab1 cr306 cr307 cr308
gen infra_elec_alt = 1 if cr306==1 & inlist(cr307,1,2,3) & cr308==1

tab infra_elec infra_elec_alt
*no difference - 425 HH with electricity from main grid

tab1 cr309 cr313 cr314 cr315

*5.3 Community has mobile phone service/network coverage OR a public telephone/kiosk
gen infra_phone=1 if cr309==1

*5.4 Community can be reached with a paved road all year OR is served by a public 
*    transportation system
gen infra_roads=1 if (cr313==1 & cr314==2) | (cr315==1 | cr315a==1)

tab1 infra_*
egen i_access_infra=rowtotal(infra_*)
tab i_access_infra
la var i_access_infra "Has access to infrastructure (0-4)"
tab i_access_infra

/*6. Availability of/access to ag extension services (r338-r341)
	 This variable indicates whether ag extensions services are available in a village 
	 and are of a minimum quality of service. (range:0-2)*/
** 
tab1 c_r338 r340 r341
gen     i_service_ag=0 
replace i_service_ag=1 if c_r338==1 & r340==1
replace i_service_ag=2 if c_r338==1 & (r340==2 | r340==8)

la def  AG  0 "No ag ext services in community" ///
			1 "Ag ext services in community but not available when needed" ///
			2 "Ag ext services in community and available when needed" 
la val  i_service_ag AG			
la var  i_service_ag "Availability of ag extension services in the village (0-2)" 
tab     i_service_ag if c06a==1
sum     i_service_ag if c06a==1
/*7. Availability of/access to livestock services (r342-r344)
	 This variable indicates whether veterinary services are available in a  
	 village and are of a minimum quality of service. (range:0-2)*/
tab1 c_r342 c_r343 r344
gen     i_service_vet=0 
replace i_service_vet=1 if (c_r342==1 | c_r343==1) & r344==1
replace i_service_vet=2 if (c_r342==1 | c_r343==1) & (r344==2 | r344==8)
la def  VET 0 "No veterinary services <5 km" ///
			1 "Veterinary services <5 km but not available when needed" ///
			2 "Veterinary services <5 km and available when needed", modify
la val  i_service_vet VET			
la var  i_service_vet "Availability of livestock veterinary services in the village (0-2)" 
tab     i_service_vet if c06a==1
sum     i_service_vet if c06a==1

/*8. Bridging social capital 
	 (see adaptive capacity index #2 above)*/

/*9. Linking social capital 
	 (see adaptive capacity index #3 above)*/

/*10.Collective action (r907 r908a-r908j)
	 A summary variable based on the number of types of collective action a HH engaged 
	 in over the last 12 months to benefit the entire community. (range:0-10)*/
sum r907 r908a-r908j
tab1 r907 r908a-r908j
for var r908a-r908j: gen Xx=1 if X==1
sum     r908ax-r908jx
egen    i_collective_act=rowtotal(r908ax-r908jx)
la var  i_collective_act "# of types of collective action HH engaged in, past 1 year (0-10)"
tab     i_collective_act if c06a==1
sum     i_collective_act if c06a==1
drop    r908ax-r908jx

/*11.Social cohesion (r902-r904 r315)
	 This index indicates whether groups come together either socially or to help 
	 others. Each binary variable indicates if a group of community members came 
	 together at least once during the 12 months prior to the survey in order to: 
	 (1-3) provide labor, food, or other help to someone else in the village who 
	 needed it, (4-5) get together with other members of the village or with  
	 members of other villages for social events (e.g., sports events, celebrations) 
	 (range:0-5)*/
*Uganda adaptation: Scoring modified to be on a scale or 0-4 because Uganda QRE  
*	  does not differentiate between people in vs. outside community.
* Kenya includes this for social gatherings - cr219 and cr220
tab1 r902-r904 cr219 cr220
for var r902-r904: gen Xx=1 if inlist(X,2,3,4)
for var cr219 cr220: gen Xx=1 if X==1
tab1 cr219x cr220x
egen i_social_coh=rowtotal(r902x r903x r904x cr219x cr220x)
la var i_social_coh "Index of social cohesion (0-5)"
tab i_social_coh if c06a==1
sum i_social_coh if c06a==1

/*
/*12.Gender equitable decision-making index (v6202_07 v6202_09 v6202_10 v6202_11 v6202_12)
	 This index uses a 5-point scale to evaluate four areas of decision-making 
	 question about social interactions control: (1) income, (2) health and 
	 nutrition, (3) household purchases, and (4) children’s education (range:0-12)*/
*Uganda adaptation: Scoring modified to be on a scale of 0-2: 0-did not participate, 
*     				1-made decision jointly, 2-made decision alone (range:0-8)
*                   Captured in A-WEAI module
/*
_05  6.2_5  Wage and salary employment
_07  6.2_7  Major household expenditures such as a large appliance for the house like refrigerator
_08  6.2_8  Minor household expenditures such as food for daily consumption or other household needs

Kenya ML does not have: 
		Your children's education, including educational expenses
		Seeking medical treatment for your children
		Also, used questions on any wages for income because questions about income were not available in Kenya ML
REMOVE - DO NOT CALCULATE
*/


/*13. Local government responsiveness (cr804a-cr804h cr805a-cr805h)
	  This index indicates whether, and how, the local government responded to  
	  community requests to improve community assets or services in the 5 years  
	  before the survey. The requests include: (1) roads, (2) schools, (3) health 
	  centers, (4) piped water, boreholes, or wells, (5) natural resource conservation, 
	  (6) irrigation systems, (7) public transportation, and (8) security. (mean range:0-6)
	  
*Uganda/Kenya adaptation: DROPPED FROM INDEX - Not included in QRE - cannot calculate
			for var cr805a-cr805h: tab X,m
			for var cr805a-cr805h: gen xX=(7-X)   
			for var cr805a-cr805h: tab xX,m
			for var cr805a-cr805h: egen sumcr805=rowtotal(xX)

			for var cr804a-cr804h: gen  xX=1 if X==1
			for var cr804a-cr804h: egen sumcr804=rowtotal(xX)

			gen    locgovt_resp=(sumcr805/sumcr804)
			la var locgovt_resp "Local government responsiveness" 
			sum    locgovt_resp */

			*/
			
/*14. Gender Index (r1501-r1504 r1507-r1510)
	  This index indicates gender practices at the HH and community levels. (range:0-8)*/
tab1 r1501-r1504 r1508 r1509 r1510
	  
	  

*14a. Community level
*	  The community component variables are equal to 1 if: (1) men and women 
*	  regularly sit and eat together within their HHs, (2) men and women regularly  
*	  sit together at public meetings, (3) men in the village help with childcare, 
*	  and (4) men in the village help fetch firewood OR carry water for the household. 
*     (range:0-4)

tab1 cr901 cr902 cr903 cr904 cr905		
gen gn_comm1 = (cr901==1)
gen gn_comm2 = (cr902==1)
gen gn_comm3 = (cr903==1)
gen gn_comm4 = (cr904==1 | cr905==1)

egen gender_cindex = rowtotal(gn_comm1 gn_comm2 gn_comm3 gn_comm4)
tab gender_cindex if c06a==1


**14b. HH level (r1501-r1510)
*	   For HHs with husband and wife, the HH component summary variable indicates 
*	   if the couple: (1) sit and eat together within their household and/or 
*	   (2) sit together at public meetings. In addition, two binary variables 
*	   indicate whether the spouse/partner helps with childcare, and whether the 
*	   spouse/partner helps fetch firewood OR carry water for the household. (range:0-4)

**14b1. Sit together at home and/or in public
tab1 r1501 r1502 r1503 r1504
gen r1502x=0 if r1501==1
replace r1502x=1 if r1502==1

gen r1504x=0 if r1501==1
replace r1504x=1 if r1504==1
	
gen gn_sit=r1502x+r1504x if r1501==1
tab gn_sit if c06a==1,m

**14b2. Child care - male partner/spouse participates in some capacity
* Note: Kenya  Midline: Use community data
*                   
*                  
*
 
tab cr903
gen     gn_childcare=0 if cr903==2
replace gn_childcare=1 if cr902==1 
tab gn_childcare,m

clonevar m1_line=v300d

merge 1:1 hhea hhnum m1_line using "$analytic\FTF_P2-ZOI_2023_Kenya-Survey_persons_data_analytic.dta", keepusing(v102)

keep if _merge==3|_merge==1
tab c06a
tab v102
**14b3. Fetch water or gather wood - male partner/spouse participates in some capacity
tab1 r1501 r1509 r1510
gen     gn_woodwater=0 
replace gn_woodwater=1 if (v102==2 & r1509_b==1 | r1510_b==1 | ///
						  (v102==1 & r1509_a==1 | r1510_a==1))
tab gn_woodwater if c06a==1

egen    i_gender_hhindex=rowtotal(gn_*)	
lab var i_gender_hhindex "Gender index, Comm & HH (0-8)"					  
tab i_gender_hhindex if c06a==1,m 
sum i_gender_hhindex if c06a==1


/*15. Participation in local decision making
	  This binary variable is equal to 1 if the respondent participates in any 
	  group's decision-making as “leader”, “very active”, or “somewhat active”.
NOTE: The original calc refers to any HH member, but because this was incorporated
	  into the A-WEAI module, captured only primary adult decisionmakers. */
	  
*Uganda adaptation: Could only calculate membership in local groups because did 
*					not include questions about involvement in decision-making.

tab1 r802_*


for var r802_*: gen Xx=1 if X==2|X==3|X==4
tab1 r802_??
tab1 r802_??x
egen i_local_dm=rowtotal(r802_*x)
tab i_local_dm if c06a==1
replace i_local_dm=1 if i_local_dm>0 & i_local_dm!=.
tab i_local_dm if c06a==1
lab var i_local_dm "Individual is actively involved in local groups (0-1)"


********************************************************************************
//6. INDEX OF COMMUNITY RESILIENCE 
********************************************************************************
/*6.1 Natural resource management groups (r308 r310 r312 r314)
	  This summary variable indicates the number of groups in the community that 
	  manage communal natural resources, including : (1) communal grazing land  
	  management group, (2) communal group deciding who can gather wood and how 
	  much from communal land, (3) communal livestock water management group, and  
	  (4) communal irrigation management group (where relevant). (range:0-4)*/
tab1 cr209 cr215 cr212  cr218
for var cr209 cr215 cr212  cr218: gen Xx=1 if X==1
for var cr209 cr215 cr212  cr218: replace Xx=0 if Xx==. & vrresult==1
for var cr209 cr215 cr212  cr218: replace Xx=0 if Xx==2 & vrresult==1
egen nrm_grp=rowtotal(cr209x cr215x cr212x cr218x)
tab1 cr209x cr215x cr212x cr218x
tab nrm_grp if c06a==1,m
sum nrm_grp if c06a==1

la  var nrm_grp "Number of natural resource management groups (0-4)"

* svy, subpop(if hhsize_dj>0): mean cr209x cr215x cr212x cr218x

/*6.2 Community disaster risk reduction index (tab1 r907 r908* r701)
	  This index comprises 3 variables: (1) binary variable indicating if there 
	  is an active disaster planning group in the community, (2) binary variable 
	  indicating any HH member worked collectively with others to reduce the risk  
	  of possible disaster (i.e, soil conservation activities or flood diversion 
	  structures), and (3) the proportion of HHs in the community receiving early  
	  warning information on natural hazards in the year prior to the survey. 
	  (range:0-3)*/
	  
*Uganda adaptation: Did not collect information on presence of disaster planning 
*                   group, so could not include. Adjusted score range:0-2.

tab1 r801_14 r907 r908* r701 prop_shock 

*6.2.1 Active disaster planning group in community
gen risk1=r801_14==1 

*6.2.2 Any HH member worked collectively with others to reduce the risk of
*	   possible disaster
gen risk2=r908f==1 | r908g==1 

*6.2.3 Proportion of HHs in the community receiving early warning info on natural 
*	   hazards in the year prior to the survey
*NOTE: Variable created above with HH piped water and HH electricity 
gen risk3=prop_shock

egen comm_risk=rowtotal(risk?)
lab var comm_risk "Community disater risk reduction index (0-3)"
tab comm_risk if c06a==1
sum comm_risk if c06a==1
/*6.3 Social protection index (v6404_04, m6404_04, v6404_05, m6404_05, v6404_10)
	  This index is constructed from 7 variables. (range:0-7)*/

*6.3.1 Active savings group in the community 
gen protect1=r801_06==1

tab protect1 if c06a==1

*6.3.2 Active mutual help group in the community
gen protect2=r801_07==1

tab protect2 if c06a==1

*6.3.3 Active women's group in the community 
gen protect3=r801_16==1 

tab protect3 if c06a==1

*6.3.4 The proportion of HHs that report having received any assistance from relatives
*	   or friends (i.e., non-relatives) within their community in the 12 months 
* 	   prior to the survey.
tab1 r1312 r1318
gen help_received_in=1 if r1312==1 | r1318==1
replace help_received_in=0 if help_received_in==. & vrresult==1

tab help_received_in  if c06a==1

*6.3.5 The proportion of HHs that report having provided any assistance from relatives
*	   or friends (i.e., non-relatives) within their community in the 12 months 
* 	   prior to the survey.
gen help_provided_in=1 if r1309==1 | r1315==1
replace help_provided_in=0 if help_provided_in==. & vrresult==1

tab help_provided_in  if c06a==1

*6.3.6 The proportion of HHs that report they could turn to a relative or friend 
*	   (i.e., non-relative) in their village if they had a problem and needed help 
*	   urgently.
gen help_turnto_in=1 if r1304a==1 | r1304b==1 
replace help_turnto_in=0 if help_turnto_in==. & vrresult==1

tab help_turnto_in  if c06a==1

*6.3.7 The proportion of HHs that report they would help a relative or friend 
*	   (i.e., non-relative) in their village if they had a problem and needed help 
*	   urgently.
gen help_give_in=1 if r1307a==1 | r1307b==1 
replace help_give_in=0 if help_give_in==. & vrresult==1

tab help_give_in  if c06a==1

preserve
collapse (count) hhnum help_*, by(hhea)
gen prop_help_received_in=help_received_in/hhnum
gen prop_help_provided_in=help_provided_in/hhnum
gen prop_help_turnto_in=help_turnto_in/hhnum
gen prop_help_give_in=help_give_in/hhnum
tab1 prop_help_*
sum prop_help_*
save "$analytic2\Temp\Kenya_P2-ZOI_Survey_comm_help.dta", replace

restore
drop _merge
merge m:1 hhea using "$analytic2\Temp\Kenya_P2-ZOI_Survey_comm_help.dta"
drop _merge

rename prop_help_received_in protect4
rename prop_help_provided_in protect5
rename prop_help_turnto_in protect6
rename prop_help_give_in protect7

sum protect?

global protect protect1 protect2 protect3 protect4 protect5 protect6 protect7

foreach x of varlist $protect {
  replace `x'=. if vrresult!=1 | c06a==2
  egen std_`x'=std(`x')
 }

global std_protect std_protect1-std_protect7

sum $protect if c06a==1
sum $std_protect if c06a==1

mean $protect
mean $std_protect

factor $std_protect if c06a==1, pcf
*estat kmo // 0.6113
predict soc_protect
sum soc_protect

egen max=max(soc_protect)
egen min=min(soc_protect)
gen soc_protect_index=(soc_protect-min)*100/(max-min)
sum soc_protect_index
sum soc_protect_index, detail
lab var soc_protect "Social protection index (pcf)"
drop max min

/*6.4 Managing and maintaining public goods (r908a-r908d r908h r908i)
	  This summary variable indicates the types of public goods maintained by the 
	  community. Public goods are: (1) schools, (2) health centers, (3) roads, 
	  (4) trees on communal land, (5) community irrigation systems, (6) community  
	  sources of drinking water. (range:0-6) */
for var r908a-r908d r908h r908i: gen  Xx=1 if X==1
for var r908ax-r908dx r908hx r908ix: replace  X=0 if X==. & vrresult==1

tab1 r908a-r908d r908h r908i

egen pub_goods=rowtotal(r908ax-r908dx r908hx r908ix)
replace pub_goods=. if vrresult!=1
lab var pub_goods "Managing and maintaining public goods (0-6)"
tab pub_goods,m

sum r908ax-r908dx r908hx r908ix if c06a==1

/*6.5 Conflict mitigation (r362 r363)
	  This summary variable is based on 3 binary variables: (1) community has a  
	  conflict resolution committee, (2) the committee has dealt with a conflict 
	  over the 2 years prior to the survey, and (3) the conflict was reduced as  
	  a result of their involvement. (range:0-3) */
	  
*Uganda and Kenya adaptation: Scoring modified to be on a 0-2 scale because 3rd component  
*     				not included in the Uganda QRE.
tab1 cr802 cr802a 
for var cr802 cr802a: gen Xx=1 if X==1
for var cr802x cr802ax: replace X=0 if X==. 

egen conflict_mit=rowtotal(cr802x cr802ax)
lab var conflict_mit "Conflict mitigation (0-2)"
tab conflict_mit if c06a==1,m
sum conflict_mit if c06a==1

sum cr802x cr802ax
sum cr802ax if cr802x==1


save "$analytic2\Temp\Kenya_2023-RCI_Component_Indicators.dta", replace
