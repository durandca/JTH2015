**********************************************************************
*Analysis of trip distance and access mode to transit using CHTS data
*Prepared by Casey P Durand
*Last updated 1/29/14
**********************************************************************

*Note: The following do-file assumes you have already merged the household,
*person and place files from CHTS.  There should be 460858 records in the dataset

*Also, note that analyses will be conducted separately for access and egress modes to/from transit
*This requires creating two separate files, one access and one egress.  This must be done because
*some trips are both access and egress, and they may be between two different types of transit, e.g.
*walking from a bus to light rail.  If separate access and egress files aren't created, it would be 
*unclear how to code those.

************************************
***Begin code for the ACCESS file***
************************************

order sampn perno plano, after(sampno)

sort sampn perno plano

*create single new variable to indicate individual people by
*combining household and person-within-household identifiers
egen sampnperno=concat(sampn perno)
order sampnperno, after(plano)
destring sampnperno, replace

**Necessary definitions using CHTS coding scheme:
*Active travel: 1 (walking) or 2 (biking)
*Transit: 15 (local bus/rapid bus), 16 (express bus), 17 (premium bus), 19 (public transit shuttle)
*24 (BART, Metro Red/Purple line, aka heavy rail), 26 (light rail), 27 (street car/cable car)

*Create a variable to flag those trips that preceded a transit trip.  We are assuming
*these are how they accessed transit.

gen accessflag=0
la var accessflag "Flag obs if preceded a transit trip"
order accessflag, after(mode)
bys sampnperno: replace accessflag=1 if inlist(mode[_n+1], 15, 16, 17, 19, 24, 26, 27)

*Create a variable to indicate if access mode was active (walking/biking)

gen activaxs=.
la var activaxs "Transit access was active (1) or not (0)"
order activaxs, after(accessflag)
bys sampnperno: replace activaxs=0 if accessflag==1
bys sampnperno: replace activaxs=1 if accessflag==1 & inlist(mode,1,2)

*Create variable indicating the transit mode they were accessing
bys sampnperno: g destmode=mode[_n+1] if accessflag==1 & inlist(mode[_n+1], 15, 16, 17, 19, 24, 26, 27)
la var destmode "Mode of transit they were accessing"

*Create variable indicating the origin of the transit access trip (i.e. what it is they were coming from before the transit trip)

bys sampnperno: g origin=0 if accessflag==1 
bys sampnperno: replace origin=1 if pname[_n-1]=="HOME" & accessflag==1 
la var origin "Origin of trip was (1) home or (0) other"


*keep only those observations that are transit access trips.
keep if accessflag==1

*Update 1/29/15.  We will exclude bike riders from "active access" group since there are so few and their behavior
*is likely qualitatively different from walkers.  We will also drop those who access transit via a wheelchair, other non-motorized form
*or plane, since it is unlikely those are modes that are possible to change.
drop if inlist(mode, 2, 3, 4, 13)



********************************************************************************
*Clean reduced dataset to recode missing/refused responses to Stata missing (.)*
********************************************************************************

foreach var of varlist hhbic resty ten incom hhsiz vehop race1 wkstat jobs{

recode `var' (97 98 99 998 999=.), g (`var'2)

 }

foreach var of varlist own gend hisp ntvty lic trans emply wsched disab educa{

recode `var' (7 8 9=.), g (`var'2)

 }



recode age (998 999=.), g(age2)

recode wloc (7 8 9=.), g (wloc3)

*Create new version of residence type to remove category 7 (boat/rv/van)

recode resty2 (7=.), g(resty3)

*Create new version of walking and biking in the last week variables
recode wtrip (98 99=.), g(wtrip2)
recode btrip (98 99=.), g(btrip2)

*Create variable indicating month of year data was collected
g month=substr(recdate, 6,2)
destring month, replace

*keep only adults (18+)
keep if age2>=18 

*Recode 1/2 variables to 0/1
foreach var of varlist own2 gend2 hisp2 ntvty2 lic2 emply2 disab2 {
recode `var'(2=0)
}


***************************
*Run non-interacted models*
***************************

*Preliminary model
 xtgee activaxs tripdistance i.origin i.month incom2 hhsize2 i.own2 i.gend2 i.hisp2 i.ntvty2 ///
 i.lic2 i.emply2 i.disab2 i.educa2 age2 i.resty3 i.dow hhveh i.destmode tripno, i(sampn) family(binomial) link(probit) r 
 
*Make table of probit coefficents.  Table will be generated in excel so final formatting can be done at discretion of user.
 outreg2 using access.xls, replace dec(2) sideway noparen
 
 *Generate average marginal effects
 margins, dydx(_all) post
 
 *Add to table of coefficients
 outreg2 using access.xls, dec(2) sideway noparen
 
 *Joint significance tests of multicategory variables 
  foreach var of varlist month educa2 resty3 dow destmode {
  testparm i.`var'
  }

 *Post-hoc pairwise comparison testing of significant "destmode" effect
 pwcompare destmode, group
 
 *Make distance decay graph
  margins, at(tripdistance=(0(.5)5))
  marginsplot, title(Predicted Probability of Active Access) xtitle(Trip Distance (Miles)) ytitle(Predicted Probability)
 
************************
*Run interaction models*
************************
  
*Automate the testing of individual interaction effects. Discrete vars first
foreach ivar of varlist origin month own2 gend2 hisp2 ntvty2 ///
 lic2 emply2 disab2 educa2 resty3 dow destmode {
 
  xtgee activaxs c.tripdistance##i.`ivar' i.origin i.month incom2 hhsize2 i.own2 i.gend2 i.hisp2 i.ntvty2 ///
  i.lic2 i.emply2 i.disab2 i.educa2 age2 i.resty3 i.dow hhveh i.destmode tripno, i(sampn) family(binomial) link(probit) r 
  
  }

*Now continuous vars

foreach cvar of varlist incom2 hhsize2 age2 hhveh tripno {

 xtgee activaxs c.tripdistance##c.`cvar' i.origin i.month incom2 hhsize2 i.own2 i.gend2 i.hisp2 i.ntvty2 ///
  i.lic2 i.emply2 i.disab2 i.educa2 age2 i.resty3 i.dow hhveh i.destmode tripno, i(sampn) family(binomial) link(probit) r 
  
  }

*Joint significance tests for interactions with multicategory variables
  
  foreach ivar of varlist month educa2 resty3 dow destmode {
 
  xtgee activaxs c.tripdistance##i.`ivar' i.origin i.month incom2 hhsize2 i.own2 i.gend2 i.hisp2 i.ntvty2 ///
  i.lic2 i.emply2 i.disab2 i.educa2 age2 i.resty3 i.dow hhveh i.destmode tripno, i(sampn) family(binomial) link(probit) r 
  
  testparm c.tripdistance#i.`ivar'
  
  }
  
  
***Make graphs of significant interaction effects
*Origin
*Run interaction model again
 xtgee activaxs c.tripdistance##i.origin i.month incom2 hhsize2 i.own2 i.gend2 i.hisp2 i.ntvty2 ///
  i.lic2 i.emply2 i.disab2 i.educa2 age2 i.resty3 i.dow hhveh i.destmode tripno, i(sampn) family(binomial) link(probit) r 
 
*Run margins command and marginsplot to produce graph
margins, dydx(origin) at(tripdistance=(0(.5)5))
marginsplot, title(Interaction Between Trip Origin and Distance) xtitle(Trip Distance (Miles)) ///
ytitle("{&Delta} Predicted Probability of Active Access" "between Home- and Non-home-based Trips")
 
*Month
*Run interaction model again
 xtgee activaxs c.tripdistance##i.month i.origin incom2 hhsize2 i.own2 i.gend2 i.hisp2 i.ntvty2 ///
  i.lic2 i.emply2 i.disab2 i.educa2 age2 i.resty3 i.dow hhveh i.destmode tripno, i(sampn) family(binomial) link(probit) r 
  
*Run margins command and marginsplot to produce graph.  Note that graph is not reported in paper. 
margins, dydx(month) at(tripdistance=(0(.5)5))
marginsplot, by(_deriv) 

********************************************
*Code for generating descriptive statistics*
********************************************

sort sampn perno plano

*Number of households
distinct sampn

*Number of people
distinct sampnperno

*Need to reduce dataset so each individual is only represented on one line in order to generate person-level descriptives.

*Preserve existing file
preserve

*Keep only the first instance of any person-level ID number
duplicates drop sampnperno, force

*Generate percentages for dichotomous variables

tabstat own2 gend2 hisp2 ntvty2 lic2 emply2 disab2

*Generate medians for continuous variables
tabstat age2 incom2 hhsize2 educa2 hhveh, s(q)

*Restore old file
restore

*Now report trip-level variables using original dataset

tab destmode

summ tripdistance, d

tab activaxs

tab origin


  
************************************
***Begin code for the EGRESS file***
************************************

order sampn perno plano, after(sampno)

sort sampn perno plano

*create single new variable to indicate individual people by
*combining household and person-within-household identifiers
egen sampnperno=concat(sampn perno)
order sampnperno, after(plano)
destring sampnperno, replace

**Necessary definitions using CHTS coding scheme:
*Active travel: 1 (walking) or 2 (biking)
*Transit: 15 (local bus/rapid bus), 16 (express bus), 17 (premium bus), 19 (public transit shuttle)
*24 (BART, Metro Red/Purple line, aka heavy rail), 26 (light rail), 27 (street car/cable car)


*Create a variable to flag those trips that followed a transit trip.  We are assuming
*these are how they departed from transit.

gen egressflag=0
la var egressflag "Flag obs if followed a transit trip"
order egressflag, after(mode)
bys sampnperno: replace egressflag=1 if inlist(mode[_n-1], 15, 16, 17, 19, 24, 26, 27)

*Create a variable to indicate if departure mode was active (walking/biking)

gen activdep=.
la var activdep "Transit egress was active (1) or not (0)"
order activdep, after(egressflag)
bys sampnperno: replace activdep=0 if egressflag==1
bys sampnperno: replace activdep=1 if egressflag==1 & inlist(mode,1,2)

*Create variable indicating the transit mode they were departing from
bys sampnperno: g arrivemode=mode[_n-1] if egressflag==1 & inlist(mode[_n-1], 15, 16, 17, 19, 24, 26, 27)
la var arrivemode "Mode of transit they were departing from"

*Create variable indicating the destination of the transit egress trip (i.e. what it is they were going to after the transit trip)
*Coded as home or not home
bys sampnperno: g dest=0 if egressflag==1 
bys sampnperno: replace dest=1 if pname=="HOME" & egressflag==1 
la var dest "Destination of trip was (1) home or (0) not home"

*keep only those observations that are transit egress trips.
keep if egressflag==1

********************************************************************************
*Clean reduced dataset to recode missing/refused responses to Stata missing (.)*
********************************************************************************

foreach var of varlist hhbic resty ten incom hhsiz vehop race1 wkstat jobs{

recode `var' (97 98 99 998 999=.), g (`var'2)

 }

foreach var of varlist own gend hisp ntvty lic trans emply wsched disab educa{

recode `var' (7 8 9=.), g (`var'2)

 }



recode age (998 999=.), g(age2)

recode wloc (7 8 9=.), g (wloc3)

*Create new version of residence type to remove category 7 (boat/rv/van)

recode resty2 (7=.), g(resty3)

*Create new version of walking and biking in the last week variables
recode wtrip (98 99=.), g(wtrip2)
recode btrip (98 99=.), g(btrip2)

*Create variable indicating month of year data was collected
g month=substr(recdate, 6,2)
destring month, replace


*keep only adults (18+)
keep if age2>=18 

*Recode 1/2 variables to 0/1 (1 stays 1; 2 is recoded to 0)
foreach var of varlist own2 gend2 hisp2 ntvty2 lic2 emply2 disab2 {
recode `var'(2=0)
}


************************
*EGRESS Analysis models*
************************

*The same code as was used in the ACCESS models can be used.
*The only difference is that instead of ACTIVAXS as the DV, you should use ACTIVDEP
*And instead of ORIGIN you should use DEST
*And instead of DESTMODE you should use ARRIVEMODE




















