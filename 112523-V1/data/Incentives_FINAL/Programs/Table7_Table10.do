#delimit;
clear;
capture log close;

log using "C:\Incentives_Matter_Data\Incentives_FINAL\Log Files\Table7_Table10.log", replace;
cd "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster";

set more off;
set mem 1000m;
set matsize 150;

******************************************************************************************************************;
**A.  Load Roster Data;
******************************************************************************************************************;
use "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster\roster_rema2012.dta";
drop if month==12 & year==2005;
append using "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster\rosterfin";

sort schid childno attendance;
drop if year==2003 & month<9;
drop if year==205;

gen time=2 if month==9 & year==2003;
replace time=3 if month==10 & year==2003;
replace time=4 if month==11 & year==2003;
replace time=5 if month==12 & year==2003;
replace time=6 if month==1 & year==2004;
replace time=7 if month==2 & year==2004;
replace time=8 if month==3 & year==2004;
replace time=9 if month==4 & year==2004;
replace time=10 if month==5 & year==2004;
replace time=11 if month==6 & year==2004;
replace time=12 if month==7 & year==2004;
replace time=13 if month==8 & year==2004;
replace time=14 if month==9 & year==2004;
replace time=15 if month==10 & year==2004;
replace time=16 if month==11 & year==2004;
replace time=17 if month==12 & year==2004;
replace time=18 if month==1 & year==2005;
replace time=19 if month==2 & year==2005;
replace time=20 if month==3 & year==2005;
replace time=21 if month==4 & year==2005;
replace time=22 if month==5 & year==2005;
replace time=23 if month==6 & year==2005;
replace time=24 if month==7 & year==2005;
replace time=25 if month==8 & year==2005;
replace time=26 if month==9 & year==2005;
replace time=27 if month==10 & year==2005;
replace time=28 if month==11 & year==2005;
replace time=29 if month==12 & year==2005;
replace time=30 if month==1 & year==2006;
replace time=31 if month==2 & year==2006;

gen trend=time-1;

**ATTENDANCE=0 MEANS STUDENT IS NOT PRESENT THAT DAY;
replace attendance = 2 if attendance ==-999 |attendance ==-888 |attendance ==-777 |attendance ==-666 |attendance ==-444 |attendance ==-333 ;
replace attendance=0 if attendance==2;
replace attendance=0 if attendance==22;

**CODING STATUS OF CHILD;
gen reg=.;
replace reg=0 if status==1;
replace reg=1 if status==2;
label variable reg "CHILD ATTENDS SCHOOL REGULARLY (>10 DAYS)";

******************************************************************************************************************;
**B.  GETING RANDOM CHECK DATA;
******************************************************************************************************************;
sort schid year month day;

merge schid year month day using "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster\rc_for_roster";

drop if randomcheck==.;
drop if month==8 & year==2003;

save Temp/temp1, replace;
clear;

******************************************************************************************************************;
**D.  MERGE IN ENROLLMENT DATA;
******************************************************************************************************************;
**ENROLLED STATUS;
use Temp/temp1;
drop _merge;
sort schid childno;
merge schid childno using "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster\enroll1";

drop _merge;
sort schid childno;
merge schid childno using "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster\pre_writ_attend";

***INTERACTIONS IN TIME PERIODS;
gen pretest=1 if time<10;
replace pretest=0 if pretest==.;

gen posttest=1 if time==10 | time==11 | time==12 | time==13 | time==14 | time==15;
replace posttest=0 if posttest !=1;

gen postexp=0;
replace postexp=1 if time>15;

gen treatpre=treat*pretest;
gen treatpost=treat*posttest;
gen treatexp=treat*postexp;

sort time;
tab time, gen(t_);

drop if time==1;
drop if childno==.;

**KEEPING ONLY ENROLLED KIDS;
keep if enrolled1==1;

save Temp\Roster_CODED, replace;

******************************************************************************************************************;
**E.  TABLE 7;
******************************************************************************************************************;
**PANEL A, COL 1;
preserve;
keep if open==1;

sort treat;
by treat: sum attendance;

reg attendance treat , cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7A.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

regress attendance treatpre treatpost treatexp t_*, cluster(schid);
outreg treatpre treatpost treatexp using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7A.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;

**PANEL B, COL 1;
preserve;

sort treat;
by treat: sum attendance;

reg attendance treat , cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7B.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

regress attendance treatpre treatpost treatexp t_*, cluster(schid);
outreg treatpre treatpost treatexp using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7B.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;

******************************************************************************************************************;
**E.  DEFINING DROPOUTS;
******************************************************************************************************************;
**DEFINING CHILD THAT DROPPED OUT;
keep if open==1;
gsort schid childno -year -month -day;

gen last=0;
replace last=last[_n-1]+1 if childno==childno[_n-1] & schid==schid[_n-1];
keep if last<5;
gen tempgov=0;
replace tempgov=1 if reason=="2";

sort schid childno treat;
collapse (sum) attendance tempgov, by(schid childno treat);
tab attendance;
tab tempgov;

gen drop=0;
replace drop=1 if attendance==0;

gen gov=0;
replace gov=1 if tempgov!=0 & drop==1;

keep schid childno drop gov treat;
sort schid childno treat;
save Temp\drop, replace;

clear;

use Temp\Roster_CODED;
drop _merge;
sort schid childno treat;
merge schid childno treat using Temp\drop;

**RUNNING REGRESSIONS FOR NONDROPOUTS;
**PANEL A, ROW 2;
preserve;
keep if open==1;

gen drop2=drop;
replace drop2=0 if gov==1;

keep if drop==0;

sort treat;
by treat: sum attendance;

reg attendance treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7A.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress attendance treatpre treatpost treatexp t_*, cluster(schid);
outreg treatpre treatpost treatexp using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7A.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;


**PANEL B, ROW 2;
preserve;

gen drop2=drop;
replace drop2=0 if gov==1;

keep if drop==0;

sort treat;
by treat: sum attendance;

reg attendance treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7B.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress attendance treatpre treatpost treatexp t_*, cluster(schid);
outreg treatpre treatpost treatexp using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7B.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;


**PANEL C ROW 1;
preserve;
keep if pre_writ==0;

gen drop2=drop;
replace drop2=0 if gov==1;

keep if drop==0;

sort treat;
by treat: sum attendance;

reg attendance treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7C.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

regress attendance treatpre treatpost treatexp t_*, cluster(schid);
outreg treatpre treatpost treatexp using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7C.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;

**PANEL C ROW 2;

preserve;
keep if pre_writ==1;

gen drop2=drop;
replace drop2=0 if gov==1;

keep if drop==0;

sort treat;
by treat: sum attendance;

reg attendance treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7C.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress attendance treatpre treatpost treatexp t_*, cluster(schid);
outreg treatpre treatpost treatexp using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table7C.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;

******************************************************************************************************************;
**TABLE 10;
******************************************************************************************************************;
clear;
use Temp/drop;

**DROPOUTS;
sort treat;
by treat: sum drop;

reg drop treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table10.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

**ENROLLED IN GOVT SCHOOLS;
sort treat;
by treat: sum gov;

reg gov treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table10.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

**DROPOUTS WHO DID NOT ENROLL IN SCHOOL;
gen drop2=drop;
replace drop2=0 if gov==1;

sort treat;
by treat: sum drop2;

reg drop2 treat, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table10.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;


clear;

