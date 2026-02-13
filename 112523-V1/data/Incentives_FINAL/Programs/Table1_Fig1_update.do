*SETTING ENVIRONMENT;
#delimit;
clear;
capture log close;

log using "C:\Incentives_Matter_Data\Incentives_FINAL\Log Files\Table1Figure1.log", replace;

set more off;
set mem 300m;
set matsize 150;

cd "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Teacher Attendance\";
use "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Teacher Attendance\randomcheck.dta";

**************************************************************************************************;
**CREATING DATA FOR CHARTS;
**************************************************************************************************;
**ALL SCHOOLS;
**QUICKLY RECODING DATES;
gen time=1 if month==8 & year==2003;
replace time=2 if month==9 & year==2003;
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

**ALLOCATING AUGUST 25th schools to September;
replace time=2 if month==8 & day>24;

drop if schid==1111;
drop if schid==1211;
drop if schid==2493;
drop if schid==5221;
drop if schid==5231;
drop if schid==5332;
drop if schid==5711;

gen RC=1;
sort schid;
save randomcheck_CODED, replace;

**************************************************************************************************************;
**TABLE 1;
sort treat;

**PANEL A COL 1 COL 2;
by treat: sum open if time==1;

**PANEL B AND D COL 1 COL 2;
by treat: sum students inside bbused interact_kids if time==1 & open==1;

**PANEL A COL 3;
regress open treat if time==1;

**PANEL B AND D COL 3;
regress students treat if time==1 & open==1;
regress inside treat if time==1 & open==1;
regress bbused treat if time==1 & open==1;
regress interact_kids treat if time==1 & open==1;

************************************************************************************************************;

**Figure 1:  RANDOM CHECKS GRAPH;
sort time treat;

preserve;
keep if treat==0;
tabstat open, by(time) stats(mean) columns(statistics);
restore;

preserve;
keep if treat==1;
tabstat open, by(time) stats(mean) columns(statistics);
restore;

clear;

************************************************************************************************************;
**PANEL C:  TEACHER QUALIFICATIONS;

use "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Teacher Attendance\Teacher_test.dta";

gen score=comp1+comp2+comp3+clozet+story+attitude+nnc1+nc1+nc2+nnc2+nnc3+nc3+vp+map;

keep schid score;
sort schid;

merge schid using randomcheck_coded;
keep if RC==1;

sort schid;
collapse (max) score, by(schid treat);

**PANEL C COL 1 COL 2;
bys treat: sum score;

**PANEL C COL 3;
regress score treat;
