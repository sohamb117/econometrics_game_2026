*SETTING ENVIRONMENT;
clear
#delimit;
capture log close;
capture program drop zscore1;
log using "C:\Incentives_Matter_Data\Incentives_FINAL\Log Files\Table1PanelE", replace;
cd "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\Tests";
set more off;
set mem 400m;
set matsize 500;

******************************************************************************************************************;
**Closed School data;
******************************************************************************************************************;
use "Closed School.dta";
gen close=1 if mon10=="Closed" &  mon11=="Closed";
gen schid=idnum;
keep schid close;
sort schid;
save Temp\tempclose, replace;

******************************************************************************************************************;
**PRE_TEST DATA;
******************************************************************************************************************;
*PRETEST VERBAL;
use Baseline\pre_verbal;
rename total_1_to_4 pre_math_v;
rename total_5_to_11 pre_lang_v;
rename total pre_total_v;
gen pre_writ=0;
keep schid childno pre_math_v pre_lang_v pre_total_v pre_writ;
save Temp\temp1, replace;
clear;

*PRETEST WRIT;
use Baseline\pre_written;
rename total_1_to_4 pre_math_w;
rename total_5_to_11 pre_lang_w;
rename total pre_total_w;
gen pre_writ=1;
keep schid childno pre_math_w pre_lang_w pre_total_w pre_writ;
save Temp\temp2, replace;
clear;

*STACK PRETEST;
use Temp\temp1;
append using Temp\temp2;
sort schid childno;
save Temp\testdata, replace;
clear;

******************************************************************************************************************;
**MID TEST DATA;
******************************************************************************************************************;
use Midline\mid_verbal;
rename tot_1_to_4 mid_math_v;
rename tot_5_to_11 mid_lang_v;
rename total mid_total_v;
keep schid childno mid_math_v mid_lang_v mid_total_v;
replace schid=2492 if schid==2493;
sort schid childno;
save Temp\temp3, replace;
clear;


*MIDTEST WRIT;
use Midline\mid_written;
rename total_1_to_5 mid_math_w;
rename total_6_to_16 mid_lang_w;
rename total mid_total_w;
gen mid_writ=1;
keep schid childno mid_math_w mid_lang_w mid_total_w mid_writ;
replace schid=2492 if schid==2493;
sort schid childno;
save Temp\temp4, replace;
clear;

*MERGE TEST;
use Temp\temp3;
sort schid childno;
merge schid childno using Temp\temp4;
drop _merge;

******************************************************************************************************************;
**MERGING PRE AND MID TEST;
******************************************************************************************************************;
sort schid childno;
merge schid childno using Temp\testdata;
save Temp\testdata, replace;

**FIXING MISTAKES IN THE DATA;
replace pre_math_w=56 if schid==4231 & childno==3;
replace pre_total_v=11 if schid==3511 & childno==17;
replace pre_total_w=34 if schid==4252 & childno==22;

gen block = int(schid/1000);

*MERGING IN TREATMENT STATUS;
drop _merge;
sort schid;
merge schid using treatschool;
gen treat = 0;
replace treat = 1 if _merge == 3;

**RECODING VARIABLES;
for var _all: replace X = . if X == -999;
for var _all: replace X = . if X == -888;
for var _all: replace X = . if X == -777;

**MERGING IN CLOSURE STATUS;
drop _merge;
sort schid;
merge schid using Temp\tempclose;

drop if treat==.;

save Temp\test_new.dta, replace;

**DROPING SCHOOLS WITH NO PRE OR POST DATA THAT WERE BASICALLY NEVER OPEN;
drop if schid==1211;
drop if schid==5332;
drop if schid==5711;
drop if schid==1111;
drop if schid==2111;
drop if schid==5221;
drop if schid==5611;

**DROPPING SCHOOL WITHOUT A PRETEST;
drop if schid==1113;

drop if schid==5731 & childno==24;

**drop if close==1;
save Temp\test_new.dta, replace;

*LOOKING AT ENTRY AND EXIT;
gen entry=0;
replace entry=1 if (pre_total_v==. & pre_total_w==.) & mid_total_v!=. ;

gen missing=0;
replace missing=1 if (pre_total_v!=. | pre_total_w!=.) & mid_total_v==. ;

gen stayer=0;
replace stayer=1 if missing==0 & entry==0;

gen treat_missing=treat*missing;

save Temp\test_new, replace;

******************************************************************************************************************;
**ADDING SCORES;
******************************************************************************************************************;
*drop if stayer==0;

replace pre_math_w=pre_math_w/5;
replace pre_lang_w=pre_lang_w/5;
replace pre_total_w=pre_total_w/5;

program define zscore1;
	syntax, option1(string);

sort treat;

egen m1_`option1'=mean(`option1') if treat==0;
egen sd1_`option1'=sd(`option1') if treat==0;

egen m_`option1'=mean(m1_`option1');
egen sd_`option1'=mean(sd1_`option1');

gen z_`option1'=(`option1'-m_`option1')/sd_`option1';

end;

zscore1, option1(pre_math_w);
zscore1, option1(pre_lang_w);
zscore1, option1(pre_total_w);
zscore1, option1(pre_total_v);

replace pre_writ=. if z_pre_total_v==. & z_pre_total_w==.;

**TABLE 1 PANEL E;
sort treat;
by treat: sum pre_writ z_pre_total_v z_pre_total_w;

regress pre_writ treat, cluster(schid);
regress z_pre_total_v treat, cluster(schid);
regress z_pre_total_w treat, cluster(schid);


