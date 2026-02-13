**TABLE 3 COLUMNS 3 and 4
**FIGURE 3

*set environment
clear
capture log close

#delimit;
set more off;
set mem 300m;

****************************************************************************************************;
**LOAD DATA AND NAME VARIABLE;
****************************************************************************************************;
cd "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RDD";

insheet using cameras.txt;

gen schid=v1;
gen year=v2;
gen month=v3;
gen day=v4;
gen weekday=v5;
gen holiday=v6;
gen worked=v7;

drop v*;

drop if schid==23513 & month==10 & year==2004;

****************************************************************************************************;
**Organize data for regressions;
****************************************************************************************************;
**Create block;
gen block=int(schid/10000);

**Create in the money dummy;
gen worked_temp=worked;
replace worked_temp=1 if holiday==1;
sort schid year month day;

gen money1=worked_temp if day==1;
replace money1=money1[_n-1]+worked_temp if day==2;
replace money1=money1[_n-1]+worked_temp if day==3;
replace money1=money1[_n-1]+worked_temp if day==4;
replace money1=money1[_n-1]+worked_temp if day==5;
replace money1=money1[_n-1]+worked_temp if day==6;
replace money1=money1[_n-1]+worked_temp if day==7;
replace money1=money1[_n-1]+worked_temp if day==8;
replace money1=money1[_n-1]+worked_temp if day==9;
replace money1=money1[_n-1]+worked_temp if day==10;
replace money1=money1[_n-1]+worked_temp if day==11;
replace money1=money1[_n-1]+worked_temp if day==12;
replace money1=money1[_n-1]+worked_temp if day==13;
replace money1=money1[_n-1]+worked_temp if day==14;
replace money1=money1[_n-1]+worked_temp if day==15;
replace money1=money1[_n-1]+worked_temp if day==16;
replace money1=money1[_n-1]+worked_temp if day==17;
replace money1=money1[_n-1]+worked_temp if day==18;
replace money1=money1[_n-1]+worked_temp if day==19;
replace money1=money1[_n-1]+worked_temp if day==20;
replace money1=money1[_n-1]+worked_temp if day==21;
replace money1=money1[_n-1]+worked_temp if day==22;
replace money1=money1[_n-1]+worked_temp if day==23;
replace money1=money1[_n-1]+worked_temp if day==24;
replace money1=money1[_n-1]+worked_temp if day==25;
replace money1=money1[_n-1]+worked_temp if day==26;
replace money1=money1[_n-1]+worked_temp if day==27;
replace money1=money1[_n-1]+worked_temp if day==28;
replace money1=money1[_n-1]+worked_temp if day==29;
replace money1=money1[_n-1]+worked_temp if day==30;
replace money1=money1[_n-1]+worked_temp if day==31;

gen inthemoney_day=0;
replace inthemoney_day=1 if money1>10;
label var inthemoney_day "IN THE MONEY";

**Drop weekends and holidays;
drop if weekday==0;
drop if holiday==1;

**Create 1st four days dummy;
sort schid year month day;
gen t1=1 if month!=month[_n-1];
replace t1=2 if t1[_n-1]==1;
replace t1=3 if t1[_n-1]==2;
replace t1=4 if t1[_n-1]==3;
replace t1=5 if t1[_n-1]==4;
replace t1=6 if t1[_n-1]==5;
replace t1=7 if t1[_n-1]==6;
replace t1=8 if t1[_n-1]==7;
replace t1=9 if t1[_n-1]==8;
replace t1=10 if t1[_n-1]==9;

gen first4=0;
replace first4=1 if t1!=.;

**Create final four days dummy;
gen t2=.;
replace t2=1 if month!=month[_n+1];
replace t2=2 if t2[_n+1]==1;
replace t2=3 if t2[_n+1]==2;
replace t2=4 if t2[_n+1]==3;
replace t2=5 if t2[_n+1]==4;
replace t2=6 if t2[_n+1]==5;
replace t2=7 if t2[_n+1]==6;
replace t2=8 if t2[_n+1]==7;
replace t2=9 if t2[_n+1]==8;
replace t2=10 if t2[_n+1]==9;

keep if t1!=. | t2!=.;

**Generating "in the money" for the month;
gen inthemoney=0;
replace inthemoney=inthemoney_day if t2==1;
replace inthemoney=inthemoney[_n+1] if t2==2;
replace inthemoney=inthemoney[_n+1] if t2==3;
replace inthemoney=inthemoney[_n+1] if t2==4;
replace inthemoney=inthemoney[_n+1] if t2==5;
replace inthemoney=inthemoney[_n+1] if t2==6;
replace inthemoney=inthemoney[_n+1] if t2==7;
replace inthemoney=inthemoney[_n+1] if t2==8;
replace inthemoney=inthemoney[_n+1] if t2==9;
replace inthemoney=inthemoney[_n+1] if t2==10;

replace inthemoney=inthemoney[_n-1] if t1==1;
replace inthemoney=inthemoney[_n-1] if t1==2;
replace inthemoney=inthemoney[_n-1] if t1==3;
replace inthemoney=inthemoney[_n-1] if t1==4;
replace inthemoney=inthemoney[_n-1] if t1==5;
replace inthemoney=inthemoney[_n-1] if t1==6;
replace inthemoney=inthemoney[_n-1] if t1==7;
replace inthemoney=inthemoney[_n-1] if t1==8;
replace inthemoney=inthemoney[_n-1] if t1==9;
replace inthemoney=inthemoney[_n-1] if t1==10;



**INTERACTION TERM;
gen first4_inthemoney=first4*inthemoney;

**Merge in the average attendance of the control group;
sort block year month;
merge block year month using av_attend;

sort block;
tab block, gen(block_);

sort schid;
tab schid, gen(schid_);
sort month year;
egen time=group(month year);
tab time, gen(time_);

gen first4_open=first4*open;

gen t=t1;
replace t=-t2 if t==.;

save "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RDD\data_RDD", replace;

replace t1=0 if t2!=.;
replace t2=0 if t1!=0;

gen t1_s=t1*t1;
gen t1_t=t1_s*t1;

gen t2_s=t2*t2;
gen t2_t=t2_s*t2;


**CREATE REGRESSION FOR TABLE;
regress worked first4 inthemoney first4_inthemoney t1 t1_s t1_t t2 t2_s t2_t, cluster(schid);
outreg first4 inthemoney first4_inthemoney using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\table3_Col34.csv", bdec(2) se 3aster rdec(2) comma bracket replace;

regress worked first4 inthemoney first4_inthemoney t1 t1_s t1_t t2 t2_s t2_t time_* schid_*;
outreg first4 inthemoney first4_inthemoney using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\table3_Col34.csv", bdec(2) se 3aster rdec(2) comma bracket append;



*************************************************************************************;
******************************************GRAPHS************************************;
*************************************************************************************;


* This program generates graphs for nonparametric RDD following Lee (2005) "Randomized Experiments for Non-random Selection in U.S. House Elections";
* It calcuates local averages within fixed intervals from the cutoff and fits a separate polynomial to these averages on each side of the cutoff;		

set seed 101;

*------------------------------------------*;
* Creating local averages within intervals *;
*------------------------------------------*;

*MY INTERVALS OF INTEREST ARE PERCENTILES;
*cec is the above-the-cutff dummy;
gen interval=t;
gen cec=0 if t!=.;
replace cec=1 if t>0&t!=.;

*MY OUTCOME VARIABLES: WORKED;

save forgraphs, replace;

use forgraphs, clear;

**********************************************;
*****GRAPH FOR TREATMENT SCHOOLS THAT ARE IN THE MONEY;
************************************************;

**keep if inthemoney==1;
collapse worked, by(interval cec inthemoney);

*---------------------------------------*;
* Fitting polynomials to local averages *;
*---------------------------------------*;

gen mworked=worked if inthemoney==1;
gen nworked=worked if inthemoney==0;

forvalues x=2/3{;		
gen interval`x'=interval^`x';
};

foreach var of varlist worked{;
forvalues x=0/1{;
reg `var' interval* if cec==`x' & inthemoney==1;
predict my`x'`var' if e(sample), xb;
};
};

foreach var of varlist worked{;
forvalues x=0/1{;
reg `var' interval* if cec==`x' & inthemoney==0;
predict y`x'`var' if e(sample), xb;
};
};


*-------------------*;
* Generating graphs *;
*-------------------*;


foreach var of varlist worked {;
twoway scatter m`var' my1`var' my0`var' interval, ms(O i i) c(i l l) clp(blank l l) clw(med med med) mc(sand navy navy) clc(sand navy navy) || || scatter n`var' y1`var' y0`var' interval, ms(O i i) c(i l l) clp(blank l l) clw(med med med) mc(sand red red) clc(sand red red) xline(0)
legend(off)
xtitle("Days") ytitle("Attendance Rate")
saving("C:\Incentives_Matter_Data\Incentives_FINAL\Output\FIG3_T", replace);
};



