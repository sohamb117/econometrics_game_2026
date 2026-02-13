**TABLE 3:  COLUMNS 1 and 2 

*set environment
clear
capture log close

#delimit;
set more off;
set mem 300m;

****************************************************************************************************;
**LOAD DATA AND NAME VARIABLE;
****************************************************************************************************;
cd "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RDD\";

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

gen first4=0;
replace first4=1 if t1!=.;

**Create final four days dummy;
gen t2=.;
replace t2=1 if month!=month[_n+1];
replace t2=2 if t2[_n+1]==1;
replace t2=3 if t2[_n+1]==2;
replace t2=4 if t2[_n+1]==3;

keep if t1!=. | t2!=.;

**Generating "in the money" for the month;
gen inthemoney=0;
replace inthemoney=inthemoney_day if t2==1;
replace inthemoney=inthemoney[_n+1] if t2==2;
replace inthemoney=inthemoney[_n+1] if t2==3;
replace inthemoney=inthemoney[_n+1] if t2==4;
replace inthemoney=inthemoney[_n-1] if t1==1;
replace inthemoney=inthemoney[_n-1] if t1==2;
replace inthemoney=inthemoney[_n-1] if t1==3;
replace inthemoney=inthemoney[_n-1] if t1==4;

**INTERACTION TERM;
gen first4_inthemoney=first4*inthemoney;

**Merge in the average attendance of the control group;
sort block year month;
merge block year month using av_attend;


****************************************************************************************************;
**Run regressions;
****************************************************************************************************;

sort block;
tab block, gen(block_);

sort schid;
tab schid, gen(schid_);
sort month year;
egen time=group(month year);
tab time, gen(time_);

gen first4_open=first4*open;

**ONLY THE LAST AND FIRST DAY;
preserve;
keep if t1==1 | t2==1;

regress worked first4 inthemoney first4_inthemoney, cluster(schid);
outreg first4 inthemoney first4_inthemoney using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\table3_Col12.csv", bdec(2) se 3aster rdec(2) comma bracket replace;

reg worked first4 inthemoney first4_inthemoney block_* time_* schid_*;
outreg first4 inthemoney first4_inthemoney using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\table3_Col12.csv", bdec(2) se 3aster rdec(2) comma bracket append;

restore;










