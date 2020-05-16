* set the stata_analysis_home_folder as the working directory
* for example, on my computer that is:
cd "E:\slvst\King\PycharmProjects\BL_data_generation\stata_analysis_home_folder"

*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************
*******************************************

set scheme s2color


* load the data generated in python
* set the working directory to the data
cd "data\data_in_stata_format"

********************************************************
*                LOAD DATA                 *
********************************************************

clear
clear mata
clear matrix
set maxvar 32767
use "exp_data_python.dta", clear

*give the name of the program that generated the data (0-Mathematica, 1-Python)
sum generated_by


********************************************************
*                INITIALIZE                 *
********************************************************
set more off

*encode timeframe, gen (tf)
* vars with "pr" in the end are the "true" values using a high sampling
* without  pr are the values as measured on a monthly basis (30 samples)

* PF is the Forward Price
* forward_premium is the Forward Premium 
gen forward_premium= (price_forward - price_expected)
gen forward_premium_relative= (price_forward - price_expected)/price_expected

label variable skew_bl "Skewness"
label variable demand_expected "Mean Demand"
label variable forward_premium "Forward Premium"
label variable sd_demand "SD Demand"
label variable varprice "VAR Price"
label variable sd_demand "SD Demand"

gen retail_rate2 = price_expected * 1.2
cap drop skew_bl2
* create the non-normalized skew variable
gen skew_bl2 = skew * (varprice^(3/2))
gen r2=retail_rate/price_expected

cap drop a_check_flex a_check_fix
gen a_check_flex = 30*(np/demand_expected)^(c-1) if amethod==0
gen a_check_fix = 30*(np/100)^(c-1) if amethod==1

******************************
* checking data *
******************************
tab amethod
bysort amethod c:tab negative_omitted 
*****************************

* check that forward positions of the retailer are the same (but with opposite sign) to the producer 
* (using the BL formula)
reg ooptforwposition_pr ooptforwposition_re
* check that Paretp-Optimal forward positions of the producer are the same as the ones using the BL formula
reg ooptforwposition_pr ooptforwposition_pr_computed




********************************************************
*                2D diagrams adressing Hypothesis 3 and 4
********************************************************
set more off
graph drop _all
* Figure 1: Simulation addressing the hypotheses

display "Hypothesis 3"
local no=1
*forvalues no = 1(1)1 {
	forvalues cc = 2(1)5 {	
	
	
		local ww=0

// 		if negative_omitted==1 display "True 1"
// 		if negative_omitted==0 display "True 0"
		display `cc'
		
		
		if (`no'==1) {
	local aname = "Figure1_H3"
}
 else {
	local aname = "Figure1A_H3_zeros"
 }  
			
********************************************************
*                Hypothesis 3
		twoway 	(scatter forward_premium sd_demand if demand_expected==50) ///
			(scatter forward_premium sd_demand if demand_expected ==75) ///
			(scatter forward_premium sd_demand if demand_expected ==100) ///
			(scatter forward_premium sd_demand if demand_expected ==125) ///
			(scatter forward_premium sd_demand if demand_expected ==150) ///
			if negative_omitted==`no' & c==`cc', ///
			ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(, labsize(20-pt)) ///
			ysize(5) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(7)) ///	
			name("`aname'_c`cc'") legend(off) scale(1.4) graphregion(margin(-4 4 0 4))	  
// 			legend(on order(1 "50" 2 "75" 3 "100" 4 "125" 5 "150") ///
// 			cols(5) nocolfirst  subtitle("Levels of Mean Demand", size(14-pt)) size(14-pt)) scale(1.5) ///
				
	}

	
	* Generate the legend
	local no=1
	local cc=2
	local aname=""
	twoway 	(scatter forward_premium sd_demand if demand_expected==50) ///
			(scatter forward_premium sd_demand if demand_expected ==75) ///
			(scatter forward_premium sd_demand if demand_expected ==100) ///
			(scatter forward_premium sd_demand if demand_expected ==125) ///
			(scatter forward_premium sd_demand if demand_expected ==150) ///
			if negative_omitted==`no' & c==`cc', ///
			ylabel(#4, labsize(12-pt)) ///
			xlabel(, labsize(12-pt)) ///
			xsize(16) ///
			ytitle(, size (12-pt)) ///
			xtitle(, size (12-pt)) ///	
			legend(on order(1 "50" 2 "75" 3 "100" 4 "125" 5 "150") region(lwidth(none))  ///
			cols(5) nocolfirst  subtitle("Levels of Mean Demand", size(14-pt)) size(14-pt)) scale(3) 
			
// 			name("`aname'_legend_c`cc'")	
	
	
	
**********************************************************	
local no=1
display "Hypothesis 4"
// forvalues no = 1(-1)0 {
	forvalues cc = 2(1)5 {	

		local ww=0
		display `cc'
		
		
		if (`no'==1) {
	local aname = "Figure1_H4"
}
 else {
	local aname = "Figure1A_H4_zeros"
 }  

********************************************************
*                Hypothesis 4
		twoway 	(scatter forward_premium demand_expected if sd_demand==5) ///
			(scatter forward_premium demand_expected if sd_demand==15) ///
			(scatter forward_premium demand_expected if sd_demand==25) ///
			(scatter forward_premium demand_expected if sd_demand==35) ///
			if negative_omitted==`no' &  c==`cc', ///
			ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(, labsize(20-pt)) ///
			ysize(5) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(7)) ///		
			name("`aname'_c`cc'") legend(off) scale(1.4)graphregion(margin(-4 4 0 4))	 
			// 			legend(on order(1 "5" 2 "15" 3 "25" 4 "35") ///
// 			cols(4) nocolfirst  subtitle("Levels of SD Demand", size(14-pt)) size(14-pt))

	}


	
*   Generate the legend
	local no=1
	local cc=2
	local aname=""
		twoway 	(scatter forward_premium demand_expected if sd_demand==5) ///
			(scatter forward_premium demand_expected if sd_demand==15) ///
			(scatter forward_premium demand_expected if sd_demand==25) ///
			(scatter forward_premium demand_expected if sd_demand==35) ///
			if negative_omitted==`no' &  c==`cc', ///
			ylabel(#4, labsize(12-pt)) ///
			xlabel(, labsize(12-pt)) ///
			xsize(16) ///
			ytitle(, size (12-pt)) ///
			xtitle(, size (12-pt)) ///	
			legend(on order(1 "5" 2 "15" 3 "25" 4 "35") region(lwidth(none))  ///
			cols(4) nocolfirst  subtitle("Levels of SD Demand", size(14-pt)) size(14-pt)) scale(3) ///
// 			name("`aname'_c`cc'") 
	
	


****************************************************************************************************************
*                2D diagrams adressing Hypothesis 1 and 2 
****************************************************************************************************************

// forvalues no = 1(-1)0 {
********************************************************
*                Hypothesis 2

********* c2 *********************************

set more off
local cc=2
local ww=0
local no=1

cap drop varcat2
egen varcat2= cut(varprice) if negative_omitted==`no' &c==`cc', at(2,2.02,10,11,50,50.5,75,76,100,100.8,125,126,150,150.5,175,175.5,200,201,500000)
table varcat2 if negative_omitted==`no' &c==`cc'

if (`no'==1) {
	local aname = "Figure1_H2"
}
 else {
	local aname = "Figure1A_H2_zeros"
 }  
************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat2==10) ///
		(scatter forward_premium skew_bl if varcat2==50) ///
		(scatter forward_premium skew_bl if varcat2==75) ///	
				(scatter forward_premium skew_bl if varcat2==100) ///	
						(scatter forward_premium skew_bl if varcat2==125) ///	
		if negative_omitted==`no' & c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(6)) ///
			xtitle(, size (20-pt) height(6)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
		legend(on order(1 "10" 2 "50" 3 "75" 4 "100" 5 "125") ///
		cols(5) nocolfirst subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4)  ///
		name("`aname'_c`cc'") graphregion(margin(-4 5 0 0))	  


********* c3 *********************************
local cc=3
local no=1
cap drop varcat3
cap egen varcat3 = cut(varprice) if negative_omitted==`no' &c==`cc' , at(1,1.1,50,50.5,100,101,200,202,250,252.5,400,404,500,502.5,600,603,800,802,1000,1005,10000,10050,50000) 
table varcat3 if negative_omitted==`no' &c==`cc' 

************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat3==100) ///
		(scatter forward_premium skew_bl if varcat3==250) ///
		(scatter forward_premium skew_bl if varcat3==600) ///		
		(scatter forward_premium skew_bl if varcat3==1000) ///	
		if negative_omitted==`no'  &  c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(5)) ///
			xtitle(, size (20-pt) height(6)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
		legend(on order(1 "100" ///
		2 "250" 3 "600" 4 "1K" )  ///
		cols(4) nocolfirst  subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4) ///
		name("`aname'_c`cc'")	graphregion(margin(-4 6 0 0))		


********** c4 *********************************
local cc=4
local ww=0
local no=1

cap drop varcat4
cap egen varcat4 = cut(varprice) if negative_omitted==`no' &c==`cc', at(10,10.5,100,100.5,500,502.5,1000,1005,1500,1507.5,2000,2010, 2500,2512.5,3000,3015,3500,3517.5,4000,4020)
table varcat4 if negative_omitted==`no' &c==`cc' 
************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat4==1000) ///
		(scatter forward_premium skew_bl if varcat4==2000) ///
		(scatter forward_premium skew_bl if varcat4==3000) ///
		(scatter forward_premium skew_bl if varcat4==4000) ///	
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(18)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "1K" ///
		2 "2K" 3 "3K" 4 "4K") colgap(25) region(margin(1 6 0 0))  ///
		cols(4) nocolfirst  subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4)   ///
		graphregion(margin(-4 10 0 3))	
		/// name("`aname'_c`cc'")

		
********** c5 *********************************
local cc=5
local ww=0
local no=1

cap drop varcat5
cap egen varcat5 = cut(varprice)  if negative_omitted==`no' &c==`cc' , at(1,10,12,500,550,1000,1005,2000,2020,2500,2525,5000,5050,10000,10050,15000,15150,20000,20100,99999999)
table varcat5 if negative_omitted==`no' &c==`cc' 
*twoway (scatter forward_premium skew_bl ) if amethod==0 & tf==1 & c==3, by(varcat3, style(combine))

************************ weighted==0 ****************************
twoway  (scatter forward_premium skew_bl if varcat5==5000) ///
		(scatter forward_premium skew_bl if varcat5==10000) ///
		(scatter forward_premium skew_bl if varcat5==15000) ///
		(scatter forward_premium skew_bl if varcat5==20000) ///
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(0 5000000, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(10)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend( keygap(1)  on order( ///
		1 "5K" 2 "10K" 3 "15K" 4 "20K") colgap(10) region(margin(1 3 0 0) ) bmargin(zero) ///
		cols(4) nocolfirst subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4)  ///
		name("`aname'_c`cc'") graphregion(margin(-4 4 1 1))	





********************************************************
*                Hypothesis 1


// forvalues no = 1(-1)0 {

********** c2 *********************************

local cc=2
local ww=0
local no=1
if (`no'==1) {
	local aname = "Figure1_H1"
}
 else {
	local aname = "Figure1A_H1_zeros"
 } 


summ skew_bl if negative_omitted==`no' &c==`cc' 
cap drop skew_blcat2
cap egen skew_blcat2 = cut(skew_bl) if negative_omitted==`no' &c==`cc' , at(2,2.075,5,5.2,10,10.2,20,20.5,50,51,100,102,250,255,500,510,750,765,1000,1020,1021,500000)
summ skew_blcat2 

table skew_blcat2
************************ c2 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat2==20) ///					
		(scatter forward_premium varprice if skew_blcat2==250) ///
		(scatter forward_premium varprice if skew_blcat2==500) ///	
		(scatter forward_premium varprice if skew_blcat2==750) ///		
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(6)) ///
			xtitle(, size (20-pt) height(6)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "20" 2 "250" 3 "500" 4 "750") bmargin(zero) ///
cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4) ///
		name("`aname'_c`cc'")	 graphregion(margin(-4 5 0 0))	


********** c3 *********************************
local cc=3
local ww=0
local no=1


cap drop skew_blcat3
cap egen skew_blcat3 = cut(skew_bl) if negative_omitted==`no' &c==`cc' , at(10,10.25,100,102.5,500,512.5,1000,1025,2000,2050,5000,5125,10000,10250,20000,20250,25000,25250,30000,30250,40000,40250,50000,51250,70000,70250,100000,102500,1000000,2000000)
table skew_blcat3 if negative_omitted==`no' &c==`cc' 
************************ c3 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat3==500) ///		
	(scatter forward_premium varprice if skew_blcat3==5000) ///	
		(scatter forward_premium varprice if skew_blcat3==10000) ///
		(scatter forward_premium varprice if skew_blcat3==25000) ///
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "500" ///
		2 "5K" 3 "10K" 4 "25K") bmargin(zero) ///
		cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4)  ///
		name("`aname'_c`cc'")	graphregion(margin(-4 1 1 1))	
	
		

********** c4 *********************************
local am=0
local cc=4
local ww=0	
local no=1


cap drop skew_blcat4
cap egen skew_blcat4 = cut(skew_bl) if negative_omitted==`no' &c==`cc', at(1,1.001,50,50.05,100,100.1,500,502.5,1000,1005,2000,2010,5000,5050,20000,20200,50000,50500,100000,101000,200000,202000,300000,303000,400000,404000, 500000)
table skew_blcat4 
************************ c4 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat4==5000) ///
		(scatter forward_premium varprice if skew_blcat4==50000) ///		
		(scatter forward_premium varprice if skew_blcat4==200000) ///		
		(scatter forward_premium varprice if skew_blcat4==400000) ///		
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(keygap(1) on order(1 "5K" ///
		2 "50K" 3 "200K" 4 "400K")  bmargin(zero) region(margin(2 10 0 0)) ///
		cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4) ///
		name("`aname'_c`cc'")		graphregion(margin(-3 5 -2 2))	
	


********** c5 *********************************
local cc=5
local ww=0
local no=1

 
cap drop skew_blcat5
cap egen skew_blcat5 = cut(skew_bl) if negative_omitted==`no' &c==`cc', at(10,10.1,50,51,100,101,1000,1010,5000,5050,10000,10050,50000,50150,100000,102000,200000,207500,500000,510000,1000000,1020000,2000000,2030000,10000000)
summ skew_bl if negative_omitted==`no' &c==`cc' 
table skew_blcat5 if negative_omitted==`no' &c==`cc' 
************************ c5 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat5==10000) ///		
		(scatter forward_premium varprice if skew_blcat5==200000) ///
		(scatter forward_premium varprice if skew_blcat5==1000000) ///
		(scatter forward_premium varprice if skew_blcat5==2000000) ///
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(6)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "10K" 2 "200K" ///
		3 "1M" 4 "2M")  bmargin(zero) ///
		cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4) /// 
		name("`aname'_c`cc'")	graphregion(margin(-3 7 -2 0))	
*}






















********************************************************************
********************************************************************
********************************************************************
********************************************************************
********************************************************************
********************************************************************
********************************************************************
********************************************************************
********************************************************************
********************************************************************
*  same as above, but in black and white
********************************************************************
********************************************************************
set scheme s2mono





********************************************************
*                2D diagrams adressing Hypothesis 3 and 4
********************************************************
set more off
graph drop _all
* Figure 1: Simulation addressing the hypotheses

display "Hypothesis 3"
local no=1
*forvalues no = 1(1)1 {
	forvalues cc = 2(1)5 {	
	
	
		local ww=0

// 		if negative_omitted==1 display "True 1"
// 		if negative_omitted==0 display "True 0"
		display `cc'
		
		
		if (`no'==1) {
	local aname = "Figure1_H3"
}
 else {
	local aname = "Figure1A_H3_zeros"
 }  
			
********************************************************
*                Hypothesis 3
		twoway 	(line forward_premium sd_demand if demand_expected==50, sort  lwidth(vthick) lpattern(solid)) ///
			(line forward_premium sd_demand if demand_expected ==75, sort  lwidth(vthick) lpattern(longdash)) ///
			(line forward_premium sd_demand if demand_expected ==100, sort  lwidth(vthick) lpattern(dash)) ///
			(line forward_premium sd_demand if demand_expected ==125, sort lwidth(vthick)  lpattern(longdash_dot)) ///
			(line forward_premium sd_demand if demand_expected ==150, sort lwidth(vthick) lpattern(dash_3dot)) ///
			if negative_omitted==`no' & c==`cc', ///
			ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(, labsize(20-pt)) ///
			ysize(5) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(7)) ///	
			name("`aname'_c`cc'") legend(off) scale(1.4) graphregion(margin(-4 4 0 4))	  
// 			legend(on order(1 "50" 2 "75" 3 "100" 4 "125" 5 "150") ///
// 			cols(5) nocolfirst  subtitle("Levels of Mean Demand", size(14-pt)) size(14-pt)) scale(1.5) ///
				
	}

	
	* Generate the legend
	local no=1
	local cc=2
	local aname=""
		twoway 	(line forward_premium sd_demand if demand_expected==50, sort  lwidth(vthick) lpattern(solid)) ///
			(line forward_premium sd_demand if demand_expected ==75, sort  lwidth(vthick) lpattern(longdash)) ///
			(line forward_premium sd_demand if demand_expected ==100, sort  lwidth(vthick) lpattern(dash)) ///
			(line forward_premium sd_demand if demand_expected ==125, sort lwidth(vthick)  lpattern(longdash_dot)) ///
			(line forward_premium sd_demand if demand_expected ==150, sort lwidth(vthick) lpattern(dash_3dot)) ///
			if negative_omitted==`no' & c==`cc', ///
			ylabel(#4, labsize(12-pt)) ///
			xlabel(, labsize(12-pt)) ///
			xsize(16) ///
			ytitle(, size (12-pt)) ///
			xtitle(, size (12-pt)) ///	
			legend(on order(1 "50" 2 "75" 3 "100" 4 "125" 5 "150") region(lwidth(none))  ///
			cols(5) nocolfirst  subtitle("Levels of Mean Demand", size(14-pt)) size(14-pt)) scale(3) 
			
// 			name("`aname'_legend_c`cc'")	
	
	
	
**********************************************************	
local no=1
display "Hypothesis 4"
// forvalues no = 1(-1)0 {
	forvalues cc = 2(1)5 {	

		local ww=0
		display `cc'
		
		
		if (`no'==1) {
	local aname = "Figure1_H4"
}
 else {
	local aname = "Figure1A_H4_zeros"
 }  

********************************************************
*                Hypothesis 4
		twoway 	(line forward_premium demand_expected if sd_demand==5, sort  lwidth(vthick) lpattern(solid)) ///
			(line forward_premium demand_expected if sd_demand ==15, sort  lwidth(vthick) lpattern(longdash)) ///
			(line forward_premium demand_expected if sd_demand ==25, sort  lwidth(vthick) lpattern(dash)) ///
			(line forward_premium demand_expected if sd_demand ==35, sort lwidth(vthick)  lpattern(longdash_dot)) ///
			if negative_omitted==`no' &  c==`cc', ///
			ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(, labsize(20-pt)) ///
			ysize(5) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(7)) ///		
			name("`aname'_c`cc'") legend(off) scale(1.4)graphregion(margin(-4 4 0 4))	 
			// 			legend(on order(1 "5" 2 "15" 3 "25" 4 "35") ///
// 			cols(4) nocolfirst  subtitle("Levels of SD Demand", size(14-pt)) size(14-pt))

	}


	
*   Generate the legend
	local no=1
	local cc=2
	local aname=""
		twoway 	(line forward_premium demand_expected if sd_demand==5, sort  lwidth(vthick) lpattern(solid)) ///
			(line forward_premium demand_expected if sd_demand ==15, sort  lwidth(vthick) lpattern(longdash)) ///
			(line forward_premium demand_expected if sd_demand ==25, sort  lwidth(vthick) lpattern(dash)) ///
			(line forward_premium demand_expected if sd_demand ==35, sort lwidth(vthick)  lpattern(longdash_dot)) ///
			if negative_omitted==`no' &  c==`cc', ///
			ylabel(#4, labsize(12-pt)) ///
			xlabel(, labsize(12-pt)) ///
			xsize(16) ///
			ytitle(, size (12-pt)) ///
			xtitle(, size (12-pt)) ///	
			legend(on order(1 "5" 2 "15" 3 "25" 4 "35") region(lwidth(none))  ///
			cols(4) nocolfirst  subtitle("Levels of SD Demand", size(14-pt)) size(14-pt)) scale(3) ///
// 			name("`aname'_c`cc'") 
	
	


****************************************************************************************************************
*                2D diagrams adressing Hypothesis 1 and 2 
****************************************************************************************************************

// forvalues no = 1(-1)0 {
********************************************************
*                Hypothesis 2

********* c2 *********************************

set more off
local cc=2
local ww=0
local no=1

cap drop varcat2
egen varcat2= cut(varprice) if negative_omitted==`no' &c==`cc', at(2,2.02,10,11,50,50.5,75,76,100,100.8,125,126,150,150.5,175,175.5,200,201,500000)
table varcat2 if negative_omitted==`no' &c==`cc'

if (`no'==1) {
	local aname = "Figure1_H2"
}
 else {
	local aname = "Figure1A_H2_zeros"
 }  
************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat2==10) ///
		(scatter forward_premium skew_bl if varcat2==50) ///
		(scatter forward_premium skew_bl if varcat2==75) ///	
				(scatter forward_premium skew_bl if varcat2==100) ///	
						(scatter forward_premium skew_bl if varcat2==125) ///	
		if negative_omitted==`no' & c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(6)) ///
			xtitle(, size (20-pt) height(6)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
		legend(on order(1 "10" 2 "50" 3 "75" 4 "100" 5 "125") ///
		cols(5) nocolfirst subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4)  ///
		name("`aname'_c`cc'") graphregion(margin(-4 5 0 0))	  


********* c3 *********************************
local cc=3
local no=1
cap drop varcat3
cap egen varcat3 = cut(varprice) if negative_omitted==`no' &c==`cc' , at(1,1.1,50,50.5,100,101,200,202,250,252.5,400,404,500,502.5,600,603,800,802,1000,1005,10000,10050,50000) 
table varcat3 if negative_omitted==`no' &c==`cc' 

************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat3==100) ///
		(scatter forward_premium skew_bl if varcat3==250) ///
		(scatter forward_premium skew_bl if varcat3==600) ///		
		(scatter forward_premium skew_bl if varcat3==1000) ///	
		if negative_omitted==`no'  &  c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(5)) ///
			xtitle(, size (20-pt) height(6)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
		legend(on order(1 "100" ///
		2 "250" 3 "600" 4 "1K" )  ///
		cols(4) nocolfirst  subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4) ///
		name("`aname'_c`cc'")	graphregion(margin(-4 6 0 0))		


********** c4 *********************************
local cc=4
local ww=0
local no=1

cap drop varcat4
cap egen varcat4 = cut(varprice) if negative_omitted==`no' &c==`cc', at(10,10.5,100,100.5,500,502.5,1000,1005,1500,1507.5,2000,2010, 2500,2512.5,3000,3015,3500,3517.5,4000,4020)
table varcat4 if negative_omitted==`no' &c==`cc' 
************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat4==1000) ///
		(scatter forward_premium skew_bl if varcat4==2000) ///
		(scatter forward_premium skew_bl if varcat4==3000) ///
		(scatter forward_premium skew_bl if varcat4==4000) ///	
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(18)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "1K" ///
		2 "2K" 3 "3K" 4 "4K") colgap(25) region(margin(1 6 0 0))  ///
		cols(4) nocolfirst  subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4)   ///
		graphregion(margin(-4 10 0 3))	
		/// name("`aname'_c`cc'")

		
********** c5 *********************************
local cc=5
local ww=0
local no=1

cap drop varcat5
cap egen varcat5 = cut(varprice)  if negative_omitted==`no' &c==`cc' , at(1,10,12,500,550,1000,1005,2000,2020,2500,2525,5000,5050,10000,10050,15000,15150,20000,20100,99999999)
table varcat5 if negative_omitted==`no' &c==`cc' 
*twoway (scatter forward_premium skew_bl ) if amethod==0 & tf==1 & c==3, by(varcat3, style(combine))

************************ weighted==0 ****************************
twoway  (scatter forward_premium skew_bl if varcat5==5000) ///
		(scatter forward_premium skew_bl if varcat5==10000) ///
		(scatter forward_premium skew_bl if varcat5==15000) ///
		(scatter forward_premium skew_bl if varcat5==20000) ///
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(0 5000000, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(10)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend( keygap(1)  on order( ///
		1 "5K" 2 "10K" 3 "15K" 4 "20K") colgap(10) region(margin(1 3 0 0) ) bmargin(zero) ///
		cols(4) nocolfirst subtitle("Levels of Variance:", size(20-pt)) size(20-pt)) scale(1.4)  ///
		name("`aname'_c`cc'") graphregion(margin(-4 4 1 1))	





********************************************************
*                Hypothesis 1


// forvalues no = 1(-1)0 {

********** c2 *********************************

local cc=2
local ww=0
local no=1
if (`no'==1) {
	local aname = "Figure1_H1"
}
 else {
	local aname = "Figure1A_H1_zeros"
 } 


summ skew_bl if negative_omitted==`no' &c==`cc' 
cap drop skew_blcat2
cap egen skew_blcat2 = cut(skew_bl) if negative_omitted==`no' &c==`cc' , at(2,2.075,5,5.2,10,10.2,20,20.5,50,51,100,102,250,255,500,510,750,765,1000,1020,1021,500000)
summ skew_blcat2 

table skew_blcat2
************************ c2 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat2==20) ///					
		(scatter forward_premium varprice if skew_blcat2==250) ///
		(scatter forward_premium varprice if skew_blcat2==500) ///	
		(scatter forward_premium varprice if skew_blcat2==750) ///		
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(6)) ///
			xtitle(, size (20-pt) height(6)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "20" 2 "250" 3 "500" 4 "750") bmargin(zero) ///
cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4) ///
		name("`aname'_c`cc'")	 graphregion(margin(-4 5 0 0))	


********** c3 *********************************
local cc=3
local ww=0
local no=1


cap drop skew_blcat3
cap egen skew_blcat3 = cut(skew_bl) if negative_omitted==`no' &c==`cc' , at(10,10.25,100,102.5,500,512.5,1000,1025,2000,2050,5000,5125,10000,10250,20000,20250,25000,25250,30000,30250,40000,40250,50000,51250,70000,70250,100000,102500,1000000,2000000)
table skew_blcat3 if negative_omitted==`no' &c==`cc' 
************************ c3 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat3==500) ///		
	(scatter forward_premium varprice if skew_blcat3==5000) ///	
		(scatter forward_premium varprice if skew_blcat3==10000) ///
		(scatter forward_premium varprice if skew_blcat3==25000) ///
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "500" ///
		2 "5K" 3 "10K" 4 "25K") bmargin(zero) ///
		cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4)  ///
		name("`aname'_c`cc'")	graphregion(margin(-4 1 1 1))	
	
		

********** c4 *********************************
local am=0
local cc=4
local ww=0	
local no=1


cap drop skew_blcat4
cap egen skew_blcat4 = cut(skew_bl) if negative_omitted==`no' &c==`cc', at(1,1.001,50,50.05,100,100.1,500,502.5,1000,1005,2000,2010,5000,5050,20000,20200,50000,50500,100000,101000,200000,202000,300000,303000,400000,404000, 500000)
table skew_blcat4 
************************ c4 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat4==5000) ///
		(scatter forward_premium varprice if skew_blcat4==50000) ///		
		(scatter forward_premium varprice if skew_blcat4==200000) ///		
		(scatter forward_premium varprice if skew_blcat4==400000) ///		
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(7)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(keygap(1) on order(1 "5K" ///
		2 "50K" 3 "200K" 4 "400K")  bmargin(zero) region(margin(2 10 0 0)) ///
		cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4) ///
		name("`aname'_c`cc'")		graphregion(margin(-3 5 -2 2))	
	


********** c5 *********************************
local cc=5
local ww=0
local no=1

 
cap drop skew_blcat5
cap egen skew_blcat5 = cut(skew_bl) if negative_omitted==`no' &c==`cc', at(10,10.1,50,51,100,101,1000,1010,5000,5050,10000,10050,50000,50150,100000,102000,200000,207500,500000,510000,1000000,1020000,2000000,2030000,10000000)
summ skew_bl if negative_omitted==`no' &c==`cc' 
table skew_blcat5 if negative_omitted==`no' &c==`cc' 
************************ c5 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat5==10000) ///		
		(scatter forward_premium varprice if skew_blcat5==200000) ///
		(scatter forward_premium varprice if skew_blcat5==1000000) ///
		(scatter forward_premium varprice if skew_blcat5==2000000) ///
		if negative_omitted==`no' &c==`cc', ///
		ylabel(#3, labsize(20-pt) labgap(quarter_tiny))  ///
			xlabel(#3, labsize(20-pt)  labgap(quarter_tiny)) ///
			ytitle(, size (20-pt) height(6)) ///
			xtitle(, size (20-pt) height(5)) ///	
			ysize(5.8) ///
			xsize(6.1) ///
			legend(on order(1 "10K" 2 "200K" ///
		3 "1M" 4 "2M")  bmargin(zero) ///
		cols(4) nocolfirst subtitle("Levels of Skewness:", size(20-pt)) size(20-pt)) scale(1.4) /// 
		name("`aname'_c`cc'")	graphregion(margin(-3 7 -2 0))	
*}





