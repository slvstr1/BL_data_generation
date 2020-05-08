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

label variable skew_bl "Non-standardized Skewness"
label variable demand_expected "mean demand"


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
			if negative_omitted==`no' & c==`cc', legend(on order(1 "50" 2 "75" 3 "100" 4 "125" 5 "150") ///
			cols(5) nocolfirst  subtitle("Levels of mean demand") ) name("`aname'_c`cc'")
			
	}


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
			if negative_omitted==`no' &  c==`cc', legend(on order(1 "5" 2 "15" 3 "25" 4 "35") ///
			cols(4) nocolfirst  subtitle("Levels of demand sd") ) name("`aname'_c`cc'")
	}





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

cap drop varcat2
egen varcat2= cut(varprice) if negative_omitted==`no' &c==`cc', at(2,2.02,10,11,50,50.5,100,101,150,150.5,175,175.5,200,201,500000)
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
		(scatter forward_premium skew_bl if varcat2==100) ///	
		(scatter forward_premium skew_bl if varcat2==150) ///		
		(scatter forward_premium skew_bl if varcat2==175) ///				
		(scatter forward_premium skew_bl if varcat2==200) ///				
		if negative_omitted==`no' & c==`cc', legend(on order(1 "10" 2 "50" ///
		3 "100"  4 "150" 5 "175" 6 "200") cols(6) nocolfirst subtitle("Levels of variance:") )  name("`aname'_c`cc'")	  


********* c3 *********************************
local cc=3
 
cap drop varcat3
cap egen varcat3 = cut(varprice) if negative_omitted==`no' &c==`cc' , at(1,1.1,50,50.5,100,101,200,202,400,404,600,603,800,802,1000,1005,10000,10050,50000) 
table varcat3 if negative_omitted==`no' &c==`cc' 

************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat3==50) ///|
		(scatter forward_premium skew_bl if varcat3==100) ///
		(scatter forward_premium skew_bl if varcat3==200) ///
		(scatter forward_premium skew_bl if varcat3==400) ///		
		(scatter forward_premium skew_bl if varcat3==600) ///		
		(scatter forward_premium skew_bl if varcat3==800) ///	
		(scatter forward_premium skew_bl if varcat3==1000) ///	
		if negative_omitted==`no'  &  c==`cc', legend(on order(1 "50" 2 "100" ///
		3 "200" 4 "400" 5 "600" 6 "800" 7 "1000") cols(7) nocolfirst  subtitle("Levels of variance:") ) name("`aname'_c`cc'")		


********** c4 *********************************
local cc=4
local ww=0

cap drop varcat4
cap egen varcat4 = cut(varprice) if negative_omitted==`no' &c==`cc', at(10,10.5,100,100.5,500,502.5,1000,1005,1500,1507.5,2000,2010, 2500,2512.5,3000,3015,3500,3517.5,4000,4020)
table varcat4 if negative_omitted==`no' &c==`cc' 
************************ weighted==0 ****************************
twoway 	(scatter forward_premium skew_bl if varcat4==100) ///
		(scatter forward_premium skew_bl if varcat4==500) ///
		(scatter forward_premium skew_bl if varcat4==1000) ///
		(scatter forward_premium skew_bl if varcat4==1500) ///
		(scatter forward_premium skew_bl if varcat4==2000) ///
		(scatter forward_premium skew_bl if varcat4==2500) ///
		(scatter forward_premium skew_bl if varcat4==3000) ///
		(scatter forward_premium skew_bl if varcat4==3500) ///
		(scatter forward_premium skew_bl if varcat4==4000) ///	
		if negative_omitted==`no' &c==`cc', legend(on order(1 "100" 2 "500" 3 "1000" ///
		4 "1500" 5 "2000" 6 "2500" 7 "3000" 8 "3500" 9 "4000") cols(9) nocolfirst  subtitle("Levels of variance:") )   name("`aname'_c`cc'")

		
********** c5 *********************************
local cc=5
local ww=0	

cap drop varcat5
cap egen varcat5 = cut(varprice)  if negative_omitted==`no' &c==`cc' , at(1,10,12,500,550,1000,1005,2000,2020,2500,2525,5000,5050,10000,10050,15000,15150,20000,20100,99999999)
table varcat5 if negative_omitted==`no' &c==`cc' 
*twoway (scatter forward_premium skew_bl ) if amethod==0 & tf==1 & c==3, by(varcat3, style(combine))

************************ weighted==0 ****************************
twoway  (scatter forward_premium skew_bl if varcat5==2000) ///
		(scatter forward_premium skew_bl if varcat5==5000) ///
		(scatter forward_premium skew_bl if varcat5==10000) ///
		(scatter forward_premium skew_bl if varcat5==15000) ///
		(scatter forward_premium skew_bl if varcat5==20000) ///
		if negative_omitted==`no' &c==`cc' , legend(on order(1 "2000" ///
		2 "5000" 3 "10 000" 4 "15 000" 5 "20 000") cols(5) nocolfirst subtitle("Levels of variance:") )  name("`aname'_c`cc'")





********************************************************
*                Hypothesis 1


// forvalues no = 1(-1)0 {

********** c2 *********************************
local cc=2
local ww=0

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
		(scatter forward_premium varprice if skew_blcat2==100) ///			
		(scatter forward_premium varprice if skew_blcat2==250) ///
		(scatter forward_premium varprice if skew_blcat2==500) ///	
		(scatter forward_premium varprice if skew_blcat2==750) ///	
		(scatter forward_premium varprice if skew_blcat2==1000) ///	
		if negative_omitted==`no' &c==`cc' , legend(on order(1 "20" 2 "100" 3 "250" 4 "500" 5 "750" 6 "1000" ///
		) cols(6) nocolfirst subtitle("Levels of non-normalized Skewness:") ) name("`aname'_c`cc'")	


********** c3 *********************************
local cc=3
local ww=0


cap drop skew_blcat3
cap egen skew_blcat3 = cut(skew_bl) if negative_omitted==`no' &c==`cc' , at(10,10.25,100,102.5,500,512.5,1000,1025,2000,2050,5000,5125,10000,10250,20000,20250,25000,25250,30000,30250,40000,40250,50000,51250,70000,70250,100000,102500,1000000,2000000)
table skew_blcat3 if negative_omitted==`no' &c==`cc' 
************************ c3 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat3==10) ///
		(scatter forward_premium varprice if skew_blcat3==100) ///		
		(scatter forward_premium varprice if skew_blcat3==500) ///		
		(scatter forward_premium varprice if skew_blcat3==1000) ///
		(scatter forward_premium varprice if skew_blcat3==2000) ///
		(scatter forward_premium varprice if skew_blcat3==5000) ///	
		(scatter forward_premium varprice if skew_blcat3==10000) ///
		(scatter forward_premium varprice if skew_blcat3==25000) ///
		if negative_omitted==`no' &c==`cc' , legend(on order(1 "10" 2 "100" 3 "500" 4 "1 000" ///
		5 "2 000" 6 "5 000" 7 "10 000" 8 "25 000") cols(8) nocolfirst subtitle("Levels of non-normalized Skewness:") )  name("`aname'_c`cc'")	
	
		

********** c4 *********************************
local am=0
local cc=4
local ww=0	


cap drop skew_blcat4
cap egen skew_blcat4 = cut(skew_bl) if negative_omitted==`no' &c==`cc', at(1,1.001,50,50.05,100,100.1,500,502.5,1000,1005,2000,2010,5000,5050,20000,20200,50000,50500,100000,101000,200000,202000,300000,303000,400000,404000, 500000)
table skew_blcat4 
************************ c4 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat4==1000) ///
		(scatter forward_premium varprice if skew_blcat4==5000) ///
		(scatter forward_premium varprice if skew_blcat4==20000) ///			
		(scatter forward_premium varprice if skew_blcat4==50000) ///		
		(scatter forward_premium varprice if skew_blcat4==100000) ///		
		(scatter forward_premium varprice if skew_blcat4==200000) ///		
		(scatter forward_premium varprice if skew_blcat4==300000) ///		
		(scatter forward_premium varprice if skew_blcat4==400000) ///		
		if negative_omitted==`no' &c==`cc' , legend(on order(1 "1 000" 2 "5 000" 3 "20 000" ///
		4 "50 000" 5 "100 000" 6 "200 000" 7 "300 000" 8 "400 000") cols(9) nocolfirst subtitle("Levels of non-normalized Skewness:") ) name("`aname'_c`cc'")	
	


********** c5 *********************************
local cc=5
local ww=0
 
cap drop skew_blcat5
cap egen skew_blcat5 = cut(skew_bl) if negative_omitted==`no' &c==`cc', at(10,10.1,50,51,100,101,1000,1010,5000,5050,10000,10050,50000,50150,100000,102000,200000,207500,500000,510000,1000000,1020000,2000000,2030000,10000000)
summ skew_bl if negative_omitted==`no' &c==`cc' 
table skew_blcat5 if negative_omitted==`no' &c==`cc' 
************************ c5 ****************************
twoway 	(scatter forward_premium varprice if skew_blcat5==10000) ///		
		(scatter forward_premium varprice if skew_blcat5==100000) ///
		(scatter forward_premium varprice if skew_blcat5==200000) ///
		(scatter forward_premium varprice if skew_blcat5==500000) ///
		(scatter forward_premium varprice if skew_blcat5==1000000) ///
		(scatter forward_premium varprice if skew_blcat5==2000000) ///
		if negative_omitted==`no' &c==`cc', legend(on order(1 "10 000" 2 "100 000" 3 "200 000" ///
		4 "500 000" 5 "1 000 000" 6 "2 000 000") cols(6) nocolfirst subtitle("Levels of non-normalized Skewness:")) name("`aname'_c`cc'")	
*}