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
cd "data\data_in_raw_format_gen_in_python"
 
********************************************************
*                MAKE DATA FILES                 *
********************************************************

pwd
clear
clear mata
clear matrix
set more off
set maxvar 10000
cap erase temp.dta

local exp_csv: dir . files "*.csv"

if missing(`exp_csv') { 
display "did you unzip the datafiles?" 
}
else {
display "found these datafiles: "
foreach file of local exp_csv {
display "`file'"
	}
}


foreach file of local exp_csv {
    
	preserve
    import delimited using "`file'",clear
    compress
	save temp,replace
    restore
    append using temp
}
*remove the temporary .dta file
erase temp.dta
compress

*******************************************
* new vars and rename 
*******************************************
label define gen_label 0 "Mathematica" 1 "Python"
gen generated_by =1
label variable generated_by "gen_label"

gen var_demand2=vardemand^2
rename skew_nonstandard_bl skew_bl
rename edapprox demand_expected_est
rename edv demand_expected
rename sdapprox sd_demand_est
rename sd sd_demand

******************************
* checking data *
******************************
*test (391,782 obs per treatment (per c per amethod))
bysort  c:tab amethod 
compress
save "..\data_in_stata_format\exp_data_python", replace






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






* load the data generated in mathematica
* set the working directory to the data
cd "..\data_in_raw_format_gen_in_mathematica"

pwd
clear
clear mata
clear matrix
cap erase temp.dta
local exp_csv: dir . files "Amethod_0*.csv"

if missing(`exp_csv') { 
display "did you unzip the datafiles?" 
}
else {
display "found these datafiles: "
foreach file of local exp_csv {
display "`file'"
	}
}


foreach file of local exp_csv {
    
	preserve
    import delimited using "`file'",clear
	
	gen amethod=0
    
	save temp,replace
    restore
    append using temp
}
save "..\data_in_stata_format\temp0", replace
clear
erase temp.dta
local exp_csv: dir . files "Amethod_1*.csv"
foreach file of local exp_csv {
    
	preserve
    import delimited using "`file'", clear
	
	gen amethod=1
    
	save temp,replace
    restore
    append using temp
}

*remove the temporary .dta file
erase temp.dta
 append using "..\data_in_stata_format\temp0"
compress


*******************************************
* new vars and rename 
*******************************************
label define gen_label 0 "Mathematica" 1 "Python"
gen generated_by =0
label variable generated_by "gen_label"

gen var_demand2=vardemand^2
rename skew_nonstandard_bl skew_bl
rename edapprox demand_expected_est
rename edv demand_expected
rename sdapprox sd_demand_est
rename sd sd_demand

******************************
* checking data *
******************************
*test (195 891 obs per treatment (per c per amethod))
bysort  c:tab amethod 
compress
save "..\data_in_stata_format\exp_data_mathematica", replace
erase "..\data_in_stata_format\temp0.dta"
clear
