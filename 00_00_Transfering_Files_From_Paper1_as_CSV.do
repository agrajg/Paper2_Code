clear all
set more off
set maxvar 120000, perm
set matsize 11000, perm
capture timer clear

do "00_00_Preamble.do"
*===============================================================================
use "Y:\agrajg\Research\Paper1_demand_stata\03_02_Regression_Data.dta" , clear
// save "$PROJECT_PATH\Input\00_00_Demand_Regression_data.dta",  replace
export delimited using "$PROJECT_PATH\Input\00_00_Demand_Regression_data.csv", quote replace
*===============================================================================
use "Y:\agrajg\Research\Data\RawMCOX_reviews_dta_data\reviews_all.dta", clear
// save "$PROJECT_PATH\Input\00_00_Reviews_data.dta",  replace

format %20s reviewer_name
format %200s comments

duplicates tag propertyid id date reviewer_id, gen(dupdum)
tab dupdum

gen reviewer_name_len = strlen(reviewer_name)
bys propertyid id date reviewer_id: egen max_reviewer_name_len = max(reviewer_name_len)

gen com_len = strlen(comments)
bys propertyid id date reviewer_id: egen max_com_len = max(com_len)

gsort propertyid id date reviewer_id -com_len -reviewer_name_len
collapse (first) reviewer_name comments dupdum reviewer_name_len /// 
				max_reviewer_name_len com_len max_com_len, /// 
				by (propertyid id date reviewer_id)
tab dupdum
drop dupdum reviewer_name_len max_reviewer_name_len com_len max_com_len

capture drop date_str
rename date date_str
gen date = date(date_str, "YMD")
order date, after(date_str )
format %td date
count if date ==.
drop date_str
order propertyid date reviewer_id id
sort propertyid date reviewer_id id
keep if date <=td(31mar2017) 
save "$PROJECT_PATH\Input\00_00_Reviews_data_plain.dta",  replace
*-------------------------------------------------------------------------------
* Encoding correctly
clear all
cd "$PROJECT_PATH\Input\"
unicode analyze "00_00_Reviews_data_plain.dta"
unicode encoding set "ASCII"
unicode retranslate "00_00_Reviews_data_plain.dta", transutf8 invalid(ignore) replace
cd "$PROJECT_PATH\Code\"
*-------------------------------------------------------------------------------
use "$PROJECT_PATH\Input\00_00_Reviews_data_plain.dta", clear
export delimited using "$PROJECT_PATH\Input\00_00_Reviews_data_plain.csv", quote replace
*===============================================================================
export delimited using "$PROJECT_PATH\Input\00_00_Reviews_data.csv", quote replace
*===============================================================================
use "Y:\agrajg\Research\Paper1_demand_stata\04_07_Price_regression_ready_data.dta", clear
// save "$PROJECT_PATH\Input\00_00_Price_Regression_data.dta",  replace
export delimited using "$PROJECT_PATH\Input\00_00_Price_Regression_data.csv", quote replace
*===============================================================================
