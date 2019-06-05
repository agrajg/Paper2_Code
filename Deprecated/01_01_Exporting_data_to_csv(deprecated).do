clear all
set more off
set maxvar 120000, perm
set matsize 11000, perm
capture timer clear

do "00_00_Preamble.do"
*===============================================================================
use "Y:\agrajg\Research\Paper1_demand_stata\04_07_Price_regression_ready_data.dta", clear
save "$PROJECT_PATH\Output\TEMP\01_01_Price_Regression_data.dta",  replace
export delimited using "$PROJECT_PATH\Output\TEMP\01_01_Price_Regression_data.csv", quote replace
*===============================================================================
use "Y:\agrajg\Research\Paper1_demand_stata\03_02_Regression_Data.dta" , clear
save "$PROJECT_PATH\Output\TEMP\01_01_Demand_Regression_data.dta",  replace
export delimited using "$PROJECT_PATH\Output\TEMP\01_01_Demand_Regression_data.csv", quote replace
contract propertyid date
drop _freq
save "$PROJECT_PATH\Output\TEMP\01_01_rental_time_panel.dta",  replace 
*===============================================================================
use "Y:\agrajg\Research\Data\RawMCOX_reviews_dta_data\reviews_all.dta", clear
save "$PROJECT_PATH\Output\TEMP\01_01_Reviews_data.dta",  replace
export delimited using "$PROJECT_PATH\Output\TEMP\01_01_Reviews_data.csv", quote replace
*===============================================================================
