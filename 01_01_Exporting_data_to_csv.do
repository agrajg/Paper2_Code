clear all
set more off
clear all 
set more off
set maxvar 120000, perm
set matsize 11000, perm
capture timer clear
*===============================================================================
use "Y:\agrajg\Research\Paper1_demand_stata\04_07_Price_regression_ready_data.dta", clear
export delimited using "Y:\agrajg\Research\Paper2\Output\TEMP\01_01_Price_Regression_data.csv", quote replace
*===============================================================================
use "Y:\agrajg\Research\Paper1_demand_stata\03_02_Regression_Data.dta" , clear
export delimited using "Y:\agrajg\Research\Paper2\Output\TEMP\01_01_Demand_Regression_data.csv", quote replace
*===============================================================================
use "Y:\agrajg\Research\Data\RawMCOX_reviews_dta_data\reviews_all.dta", clear
export delimited using "Y:\agrajg\Research\Paper2\Output\TEMP\01_01_Reviews_data.csv", quote replace
*===============================================================================
