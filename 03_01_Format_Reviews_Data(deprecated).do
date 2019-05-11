clear all
set more off
clear all 
set more off
set maxvar 120000, perm
set matsize 11000, perm
capture timer clear
*-------------------------------------------------------------------------------
do "00_00_Preamble.do"
*-------------------------------------------------------------------------------

*===============================================================================
use "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data.dta", clear
format %20s reviewer_name
format %200s comments


