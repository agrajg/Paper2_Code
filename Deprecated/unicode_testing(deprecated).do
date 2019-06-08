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
cd "$PROJECT_PATH\Output\TEMP\"
unicode analyze "02_01_Reviews_data_plain.dta"
unicode encoding set "ASCII"
unicode retranslate "02_01_Reviews_data_plain.dta", transutf8 invalid(ignore)
