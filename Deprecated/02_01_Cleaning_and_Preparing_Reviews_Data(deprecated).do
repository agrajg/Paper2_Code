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
import delimited "$PROJECT_PATH\Output\TEMP\01_01_Reviews_data.csv", delimiter(comma) bindquote(strict) varnames(1) encoding(utf8)
format %20s reviewer_name
format %200s comments

duplicates tag propertyid id date reviewer_id, gen(dupdum)
tab dupdum

gen reviewer_name_len = strlen(reviewer_name)
bys propertyid id date reviewer_id: egen max_reviewer_name_len = max(reviewer_name_len)

gen com_len = strlen(comments)
bys propertyid id date reviewer_id: egen max_com_len = max(com_len)

gsort propertyid id date reviewer_id -com_len -reviewer_name_len
collapse (first) reviewer_name comments dupdum reviewer_name_len max_reviewer_name_len com_len max_com_len, by (propertyid id date reviewer_id)
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
save "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data_plain.dta",  replace

*-------------------------------------------------------------------------------
* Encoding correctly
clear all
cd "$PROJECT_PATH\Output\TEMP\"
unicode analyze "02_01_Reviews_data_plain.dta"
unicode encoding set "ASCII"
unicode retranslate "02_01_Reviews_data_plain.dta", transutf8 invalid(ignore) replace
cd "$PROJECT_PATH\Code\"
*-------------------------------------------------------------------------------
use "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data_plain.dta", clear
export delimited using "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data_plain.csv", quote replace
*===============================================================================
// clear all
// set more off
// clear all 
// set more off
// set maxvar 120000, perm
// set matsize 11000, perm
// capture timer clear
// *-------------------------------------------------------------------------------
// do "00_00_Preamble.do"
// *-------------------------------------------------------------------------------
// use "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data_plain.dta", clear
//
// sort propertyid date
// capture drop corpus
// capture drop num_comments
//
// by propertyid : gen num_comments = 1 if _n==1
// by propertyid : replace num_comments = num_comments[_n-1] + 1 if _n > 1
//
// by propertyid : gen corpus = comments if _n==1
// by propertyid : replace corpus = corpus[_n-1] + " " + comments if _n > 1
// // by propertyid date : replace corpus = comments + " " + corpus[_n-1] if _n > 1
//
// collapse (last) num_comments corpus , by (propertyid date )
// count
// format %200s corpus
// keep if date <=td(31mar2017) 
//
// * Removing spaces , special characters , etc.
// replace corpus = stritrim(corpus)
// replace corpus = strtrim(corpus)
// * Emoticon from https://en.wikipedia.org/wiki/List_of_emoticons
//
// // remove special characters with space or interpretation
//
//
// merge 1:1 propertyid date using "$PROJECT_PATH\Output\TEMP\01_01_rental_time_panel.dta"
// * this data will have entries of the rentals when a comment was placed on a day it was blocked.
// sort propertyid date
// by propertyid : carryforward num_comments , replace
// by propertyid : carryforward corpus , replace
//
//
// drop if date < td(01sep2014)
// replace num_comments = 0 if num_comments ==.
//
// drop _merge
//
// save "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data.dta", replace
//
// export delimited using "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data.csv", quote replace
//

// *===============================================================================
// clear all
// set more off
// clear all 
// set more off
// set maxvar 120000, perm
// set matsize 11000, perm
// capture timer clear
// *-------------------------------------------------------------------------------
// do "00_00_Preamble.do"
// *-------------------------------------------------------------------------------
// use "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data.dta", clear
// sum date
// return list
// di 
//
// // forvalues i = `r(min)'(1)`r(max)' { 
// forvalues i = 20867(1)20909 { 
// preserve
// keep if date == `i'
// save "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data_`i'.dta", replace
// export delimited using "$PROJECT_PATH\Output\TEMP\02_01_Reviews_data_`i'.csv", quote replace
// restore
// }
