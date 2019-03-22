clear all
set more off
clear all 
set more off
set maxvar 120000, perm
set matsize 11000, perm
capture timer clear
*===============================================================================
import delimited "Y:\agrajg\Research\Paper2\Output\TEMP\01_01_Reviews_data.csv", delimiter(comma) bindquote(strict) varnames(1)
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
export delimited using "Y:\agrajg\Research\Paper2\Output\TEMP\02_01_Reviews_data_plain.csv", quote replace


capture drop corpus
by propertyid : gen corpus = comments if _n==1
by propertyid : replace corpus = corpus[_n-1] + " " + comments if _n > 1
// by propertyid date : replace corpus = comments + " " + corpus[_n-1] if _n > 1

collapse (last) corpus , by (propertyid date )
count
format %200s corpus
keep if date >=td(01sep2014) & date <=td(31mar2017) 

* Removing spaces , special characters , etc.
replace corpus = stritrim(corpus)

* Emoticon from https://en.wikipedia.org/wiki/List_of_emoticons

// remove special characters with space or interpretation

export delimited using "Y:\agrajg\Research\Paper2\Output\TEMP\02_01_Reviews_data.csv", quote replace
*===============================================================================
*===============================================================================
