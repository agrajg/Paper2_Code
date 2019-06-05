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
use  "$PROJECT_PATH\Input\00_00_MCOX_property_data_clean_final.dta",  clear
// use  "$PROJECT_PATH\Input\00_00_rental_characteristics.dta",  clear

sort propertyid scraped_date
foreach var in name-host_listing_count {
	by propertyid: carryforward `var', replace
}

gsort propertyid -scraped_date
foreach var in name-host_listing_count {
	by propertyid: carryforward `var', replace
}

sort propertyid scraped_date
collapse (last)name-host_listing_count, by (propertyid)

format %20s name
foreach var in thumbnail_url medium_url picture_url xl_picture_url host_url host_thumbnail_url host_picture_url {
	replace `var' = strtrim(`var')
	replace `var' = stritrim(`var')
	gen `var'_dum = (`var' != "")
	drop `var'
}

replace host_response_rate  = subinstr(host_response_rate, "%","",10)
destring host_response_rate , force replace
replace host_response_rate = 0 if host_response_rate == .

replace host_acceptance_rate  = subinstr(host_acceptance_rate, "%","",10)
destring host_acceptance_rate , force replace
replace host_acceptance_rate = 0 if host_acceptance_rate == .

drop host_neighbourhood
drop host_name
drop host_since
drop host_location
drop host_id

destring host_listings_count , force replace
replace host_listings_count = 1 if host_listings_count ==. 
drop host_total_listings_count

replace host_verification = strtrim(host_verification)
replace host_verification = stritrim(host_verification)
gen host_verifications_email = (strpos(host_verifications, "email") > 0)
gen host_verifications_phone = (strpos(host_verifications, "phone") > 0)
gen host_verifications_facebook = (strpos(host_verifications, "facebook") > 0)
gen host_verifications_linkedin = (strpos(host_verifications, "linkedin") > 0)
gen host_verifications_reviews = (strpos(host_verifications, "reviews") > 0)
gen host_verifications_gid = (strpos(host_verifications, "government_id") > 0)
gen host_verifications_others = (strlen(host_verifications) > 0 & /// 
	(host_verifications_email !=1 | ///
	 host_verifications_phone !=1 | ///
	 host_verifications_facebook !=1 | ///
	 host_verifications_linkedin !=1 | ///
	 host_verifications_reviews !=1 | ///
	 host_verifications_gid !=1 ))
drop host_verifications

replace host_response_time = "N/A" if host_response_time == ""

rename require_guest_phone_verification rgpv
rename require_guest_profile_picture rgpp

foreach var in host_is_superhost host_has_profile_pic host_identity_verified requires_license rgpv rgpp instant_bookable {
	replace `var' = strtrim(`var')
	replace `var' = stritrim(`var')
	gen `var'_dum = (`var' =="t")
	drop `var'
}

drop street neighbourhood
rename neighbourhood_cleansed neighbourhood
drop neighbourhood_group_cleansed 	city	state	zipcode	zip	market	smart_location	country_code	country	latitude	longitude	is_location_exact	property_type	room_type	accommodates	bathrooms	bedrooms

drop beds

drop square_feet	price	weekly_price	monthly_price


foreach var in security_deposit cleaning_fee {
	replace `var'  = subinstr(`var', "$","",10)
	destring `var' , force replace
	replace `var' = 0 if `var' == .
}

foreach var in extra_people guests_included minimum_nights maximum_nights guests_included minimum_nights maximum_nights review_scores_rating review_scores_accuracy review_scores_cleanliness review_scores_checkin review_scores_communication review_scores_location review_scores_value{
	replace `var' = 0 if `var' == .
}

drop calendar_updated
drop has_availability
drop availability_*
drop calendar_last_scraped number_of_reviews first_review last_review

drop cancellation_policy 
drop instant_bookable 
drop license
drop jurisdiction_names
drop reviews_per_month

replace host_listings_count = 1 if host_listings_count == .

drop neighbourhood 
egen bed_amenities = concat(bed_type amenities), punct(" ")
drop bed_type amenities 

egen desc = concat(name summary space description experiences_offered neighborhood_overview notes transit access interaction house_rules host_about), punct(" ")
drop name summary space description experiences_offered neighborhood_overview notes transit access interaction house_rules host_about

sort propertyid
merge 1:1 propertyid using "$PROJECT_PATH\Input\00_00_rental_characteristics.dta"
drop if _merge == 1
drop _merge
merge 1:1 propertyid using "$PROJECT_PATH\Input\00_00_pid_key.dta"
keep if _merge == 3 
drop _merge
order pid propertyid
sort propertyid

drop calculated_host_listings_count
replace host_listing_count = 1 if host_listing_count ==.
drop host_listings_count
count

********************************************************************************
save "$PROJECT_PATH\Output\TEMP\03_01_Rental_Characteristics_Add_Desc.dta",  replace

*-------------------------------------------------------------------------------
* Encoding correctly
clear all
cd "$PROJECT_PATH\Output\TEMP\"
unicode analyze "03_01_Rental_Characteristics_Add_Desc.dta"
unicode encoding set "ASCII"
unicode retranslate "03_01_Rental_Characteristics_Add_Desc.dta", transutf8 invalid(ignore) replace
cd "$PROJECT_PATH\Code\"
*-------------------------------------------------------------------------------
use "$PROJECT_PATH\Output\TEMP\03_01_Rental_Characteristics_Add_Desc.dta", clear
count
drop if pid == .
save "$PROJECT_PATH\Output\TEMP\03_01_Rental_Characteristics_Add_Desc.dta",  replace
export delimited using "$PROJECT_PATH\Output\TEMP\03_01_Rental_Characteristics_Add_Desc.csv", quote replace
*===============================================================================
