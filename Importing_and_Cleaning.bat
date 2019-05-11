set drive=Y:
set CodePath="Y:\agrajg\Research\Paper2\Code"

%drive%
cd %CodePath%

REM "C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do "Y:\agrajg\Research\Paper2\Code\00_00_Cleaning_and_Preparing_Reviews_Data.do"
REM "C:\Program Files\R\R-3.5.2\bin\R.exe" CMD BATCH "Y:\agrajg\Research\Paper2\Code\00_00_Preamble.R"

"C:\Program Files\R\R-3.5.2\bin\R.exe" CMD BATCH 		"00_00_Preamble.R"
"C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do 	"00_00_Preamble.do" 
REM "C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do 	"01_01_Exporting_data_to_csv.do" 

"C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do 	"02_01_Cleaning_and_Preparing_Reviews_Data.do"
"C:\Program Files\R\R-3.5.2\bin\R.exe" CMD BATCH 		"11_00_Saving_CSV_toRData.R"
"C:\Program Files\R\R-3.5.2\bin\R.exe" CMD BATCH 		"21_01_Format_Reviews_Data.R"



pause