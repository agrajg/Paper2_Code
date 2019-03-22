Y:
cd "Y:\agrajg\Research\Paper2\Code"

REM "C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do "Y:\agrajg\Research\Paper2\Code\00_00_Cleaning_and_Preparing_Reviews_Data.do"
REM "C:\Program Files\R\R-3.5.2\bin\R.exe" CMD BATCH "Y:\agrajg\Research\Paper2\Code\00_00_Preamble.R"


"C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do "Y:\agrajg\Research\Paper2\Code\01_01_Exporting_data_to_csv.do" 
"C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do "Y:\agrajg\Research\Paper2\Code\02_01_Cleaning_and_Preparing_Reviews_Data.do"

pause