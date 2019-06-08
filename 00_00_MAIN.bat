REM set drive=Y:
REM set CodePath="Y:\agrajg\Research\Paper2\Code"
REM %drive%
REM cd %CodePath%

REM "C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do "Y:\agrajg\Research\Paper2\Code\00_00_Cleaning_and_Preparing_Reviews_Data.do"
REM "C:\Program Files\R\R-3.5.2\bin\R.exe" CMD BATCH "Y:\agrajg\Research\Paper2\Code\00_00_Preamble.R"



REM "C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do 	"00_00_Preamble__current.do" 
REM "C:\Program Files (x86)\Stata15\StataMP-64.exe" /e do 	"00_00_Transfering_Files_From_Paper1__current.do"

"C:\Program Files\R\R-3.5.3\bin\R.exe" CMD BATCH 		"00_00_Preamble__current.R"
REM "C:\Program Files\R\R-3.5.3\bin\R.exe" CMD BATCH 		"01_01_Reading_CSV_and_Saving_Data__current.R"
"C:\Program Files\R\R-3.5.3\bin\R.exe" CMD BATCH 		"60_00_H2O_Standalone_MAIN.R"
"C:\Program Files\R\R-3.5.3\bin\R.exe" CMD BATCH 		"61_00_H2O_Standalone_MAIN.R"
"C:\Program Files\R\R-3.5.3\bin\R.exe" CMD BATCH 		"62_00_H2O_Standalone_MAIN.R"
"C:\Program Files\R\R-3.5.3\bin\R.exe" CMD BATCH 		"63_00_H2O_Standalone_MAIN.R"

pause
