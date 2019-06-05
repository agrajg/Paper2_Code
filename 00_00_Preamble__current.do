* Preamble for Stata
* Setting project path
global PROJECT_PATH = subinstr("`c(pwd)'", "\Code", "", 1)
di "R Project path set to: $PROJECT_PATH" 
