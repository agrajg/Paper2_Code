# Preamble
# rm(list = ls())
# Setting project path
project.path = "Y:/agrajg/Research/Paper2/"
setwd(project.path)
print(paste("Project path set to: ", project.path, sep = " "))
# Setting Library Path
library.path = paste(project.path ,"Rlib", sep = "", collapse = NULL)
.libPaths(new = library.path)
print(paste("Library path set to: ", library.path, sep = " "))