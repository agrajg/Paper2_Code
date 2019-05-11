# Preamble for R.
# Setting project path
# input these correctly.
project.path = "Y:/agrajg/Research/Paper2/"
library.path  = c("C:/Users/agrajg/Documents/R/win-library/3.5")
# library.path = paste(project.path ,"Rlib", sep = "", collapse = NULL)
# library.path = c("Z:/agrajg/Documents/R/win-library/3.5", "H:/agrajg/Documents/R/win-library/3.5", "U:/agrajg/Documents/R/win-library/3.5")
# library.path = c("H:/agrajg/R","Z:/agrajg/R","U:/agrajg/R")
# library.path = c("H:/agrajg/R1","Z:/agrajg/R1","U:/agrajg/R1")
# library.path = c("H:/agrajg/R2","Z:/agrajg/R2","U:/agrajg/R2")
# library.path = c("H:/agrajg/R3","Z:/agrajg/R3","U:/agrajg/R3")


cat('* Setting Library Path ... ')
.libPaths(new = library.path)
print(paste("R Project path set to: ", project.path, sep = " "))
print(paste("R Library path set to: ", library.path, sep = " "))

if(!dir.exists(paste(project.path, "Output/", sep=""))){
  dir.create(paste(project.path, "Output/", sep=""))
}

if(!dir.exists(paste(project.path, "Output/TEMP/", sep=""))){
  dir.create(paste(project.path, "Output/TEMP/", sep=""))
}

if(!dir.exists(paste(project.path, "Output/Final/", sep=""))){
  dir.create(paste(project.path, "Output/Final/", sep=""))
}

if(!dir.exists(paste(project.path, "Output/TEMP/ForH2O/", sep=""))){
  dir.create(paste(project.path, "Output/TEMP/ForH2O/", sep=""))
}


# {
#   cat('* Install Fresh H2O ... ')
#   if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#   if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#   pkgs <- c("RCurl","jsonlite")
#   for (pkg in pkgs) {
#     if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
#   }
#   rm(pkgs)
#   install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")), dependencies = TRUE)
# }


# Check package function 
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

