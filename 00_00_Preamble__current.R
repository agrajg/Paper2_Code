# Preamble for R.
# Setting project path
# input these correctly.

cat('* SET PROJECT PATH ... ', '\n')
project.path = gsub(x = getwd(), pattern = "Code", replacement = "")
print(paste("R Project path set to: ", project.path, sep = " "))

# cat('* SET LIBRARY PATH ... ', '\n')
# {
#   library.path  = c("C:/Users/agrajg/Documents/R/win-library/3.5")
#   cat('* Setting Library Path ... ')
#   .libPaths(new = library.path)
#   print(paste("R Library path set to: ", library.path, sep = " ")) 
# }


cat('* Create new folders if non-exist' , '\n')
{
  if(!dir.exists(paste(project.path, "Output/", sep=""))){
    dir.create(paste(project.path, "Output/", sep=""))
  }
  
  if(!dir.exists(paste(project.path, "Output/TEMP/", sep=""))){
    dir.create(paste(project.path, "Output/TEMP/", sep=""))
  }
  
  if(!dir.exists(paste(project.path, "Output/Final/", sep=""))){
    dir.create(paste(project.path, "Output/Final/", sep=""))
  }
  
  if(!dir.exists(paste(project.path, "Output/TEMP/ForH2O_DML/", sep=""))){
    dir.create(paste(project.path, "Output/TEMP/ForH2O_DML/", sep=""))
  }
  
  if(!dir.exists(paste(project.path, "Output/TEMP/ForH2O_Rental_GLRM/", sep=""))){
    dir.create(paste(project.path, "Output/TEMP/ForH2O_Rental_GLRM/", sep=""))
  }
  
  if(!dir.exists(paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", sep=""))){
    dir.create(paste(project.path, "Output/TEMP/ForH2O_Reviews_GLRM/", sep=""))
  }
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


# # Check package function 
# check.packages <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE, repos = "https://cran.cnr.berkeley.edu/")
#   sapply(pkg, require, character.only = TRUE)
# }

